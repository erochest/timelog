{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}


module Main where


import           Control.Error
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.Aeson                 as A
import           Data.Aeson.Encode.Pretty
import           Data.Aeson.TH
import           Data.AffineSpace           ((.-.))
import           Data.Attoparsec.ByteString
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.Char                  as C
import qualified Data.Foldable              as F
import qualified Data.Sequence              as S
import qualified Data.Text                  as T
import           Data.Thyme
import           Data.Thyme.Format.Human
import           Data.Thyme.LocalTime
import qualified Data.Vector                as V
import           Filesystem
import qualified Filesystem                 as FS
import           Filesystem.Path.CurrentOS
import           Options.Applicative        hiding ((&))
import qualified Options.Applicative        as O
import           Prelude                    hiding (FilePath)
import           System.Exit
import           System.Locale


jsonTimeFormat :: String
jsonTimeFormat = "%FT%T%QZ"


instance ToJSON UTCTime where
    toJSON time = String . T.pack $ formatTime defaultTimeLocale jsonTimeFormat time

instance FromJSON UTCTime where
    parseJSON (String s) =
        maybe mzero return . parseTime defaultTimeLocale jsonTimeFormat $ T.unpack s
    parseJSON _          = mzero

instance ToJSON a => ToJSON (S.Seq a) where
    toJSON = Array . V.fromList . map toJSON . F.toList

data Hole = Hole

instance FromJSON a => FromJSON (S.Seq a) where
    parseJSON (Array v) = S.fromList . V.toList <$> V.mapM parseJSON v
    parseJSON _         = mzero


data TimeLog
        = TimeLog
        { _tlogName  :: T.Text
        , _tlogStart :: UTCTime
        , _tlogEnd   :: Maybe UTCTime
        , _tlogTags  :: Maybe [T.Text]
        , _tlogNotes :: Maybe [T.Text]
        } deriving (Show)
$(makeLenses ''TimeLog)
$(deriveJSON defaultOptions { fieldLabelModifier = map C.toLower . drop 5 }
             ''TimeLog)

data WorkList = Work { _work :: S.Seq TimeLog }
              deriving (Show)
$(makeLenses ''WorkList)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 1 } ''WorkList)

readWorkList :: FilePath -> Script WorkList
readWorkList filename =   ensureFile filename
                      >>  (scriptIO . BSL.readFile $ encodeString filename)
                      >>= hoistEither . eitherDecode'

writeWorkList :: FilePath -> WorkList -> Script ()
writeWorkList filepath =
        scriptIO . FS.writeFile filepath . BSL.toStrict . encodePretty' defConfig

ensureFile :: FilePath -> Script ()
ensureFile fileName = do
    exists <- scriptIO $ isFile fileName
    unless exists $
        writeWorkList fileName $ Work S.empty

withJsonLog :: FilePath -> (WorkList -> Script WorkList) -> Script ()
withJsonLog filename f = writeWorkList filename =<< f =<< readWorkList filename

-- | This is the primary controller function.
tlog :: TLogCommand -> WorkList -> Script WorkList
tlog (On{..}) works = do
    unless (and $ works ^.. work . traverse . tlogEnd . to isJust) $
        left "You're done working on something. Finish your current task\
             \ before beginning anything new."
    start <- scriptIO $ maybe getCurrentTime return startTime
    right $ works & work %~ (S.|> TimeLog projectName start Nothing Nothing Nothing)
tlog (Fin{..}) works =
    case S.viewr (_work works) of
        S.EmptyR -> left "You're not working on anything. You have to start things before you can finish them."
        s S.:> current -> do
            unless (isNothing $ _tlogEnd current) $
                left "You're not working on anything. You have to start things before you can finish them."
            let start = fromMaybe (_tlogStart current) startTime
            end <- scriptIO $ maybe getCurrentTime return endTime
            right . Work $ s S.|> current { _tlogStart = start
                                          , _tlogEnd   = Just end
                                          }
tlog Status works = do
    -- Surely this can be cleaned up some. Nasty, nasty, nasty.
    scriptIO $ case S.viewr (_work works) of
        S.EmptyR -> putStrLn "You've never worked on anything."
        _ S.:> current ->
            case _tlogEnd current of
                Just end -> putStrLn $  "You last worked on '"
                                    <> T.unpack (_tlogName current)
                                    <> "' for "
                                    <> humanTimeDiff (end .-. _tlogStart current)
                                    <> "."
                Nothing -> do
                    now <- getCurrentTime
                    putStrLn $  "You started working on '"
                             <> T.unpack (_tlogName current)
                             <> "' "
                             <> humanRelTime (_tlogStart current) now
                             <> "."
    return works


main :: IO ()
main = do
    tz   <- getCurrentTimeZone
    home <- getHomeDirectory
    cfg  <- execParser (opts tz)
    let jsonFile = fromMaybe (home </> ".ti-sheet") $ tlogJsonFile cfg
    runScript $ withJsonLog jsonFile . tlog $ tlogCommand cfg
    where opts tz = info (helper <*> tlogConfig tz)
                         (  fullDesc
                         <> progDesc "A simple time-tracking app."
                         <> header  "timelog - track your time")

-- | Parses a time specification (currently just ISO-8601) from a local time
-- (without the timezone) into a UTC time.
timeReader :: TimeZone -> String -> Either String UTCTime
timeReader tz = fmap (snd . view (from zonedTime) . flip ZonedTime tz . buildTime)
              . parseOnly (timeParser defaultTimeLocale format)
              . C8.pack
    where format = iso8601DateFormat (Just "%H:%M")

-- | CLI configuration for the program.
tlogConfig :: TimeZone -> O.Parser TLogConfig
tlogConfig tz =
            TLogConfig
        <$> nullOption (  short 'j' <> long "json" <> metavar "JSON_STORAGE" <> value Nothing
                       <> reader (return . Just . decodeString)
                       <> help "The JSON file storing the time log.")
        <*> tlogCom tz

-- | Reading commands and configuration from the command line.
tlogCom :: TimeZone -> O.Parser TLogCommand
tlogCom tz =
        subparser
            (  command "on"  (info (   On
                                    <$> nullOption (  short 'p' <> long "project" <> metavar "PROJECT_NAME"
                                                   <> reader (return . T.pack)
                                                   <> help "The name of the project.")
                                    <*> nullOption (  short 's' <> long "start-time" <> metavar "TIME"
                                                   <> value Nothing
                                                   <> eitherReader (fmap Just <$> timeReader tz)
                                                   <> help "The starting time for the project (YYYY-MM-DDTHH:MM)."))
                                    (progDesc "Start on a task."))
            <> command "fin" (info (   Fin
                                    <$> nullOption (  short 's' <> long "start-time" <> metavar "TIME"
                                                   <> value Nothing
                                                   <> eitherReader (fmap Just <$> timeReader tz)
                                                   <> help "The starting time for the project (YYYY-MM-DDTHH:MM.")
                                    <*> nullOption (  short 'e' <> long "end-time" <> metavar "TIME"
                                                   <> value Nothing
                                                   <> eitherReader (fmap Just <$> timeReader tz)
                                                   <> help "Set the ending time for the project (YYYY-MM-DDTHH:MM."))
                                    (progDesc "Change the starting time for the task."))
            <> command "status" (info (pure Status)
                                      (progDesc "Report on the status."))
            )

-- | Data for command-line modes and configuration.
data TLogConfig
        = TLogConfig { tlogJsonFile :: Maybe FilePath
                     , tlogCommand  :: TLogCommand
                     }
        deriving (Show)

data TLogCommand
        = On  { projectName :: T.Text
              , startTime   :: Maybe UTCTime
              }
        | Fin { startTime :: Maybe UTCTime
              , endTime   :: Maybe UTCTime
              }
        | Status
        deriving (Show)

