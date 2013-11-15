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
import           Data.Attoparsec.ByteString
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.Char                  as C
import qualified Data.Text                  as T
import           Data.Thyme
import           Data.Thyme.LocalTime
import           Filesystem
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

-- TODO: Use a Data.Sequence (from containers) for this. Will need to define
-- {From,To}JSON for it.
data WorkList = Work { _work :: [TimeLog] }
              deriving (Show)
$(makeLenses ''WorkList)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 1 } ''WorkList)

withJsonLog :: FilePath -> (WorkList -> Script WorkList) -> Script ()
withJsonLog filename f =
        scriptIO (BSL.readFile filename')
    >>= hoistEither . eitherDecode'
    >>= f
    >>= scriptIO . BS.writeFile filename' . BSL.toStrict . encodePretty' defConfig
    where filename' = encodeString filename

-- | This is the primary controller function.
tlog :: TLogCommand -> WorkList -> Script WorkList
tlog (On{..}) works = do
    unless (and $ works ^.. work . traverse . tlogEnd . to isJust) $
        left "You're done working on something. Finish your current task\
             \ before beginning anything new."
    start <- scriptIO $ maybe getCurrentTime return startTime
    return $ works & work <>~ [TimeLog projectName start Nothing Nothing Nothing]


main :: IO ()
main = do
    tz   <- getCurrentTimeZone
    home <- getHomeDirectory
    cfg  <- execParser (opts tz)
    runScript $ withJsonLog (home </> ".ti-sheet") $ tlog cfg
    where opts tz = info (helper <*> tlogCommand tz)
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

-- | Reading commands and configuration from the command line.
tlogCommand :: TimeZone -> O.Parser TLogCommand
tlogCommand tz =
            On
        <$> nullOption (  short 'p' <> long "project" <> metavar "PROJECT_NAME"
                       <> reader (return . T.pack)
                       <> help "The name of the project.")
        <*> nullOption (  short 's' <> long "start-time" <> metavar "TIME"
                       <> value Nothing
                       <> eitherReader (fmap Just <$> timeReader tz)
                       <> help "The starting time for the project (YYYY-MM-DDTHH:MM).")

-- | Data for command-line modes and configuration.
-- TODO: Configuration (json file, at least).
data TLogCommand
        = On { projectName :: T.Text
             , startTime   :: Maybe UTCTime
             }
        deriving (Show)

