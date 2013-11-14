{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module Main where


import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Attoparsec.ByteString
import qualified Data.ByteString.Char8      as C8
import qualified Data.Char                  as C
import qualified Data.Text                  as T
import           Data.Thyme
import           Data.Thyme.LocalTime
import           Options.Applicative
import qualified Options.Applicative        as O
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


main :: IO ()
main = do
    tz <- getCurrentTimeZone
    execParser (opts tz) >>= print
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
data TLogCommand
        = On { projectName :: T.Text
             , startTime   :: Maybe UTCTime
             }
        deriving (Show)

