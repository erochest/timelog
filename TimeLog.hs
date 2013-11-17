{-# LANGUAGE OverloadedStrings #-}


module Main where


import           Control.Error
import           Control.Lens
import           Data.Attoparsec.ByteString
import qualified Data.ByteString.Char8      as C8
import           Data.Maybe
import qualified Data.Text                  as T
import           Data.Thyme
import           Filesystem
import           Filesystem.Path.CurrentOS
import           Options.Applicative        hiding ((&))
import qualified Options.Applicative        as O
import           Prelude                    hiding (FilePath)
import           System.Locale
import           TimeLog.Commands
import           TimeLog.Types
import           TimeLog.Utils


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
            <> command "tag" (info (   Tag
                                   <$> some (O.argument (Just . T.pack)
                                                        (  metavar "TAG(S)"
                                                        <> help "Tags to add to the current task.")))
                                   (progDesc "Add tags to the current task."))
            <> command "note" (info (   Note
                                    <$> O.argument (Just . T.pack)
                                                   (  metavar "NOTE"
                                                   <> help "A note to add to the current task."))
                                    (progDesc "Add a note to the current task."))
            <> command "info" (info (pure Info)
                                    (progDesc "Print information about the current task."))
            <> command "log" (info (   Log
                                   <$> nullOption (  short 'n' <> metavar "N" <> value Nothing
                                                  <> reader (fmap Just . auto)
                                                  <> help "The number of items to print."))
                                   (progDesc "List the work log."))
            )

