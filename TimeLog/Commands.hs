{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module TimeLog.Commands
    ( tlog
    ) where


import           Control.Applicative
import           Control.Error
import           Control.Lens
import           Control.Monad
import           Data.AffineSpace        ((.-.))
import qualified Data.Foldable           as F
import qualified Data.HashSet            as HS
import           Data.Maybe
import           Data.Monoid
import qualified Data.Sequence           as S
import qualified Data.Text               as T
import           Data.Thyme
import           Data.Thyme.Format.Human
import           System.Locale
import           TimeLog.Types
import           TimeLog.Utils


-- | This is the primary controller function.
tlog :: TLogCommand -> WorkList -> Script WorkList
tlog (On{..}) works = do
    unless (and $ works ^.. work . traverse . tlogEnd . to isJust) $
        left "You're done working on something. Finish your current task\
             \ before beginning anything new."
    start <- scriptIO $ maybe getCurrentTime return startTime
    right $ works & work %~ (S.|> TimeLog projectName start Nothing Nothing Nothing)

tlog (Fin{..}) works =
    onCurrent "You're not working on anything. You have to start things before you can finish them."
              (\current -> do
                    let start = fromMaybe (_tlogStart current) startTime
                    end <- scriptIO $ maybe getCurrentTime return endTime
                    right $ current { _tlogStart = start
                                    , _tlogEnd   = Just end
                                    })
              works

tlog Status works =
    -- Not sure if this is an improvement or not.
    lastCases (scriptIO $ putStrLn "You've never worked on anything.")
              (\current -> scriptIO $ do
                    now <- getCurrentTime
                    putStrLn $  "You started working on '"
                             <> T.unpack (_tlogName current)
                             <> "' "
                             <> humanRelTime (_tlogStart current) now
                             <> ".")
              (\done -> scriptIO $ do
                    end <- maybe getCurrentTime return $ _tlogEnd done
                    putStrLn $  "You last worked on '"
                             <> T.unpack (_tlogName done)
                             <> "' for "
                             <> humanTimeDiff (end .-. _tlogStart done)
                             <> ".")
              works

tlog (Tag{..}) works =
    onCurrent "No current task."
              (\current ->
                    let tags' = HS.fromList tags
                        cTags = maybe tags' (<> tags') $ _tlogTags current
                    in  right $ current { _tlogTags = Just cTags })
              works

tlog (Note{..}) works =
    onCurrent "No current task."
              (\current ->
                    let cNotes = maybe (S.singleton note) (S.|> note) $ _tlogNotes current
                    in  right $ current { _tlogNotes = Just cNotes })
              works

tlog Info works =
    lastCases
        (scriptIO $ putStrLn "No time logs.")
        (\current -> scriptIO $ printInfo True current =<< getCurrentTime)
        (\done    -> scriptIO . onJust (printInfo False done) $ _tlogEnd done)
        works
    where printInfo isCurrent TimeLog{..} time = do
            putStrLn . ("Name: " <>) $ T.unpack _tlogName
            onJust (putStrLn . ("Tags: " <>) . T.unpack . T.intercalate ", " . HS.toList)
                   _tlogTags
            when (isJust _tlogNotes) $ do
                putStrLn "Notes:"
                onJust (mapM_ (putStrLn . ("  " <>) . T.unpack) . F.toList) _tlogNotes
                putStrLn ""
            putStrLn $ "Start: " <> humanRelTime _tlogStart time
            unless isCurrent $
                maybe (return ()) (\e -> putStrLn $ "End: " <> humanRelTime e time) _tlogEnd
            putStrLn . ("Duration: " <>) . humanTimeDiff $ time .-. _tlogStart

tlog (Log{..}) works = do
    scriptIO . mapM_ (putStrLn . T.unpack . formatTimeLog)
             . F.toList
             . taker logN
             $ _work works
    return works
    where formatTimeLog :: TimeLog -> T.Text
          formatTimeLog (TimeLog{..}) =
              mconcat $
              catMaybes [ Just _tlogName, Just "\n"
                        , Just $ "  " <> ft _tlogStart
                        , Just " - "
                        , ft <$> _tlogEnd
                        , Just "\n"
                        , nl . ("  " <>) . T.intercalate ", " . F.toList <$> _tlogTags
                        , nl . T.intercalate "\n" . map ("    " <>) . F.toList <$> _tlogNotes
                        ]
          ft :: UTCTime -> T.Text
          ft = T.pack . formatTime defaultTimeLocale "%c"

          taker :: Maybe Int -> S.Seq TimeLog -> S.Seq TimeLog
          taker Nothing  = id
          taker (Just n) = S.take n

          nl :: T.Text -> T.Text
          nl = (<> "\n")


