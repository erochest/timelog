module TimeLog.Utils
    ( readWorkList
    , writeWorkList
    , ensureFile
    , withJsonLog
    , onCurrent
    , lastCases
    , onJust
    ) where


import           Control.Applicative
import           Control.Error
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.Sequence             as S
import           Filesystem
import qualified Filesystem                as FS
import           Filesystem.Path.CurrentOS
import           Prelude                   hiding (FilePath)
import           TimeLog.Types


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

onCurrent :: String -> (TimeLog -> Script TimeLog) -> WorkList -> Script WorkList
onCurrent errorMsg f works =
    case S.viewr (_work works) of
        S.EmptyR -> left errorMsg
        s S.:> current ->
            case _tlogEnd current of
                Just _  -> left errorMsg
                Nothing -> right . Work . (s S.|>) =<< f current

lastCases :: Script ()
          -> (TimeLog -> Script())
          -> (TimeLog -> Script ())
          -> WorkList
          -> Script WorkList
lastCases none current done works =
    case S.viewr (_work works) of
        S.EmptyR -> none
        _ S.:> task ->
            case _tlogEnd task of
                Nothing  -> current task
                Just end -> done task
    >> return works

onJust :: Applicative a => (x -> a ()) -> Maybe x -> a ()
onJust = maybe (pure ())

