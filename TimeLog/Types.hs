{-# LANGUAGE TemplateHaskell #-}

module TimeLog.Types
    ( TimeLog(..)
    , tlogName
    , tlogStart
    , tlogEnd
    , tlogTags
    , tlogNotes
    , WorkList(..)
    , work
    , TLogConfig(..)
    , TLogCommand(..)
    ) where


import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.Char                 as C
import qualified Data.Foldable             as F
import qualified Data.HashSet              as HS
import qualified Data.Sequence             as S
import qualified Data.Text                 as T
import           Data.Thyme
import qualified Data.Vector               as V
import           Filesystem.Path.CurrentOS
import           Prelude                   hiding (FilePath)
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

instance FromJSON a => FromJSON (S.Seq a) where
    parseJSON (Array v) = S.fromList . V.toList <$> V.mapM parseJSON v
    parseJSON _         = mzero


data TimeLog
        = TimeLog
        { _tlogName  :: T.Text
        , _tlogStart :: UTCTime
        , _tlogEnd   :: Maybe UTCTime
        , _tlogTags  :: Maybe (HS.HashSet T.Text)
        , _tlogNotes :: Maybe (S.Seq T.Text)
        } deriving (Show)
$(makeLenses ''TimeLog)
$(deriveJSON defaultOptions { fieldLabelModifier = map C.toLower . drop 5 }
             ''TimeLog)

data WorkList = Work { _work :: S.Seq TimeLog }
              deriving (Show)
$(makeLenses ''WorkList)
$(deriveJSON defaultOptions { fieldLabelModifier = drop 1 } ''WorkList)


-- | Data for command-line modes and configuration.
data TLogConfig
        = TLogConfig { tlogJsonFile :: Maybe FilePath
                     , tlogCommand  :: TLogCommand
                     }
        deriving (Show)

data TLogCommand
        = On   { projectName :: T.Text
               , startTime   :: Maybe UTCTime
               }
        | Fin  { startTime :: Maybe UTCTime
               , endTime   :: Maybe UTCTime
               }
        | Status
        | Tag  { tags :: [T.Text] }
        | Note { note :: T.Text }
        | Info
        | Log  { logN :: Maybe Int }
        deriving (Show)

