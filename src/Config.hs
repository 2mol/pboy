module Config where

import Lens.Micro.TH (makeLenses)
import           System.FilePath         ((</>))

data Config = Config
    { _inboxDir                   :: FilePath
    , _libraryDir                 :: FilePath
    , _nameSuggestionsFileContent :: Bool
    , _nameSuggestionsMetaData    :: Bool
    , _importAction               :: ImportAction
    } deriving Show

data ImportAction
    = Move | Copy
    deriving Show

makeLenses ''Config

testDefaultConfig :: FilePath -> Config
testDefaultConfig home =
    Config
    { _inboxDir = home </> "Downloads"
    , _libraryDir = home </> "pboy-test"
    , _nameSuggestionsFileContent = True
    , _nameSuggestionsMetaData = True
    , _importAction = Copy
    }
