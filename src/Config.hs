module Config where

import qualified Data.Text.IO     as TIO
import qualified Data.Text     as T
import           Lens.Micro.TH    (makeLenses)
import qualified System.Directory as D
import           System.FilePath  ((</>))
import qualified Text.Toml        as Toml
import           Data.Function           ((&))
import           Text.Toml.Types  (Table)
import Control.Arrow (left)

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

readConfig :: IO (Either T.Text Table)
readConfig = do
    home <- D.getHomeDirectory
    configTxt <- TIO.readFile (home </> ".pboy.toml")
    let
        configResult =
            Toml.parseTomlDoc "" configTxt
                & left (T.pack . Toml.parseErrorPretty)
    pure configResult
