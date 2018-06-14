module Config where

import           Control.Arrow     (left)
import           Data.Function     ((&))
import           Data.HashMap.Lazy ((!))
import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO
import           Lens.Micro.TH     (makeLenses)
import qualified System.Directory  as D
import           System.FilePath   ((</>))
import           Lens.Micro                 ((%~))
import qualified Text.Toml         as Toml
import           Text.Toml.Types   (Table, Node(..))

data Config = Config
    { _inboxDir     :: FilePath
    , _libraryDir   :: FilePath
    -- , _nameSuggestionsFileContent :: Bool
    -- , _nameSuggestionsMetaData    :: Bool
    , _importAction :: ImportAction
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
    -- , _nameSuggestionsFileContent = True
    -- , _nameSuggestionsMetaData = True
    , _importAction = Copy
    }


getDefaultConfig :: IO Config
getDefaultConfig = do
    homeDir <- D.getHomeDirectory
    pure $ Config.testDefaultConfig homeDir


getConfig :: IO (Maybe Config)
getConfig = undefined

readConfig :: IO (Maybe Config)
readConfig = do
    home <- D.getHomeDirectory
    configTxt <- TIO.readFile (home </> ".pboy.toml")
    let
        configResult =
            Toml.parseTomlDoc "" configTxt
                & left (T.pack . Toml.parseErrorPretty)

        config =
            case configResult of
                Left _ -> Nothing
                Right configMap ->
                    getConfigHelper configMap

    pure $ prependHome home <$> config

getConfigHelper :: Table -> Maybe Config
getConfigHelper configMap =
    case (configMap ! "inbox", configMap ! "library", configMap ! "move") of
        (VString inb, VString lib, VBoolean mov) ->
            Just (configHelper inb lib mov)
        _ -> Nothing

configHelper :: T.Text -> T.Text -> Bool -> Config
configHelper inb lib mov =
    let
        act =
            if mov
                then Move
                else Copy
    in
        Config
        { _inboxDir = T.unpack inb
        , _libraryDir = T.unpack lib
        , _importAction = act
        }

prependHome :: FilePath -> Config -> Config
prependHome home config =
    config & inboxDir %~ (home </>) & libraryDir %~ (home </>)
