module Config
    ( Config(..)
    , inboxDir
    , libraryDir
    , importAction
    , ImportAction(..)
    , getOrCreateConfig
    , makeDefaultConfig
    ) where

import           Control.Arrow     (left)
import           Data.Function     ((&))
import           Data.HashMap.Lazy ((!))
import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO
import           Lens.Micro        ((%~))
import           Lens.Micro.TH     (makeLenses)
import qualified System.Directory  as D
import           System.FilePath   ((</>))
import qualified Text.Toml         as Toml
import           Text.Toml.Types   (Node (..), Table)
import Control.Exception


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


-- defaultConfig :: IO Config
-- defaultConfig = do
--     home <- D.getHomeDirectory
--     pure $
--         Config
--         { _inboxDir = home </> "Downloads"
--         , _libraryDir = home </> "pboy"
--         -- , _nameSuggestionsFileContent = True
--         -- , _nameSuggestionsMetaData = True
--         , _importAction = Copy
--         }

getOrCreateConfig :: IO Config
getOrCreateConfig = do
    mconfig <- getConfig
    case mconfig of
        Just config -> pure config
        Nothing -> do
            makeDefaultConfig
            getOrCreateConfig


getConfig :: IO (Maybe Config)
getConfig = do
    home <- D.getHomeDirectory
    configTxtResult <- tryJust displayErr (TIO.readFile (home </> ".pboy.toml"))
    case configTxtResult of
        Right configTxt -> do
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

        Left _ ->
            pure Nothing


displayErr :: SomeException -> Maybe String
displayErr e =
    Just $ displayException e


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

makeDefaultConfig :: IO ()
makeDefaultConfig = do
    home <- D.getHomeDirectory
    TIO.writeFile (home </> ".pboy.toml") configContent
    where
        configContent =
            T.unlines
            [ "inbox = \"Downloads\""
            , "library = \"pboy\""
            , "move = true"
            ]
