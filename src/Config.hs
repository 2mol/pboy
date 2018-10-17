{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Config
    ( Config(..)
    , inboxDir
    , libraryDir
    , importAction
    , ImportAction(..)
    , getOrCreateConfig
    , makeDefaultConfig
    ) where

-- import           Control.Exception
-- import           Data.HashMap.Lazy ((!))
import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO
import           Lens.Micro.TH     (makeLenses)
import           Path              (Abs, Dir, File, Path, Rel, (</>))
import qualified Path
import qualified Path.IO           as Path


data Config = Config
    { _inboxDir     :: Path Abs Dir
    , _libraryDir   :: Path Abs Dir
    , _importAction :: ImportAction
    } deriving Show

data ImportAction
    = Move | Copy
    deriving Show

makeLenses ''Config


defaultConfig :: IO Config
defaultConfig = do
    home <- Path.getHomeDir
    inbDir <- Path.resolveDir home $ "Downloads"
    libDir <- Path.resolveDir home $ "papers"
    pure Config
        { _inboxDir = inbDir
        , _libraryDir = libDir
        , _importAction = Move
        }

getOrCreateConfig :: IO Config
getOrCreateConfig = defaultConfig

-- getOrCreateConfig :: IO Config
-- getOrCreateConfig = do
--     mconfig <- getConfig
--     case mconfig of
--         Just config -> pure config
--         Nothing -> do
--             makeDefaultConfig
--             getOrCreateConfig


-- getConfig :: IO (Maybe Config)
-- getConfig = do undefined
    -- configHome <- Path.getXdgDir Path.XdgConfig Nothing

    -- let configPathStr = Path.fromAbsFile $ configHome </> defaultConfigFile

    -- configTxtResult <-
    --     tryJust displayErr (TIO.readFile configPathStr)

    -- case configTxtResult of
    --     Right configTxt -> do
    --         let
    --             configResult =
    --                 -- Toml.parseTomlDoc "" configTxt

    --         case configResult of
    --             Left _ -> pure Nothing
    --             Right configMap ->
    --                 getConfigHelper configMap


    --     Left _ ->
    --         pure Nothing


-- displayErr :: SomeException -> Maybe String
-- displayErr e =
--     Just $ displayException e


-- getConfigHelper :: Toml.Table -> IO (Maybe Config)
-- getConfigHelper configMap =
    -- case (configMap ! "inbox", configMap ! "library", configMap ! "move") of
    --     (Toml.VString inb, Toml.VString lib, Toml.VBoolean mov) ->
    --         Just <$> configHelper inb lib mov
    --     _ -> pure Nothing

-- configHelper :: T.Text -> T.Text -> Bool -> IO Config
-- configHelper inb lib mov = do
--     let
--         act =
--             if mov
--                 then Move
--                 else Copy
--     home <- Path.getHomeDir
--     inbDir <- Path.resolveDir home $ T.unpack inb
--     libDir <- Path.resolveDir home $ T.unpack lib
--     pure Config
--         { _inboxDir = inbDir
--         , _libraryDir = libDir
--         , _importAction = act
--         }

makeDefaultConfig :: IO ()
makeDefaultConfig = do
    configHome <- Path.getXdgDir Path.XdgConfig Nothing
    TIO.writeFile (Path.fromAbsFile (configHome </> defaultConfigFile)) configContent
    where
        configContent =
            T.unlines
            [ "inbox = \"Downloads\""
            , "library = \"papers\""
            , "move = true"
            ]


defaultConfigFile :: Path Rel File
defaultConfigFile = $(Path.mkRelFile "pboy.toml")
