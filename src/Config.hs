{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Config
    ( Config(..)
    , inboxDir
    , libraryDir
    , importAction
    , ImportAction(..)
    , tryGetConfig
    , createConfig
    -- , makeDefaultConfig
    ) where

import qualified Control.Exception as E
-- import           Data.HashMap.Lazy ((!))
import           Data.Function ((&))
import qualified Data.Text     as T
import qualified Data.Text.IO  as TIO
import           Lens.Micro.TH (makeLenses)
import           Path          (Abs, Dir, File, Path, Rel, (</>))
import qualified Path
import qualified Path.IO       as Path
-- import Data.Ini.Config
import           Data.Ini.Config.Bidir (Ini, IniSpec, (.=))
import qualified Data.Ini.Config.Bidir as C

data Config = Config
    { _inboxDir     :: Path Abs Dir
    , _libraryDir   :: Path Abs Dir
    , _importAction :: ImportAction
    } deriving Show

data ImportAction
    = Move | Copy
    deriving Show


data ConfigData = ConfigData
    { _inboxDirD   :: FilePath
    , _libraryDirD :: FilePath
    , _importMove  :: Bool
    } deriving Show

makeLenses ''Config
makeLenses ''ConfigData

defaultConfigData :: ConfigData
defaultConfigData =
    ConfigData
        { _inboxDirD = "Downloads"
        , _libraryDirD = "papers"
        , _importMove = True
        }

createConfig :: IO Config
createConfig = undefined


tryGetConfig :: IO (Maybe Config)
tryGetConfig = do
    configHome <- Path.getXdgDir Path.XdgConfig Nothing

    let configPathStr = Path.fromAbsFile $ configHome </> defaultConfigFile

    configTxtResult <-
        E.tryJust displayErr (TIO.readFile configPathStr)

    case configTxtResult of
        Right configTxt -> do
            let
                configResult = C.parseIni configTxt configIni

            case configResult of
                Left _ -> pure Nothing
                Right config ->
                    Just <$> readConfigData (C.getIniValue config)


        Left _ ->
            pure Nothing

readConfigData :: ConfigData -> IO Config
readConfigData = undefined

configSpec :: IniSpec ConfigData ()
configSpec = do
    C.section "PAPERBOY" $ do
        inboxDirD .=  C.field "inbox" C.string
            & C.comment [
                "The folder to watch for incoming files.\n\
                \# All paths are relative to your home directory:"
                ]
            -- & C.optional

        libraryDirD .=  C.field "library" C.string
            & C.comment ["The folder to copy/move renamed files to:"]

        importMove .=  C.field "move" C.bool
            & C.comment [
                "Whether to move imported files.\n\
                \# If set to false it will leave the original file unchanged:"
                ]

configIni :: Ini ConfigData
configIni = C.ini defaultConfigData configSpec


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


displayErr :: E.SomeException -> Maybe String
displayErr e =
    Just $ E.displayException e


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

-- makeDefaultConfig :: IO ()
-- makeDefaultConfig = do
--     configHome <- Path.getXdgDir Path.XdgConfig Nothing
--     TIO.writeFile (Path.fromAbsFile (configHome </> defaultConfigFile)) configContent
--     where
--         configContent =
--             T.unlines
--             [ "inbox = \"Downloads\""
--             , "library = \"papers\""
--             , "move = true"
--             ]


defaultConfigFile :: Path Rel File
defaultConfigFile = $(Path.mkRelFile "pboy.toml")
