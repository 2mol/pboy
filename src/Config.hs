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
    , defaultConfig
    , configPath
    ) where

import qualified Control.Exception as E
import Data.Function ((&))
import Data.Ini.Config.Bidir (Ini, IniSpec, (.=))
import qualified Data.Ini.Config.Bidir as C
import qualified Data.Text.IO as TIO
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Path (Abs, Dir, File, Path, Rel, (</>))
import qualified Path
import qualified Path.IO as Path


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

defaultConfig :: IO Config
defaultConfig = readConfigData defaultConfigData

createConfig :: IO ()
createConfig = do
    cpath <- configPath
    TIO.writeFile (Path.fromAbsFile cpath) configContent
    -- readConfigData defaultConfigData
    where
        configContent =
            C.serializeIni $ C.ini defaultConfigData configSpec

tryGetConfig :: IO (Either String Config)
tryGetConfig = do
    cpath <- configPath
    configTxtResult <-
        E.tryJust displayErr $ TIO.readFile (Path.fromAbsFile cpath)

    let
        configIniResult =
            configTxtResult >>= (\t -> C.parseIni t configIni)

        configResult =
            C.getIniValue <$> configIniResult

    sequence $ readConfigData <$> configResult

readConfigData :: ConfigData -> IO Config
readConfigData configData = do
    home <- Path.getHomeDir
    inbDir <- Path.resolveDir home (configData ^. inboxDirD)
    libDir <- Path.resolveDir home (configData ^. libraryDirD)

    let action = if (configData ^. importMove) then Move else Copy

    pure Config
        { _inboxDir = inbDir
        , _libraryDir = libDir
        , _importAction = action
        }

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

displayErr :: E.SomeException -> Maybe String
displayErr e =
    Just $ E.displayException e

configFile :: Path Rel File
configFile = $(Path.mkRelFile "pboy.ini")

configPath :: IO (Path Abs File)
configPath = do
    configHome <- Path.getXdgDir Path.XdgConfig Nothing
    pure $ configHome </> configFile
