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
    ) where

import qualified Control.Exception as E
import           Data.Function ((&))
import qualified Data.Text.IO  as TIO
import           Lens.Micro.TH (makeLenses)
import           Path          (Abs, Dir, File, Path, Rel, (</>))
import qualified Path
import qualified Path.IO       as Path
import           Data.Ini.Config.Bidir   (Ini, IniSpec, (.=))
import qualified Data.Ini.Config.Bidir   as C
import           Lens.Micro              ((^.))


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


createConfig :: IO ()
createConfig = do
    configHome <- Path.getXdgDir Path.XdgConfig Nothing
    TIO.writeFile (Path.fromAbsFile (configHome </> configPath)) configContent
    -- readConfigData defaultConfigData
    where
        configContent =
            C.serializeIni $ C.ini defaultConfigData configSpec


tryGetConfig :: IO (Either String Config)
tryGetConfig = do
    configHome <- Path.getXdgDir Path.XdgConfig Nothing

    let configPathStr = Path.fromAbsFile $ configHome </> configPath

    configTxtResult <-
        E.tryJust displayErr (TIO.readFile configPathStr)

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


configPath :: Path Rel File
configPath = $(Path.mkRelFile "pboy.toml")
