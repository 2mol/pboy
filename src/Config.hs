{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Config
    ( Config(..)
    , homeDir
    , inboxDirs
    , libraryDir
    , importAction
    , ImportAction(..)
    , wordSeparator
    , tryGetConfig
    , defaultConfig
    , createConfig
    , getConfigPath
    ) where

import qualified Control.Exception as E
import           Data.Function ((&))
import           Data.Ini.Config.Bidir (Ini, IniSpec, (.=))
import qualified Data.Ini.Config.Bidir as C
import           Data.Text (Text)
import qualified Data.Text.IO as TIO
import           Lens.Micro ((.~), (^.))
import           Lens.Micro.TH (makeLenses)
import qualified System.Directory as Dir
import           System.FilePath ((</>))
import qualified System.FilePath as FilePath


data Config = Config
    { _homeDir      :: FilePath
    , _inboxDirs    :: [FilePath]
    , _libraryDir   :: FilePath
    , _importAction :: ImportAction
    , _wordSeparator:: Text
    }


data ImportAction
    = Move | Copy


data ConfigData = ConfigData
    { _inboxDirsD  :: [FilePath]
    , _libraryDirD :: FilePath
    , _importMove  :: Bool
    , _wordSeparatorD :: Text
    }


makeLenses ''Config
makeLenses ''ConfigData


defaultConfigData :: ConfigData
defaultConfigData =
    ConfigData
        { _inboxDirsD= ["Downloads"]
        , _libraryDirD = "papers"
        , _importMove = True
        , _wordSeparatorD = "_"
        }


defaultConfig :: IO Config
defaultConfig = readConfigData defaultConfigData


createConfig :: FilePath -> IO ()
createConfig configPath = do
    _ <- Dir.createDirectoryIfMissing True (FilePath.takeDirectory configPath)
    TIO.writeFile configPath configContent
    where
        configContent =
            C.serializeIni $ C.ini defaultConfigData configSpec


tryGetConfig :: FilePath -> IO (Either String Config)
tryGetConfig configPath = do
    configTxtResult <-
        E.tryJust displayErr $ TIO.readFile configPath

    let
        configIniResult =
            configTxtResult >>= (\t -> C.parseIni t configIni)

        configDataResult =
            C.getIniValue <$> configIniResult

        -- special case: allow " " in the .ini to be interpreted as
        -- a single space. Ugly but neceessary.
        inferSpace :: Config -> Config
        inferSpace conf =
            if conf ^. wordSeparator == "\" \""
                then conf & wordSeparator .~ " "
                else conf
    configResult <- sequence $ readConfigData <$> configDataResult
    pure $ inferSpace <$> configResult


readConfigData :: ConfigData -> IO Config
readConfigData configData = do
    homeDir' <- Dir.getHomeDirectory
    let inboxDirs' = map (homeDir' </>) (configData ^. inboxDirsD)
        libraryDir' = homeDir' </> (configData ^. libraryDirD)

    let importAction' = if configData ^. importMove then Move else Copy
    let wordSeparator' = configData ^. wordSeparatorD

    pure Config
        { _homeDir = homeDir'
        , _inboxDirs = inboxDirs'
        , _libraryDir = libraryDir'
        , _importAction = importAction'
        , _wordSeparator = wordSeparator'
        }


configSpec :: IniSpec ConfigData ()
configSpec =
    C.section "PAPERBOY" $ do
        inboxDirsD .=  C.field "inbox" (C.listWithSeparator "," C.string)
            & C.comment
                [ "The folder to watch for incoming files."
                , "Paths are relative to your home directory, but absolute paths are valid too."
                , "I will watch multiple folders if you give me a comma-separated list."
                ]

        libraryDirD .=  C.field "library" C.string
            & C.comment [ "The folder to copy/move renamed files to." ]

        importMove .=  C.field "move" C.bool
            & C.comment
                [ "Whether to move imported files."
                , "If set to false it will leave the original file unchanged."
                ]

        wordSeparatorD .=  C.field "wordseparator" C.text
            & C.comment
                [ "The character to insert between words."
                , "Write \" \" for space."
                ]


configIni :: Ini ConfigData
configIni = C.ini defaultConfigData configSpec


displayErr :: E.SomeException -> Maybe String
displayErr e =
    Just $ E.displayException e


getConfigPath :: IO FilePath
getConfigPath = do
    configHome <- Dir.getXdgDirectory Dir.XdgConfig ""
    pure $ configHome </> "pboy/pboy.ini"
