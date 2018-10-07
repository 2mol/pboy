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

import           Control.Exception
import           Data.HashMap.Lazy ((!))
import qualified Data.Text         as T
import qualified Data.Text.IO      as TIO
import           Lens.Micro.TH     (makeLenses)
import qualified Text.Toml         as Toml
import           Path              (Path, Rel, Abs, Dir, File, (</>))
import qualified Path
import           Path.IO


data Config = Config
    { _inboxDir     :: Path Abs Dir
    , _libraryDir   :: Path Abs Dir
    , _importAction :: ImportAction
    } deriving Show

data ImportAction
    = Move | Copy
    deriving Show

makeLenses ''Config


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
    configHome <- getXdgDir XdgConfig Nothing
    configTxtResult <- tryJust displayErr (TIO.readFile (Path.fromAbsFile (configHome </> defaultConfigFile)))
    case configTxtResult of
        Right configTxt -> do
            let
                configResult =
                    Toml.parseTomlDoc "" configTxt
                        -- & left (T.pack . Toml.parseErrorPretty)

            case configResult of
                Left _ -> pure Nothing
                Right configMap ->
                    getConfigHelper configMap


        Left _ ->
            pure Nothing


displayErr :: SomeException -> Maybe String
displayErr e =
    Just $ displayException e


getConfigHelper :: Toml.Table -> IO (Maybe Config)
getConfigHelper configMap =
    case (configMap ! "inbox", configMap ! "library", configMap ! "move") of
        (Toml.VString inb, Toml.VString lib, Toml.VBoolean mov) ->
            Just <$> configHelper inb lib mov
        _ -> pure Nothing

configHelper :: T.Text -> T.Text -> Bool -> IO Config
configHelper inb lib mov = do
    let
        act =
            if mov
                then Move
                else Copy
    home <- getHomeDir
    inbd <- resolveDir home $ T.unpack inb
    libd <- resolveDir home $ T.unpack lib
    pure Config
        { _inboxDir = inbd
        , _libraryDir = libd
        , _importAction = act
        }

makeDefaultConfig :: IO ()
makeDefaultConfig = do
    configHome <- getXdgDir XdgConfig Nothing
    TIO.writeFile (Path.fromAbsFile (configHome </> defaultConfigFile)) configContent
    where
        configContent =
            T.unlines
            [ "inbox = \"Downloads\""
            , "library = \"pboy\""
            , "move = true"
            ]


-- TODO: clean this up and make absolute
defaultConfigFile :: Path Rel File
defaultConfigFile = $(Path.mkRelFile "pboy.toml")
