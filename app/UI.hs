{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
module UI where

import Control.Monad.IO.Class (liftIO)

import           Brick
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
import qualified Brick.Widgets.Core         as BC
import qualified Brick.Widgets.List         as L
import           Data.Function              ((&))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Vector                as Vec
import           Fmt                        (fmt)
import           Fmt.Time                   (dateDashF)
import qualified Graphics.Vty               as V
import           Lens.Micro                 ((.~), (^.))
import           Lens.Micro.TH              (makeLenses)

import qualified Lib

data State = State
    { _focus     :: UIElement
    -- , _library :: L.List UIElement Lib.FileInfo
    , _inbox     :: L.List UIElement Lib.FileInfo
    , _fileNames :: L.List UIElement Text
    }

type Event = ()

-- data ImportScreen = ImportScreen
--     { _suggestions :: L.List UIElement Text
--     }

data UIElement
    = Inbox
    | Library
    | Import
    deriving (Eq, Ord, Show)

makeLenses ''State

initState :: IO State
initState = do
    config <- Lib.getDefaultConfig
    inboxFileInfos <- Lib.listFiles (Lib.inboxDir config)
    let
        fileList = L.list Inbox (Vec.fromList inboxFileInfos) 1
        suggestions = L.list Import (Vec.fromList []) 1
    pure $ State Inbox fileList suggestions

app :: App State () UIElement
app = App
    { appDraw = drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = pure
    , appAttrMap = const theMap
    }

drawFileInfo :: Bool -> Lib.FileInfo -> Widget UIElement
drawFileInfo selected fileInfo =
    let
        fileLabel =
            [ str (Lib._fileName fileInfo)
            , fill ' '
            , str (fmt (dateDashF $ Lib._modTime fileInfo))
            ]

        fileLabelWidget =
            BC.vLimit 1 $ BC.hBox fileLabel
    in
        if selected
            then withAttr L.listSelectedAttr fileLabelWidget
            else fileLabelWidget

drawUI :: State -> [Widget UIElement]
drawUI s =
    let
        inboxWidget =
            L.renderList drawFileInfo (s ^. focus == Inbox) (s ^. inbox)

        libraryWidget =
            C.center (str " ")

        mainScreen =
            withBorderStyle BS.unicodeRounded
                $ B.borderWithLabel (str "PAPERBOY")
                $ libraryWidget <=> B.hBorder <=> inboxWidget

        importWidget =
            C.centerLayer
                $ B.borderWithLabel (str "Import")
                $ padLeftRight 2 $ padTopBottom 1 $ hLimit 64 $ vLimit 16
                $ L.renderList (\_ t -> str (T.unpack t)) (s ^. focus == Import) (s ^. fileNames)

        ui =
            case s ^. focus of
                Import ->
                    [importWidget, mainScreen]
                _ ->
                    [mainScreen]
    in
        ui

handleEvent :: State -> BrickEvent UIElement Event -> EventM UIElement (Next State)
handleEvent s (VtyEvent e) =
    case (e, s ^. focus) of
        (V.EvKey (V.KChar 'q') [V.MCtrl], _) -> halt s
        (V.EvKey (V.KChar 'w') [V.MCtrl], _) -> halt s

        (V.EvKey V.KEsc [], _) ->
            continue (s & focus .~ Inbox)

        (V.EvKey V.KEnter [], Inbox) ->
            case L.listSelectedElement (s ^. inbox) of
                Nothing            -> continue s
                Just (_, fileInfo) -> handleFileSelect s fileInfo

        (ev, Inbox) -> do
                newInbox <- L.handleListEvent ev (s ^. inbox)
                continue (s & inbox .~ newInbox)

        (ev, Import) -> do
                newFileNames <- L.handleListEvent ev (s ^. fileNames)
                continue (s & fileNames .~ newFileNames)
        _ ->
            continue s
handleEvent s _ = continue s

handleFileSelect :: State -> Lib.FileInfo -> EventM UIElement (Next State)
handleFileSelect s fileInfo =
    do
        config <- liftIO $ Lib.getDefaultConfig
        suggestions <- liftIO $ Lib.fileNameSuggestions config (Lib._fileName fileInfo)

        let
            newImport =
                L.list Import (Vec.fromList suggestions) 1

        continue $ s
            & focus .~ Import
            & fileNames .~ newImport

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (L.listAttr,            V.brightWhite `on` V.black)
    , (L.listSelectedAttr,    V.black `on` V.brightWhite)
    -- , (customAttr,            fg V.cyan)
    ]
