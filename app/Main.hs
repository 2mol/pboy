module Main where

import           Brick
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
import qualified Brick.Widgets.Core         as BC
import qualified Brick.Widgets.List         as L
import           Control.Monad              (void)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Vector                as Vec
import           Fmt                        (fmt)
import           Fmt.Time                   (dateDashF)
import qualified Graphics.Vty               as V
-- import           Lens.Micro                 ((^.))

import qualified Lib

main :: IO ()
main = do
    config <- Lib.getDefaultConfig
    inboxFileInfos <- Lib.listFiles (Lib.inboxDir config)
    let
        fileList = L.list Inbox (Vec.fromList inboxFileInfos) 1
        suggestions = L.list Import (Vec.fromList []) 1
        initState = State Inbox fileList suggestions

    void $ defaultMain app initState

data State = State
    { _focus   :: Name
    -- , _library :: L.List Name Lib.FileInfo
    , _inbox   :: L.List Name Lib.FileInfo
    , _import  :: L.List Name Text
    }

type Event = ()

-- data ImportScreen = ImportScreen
--     { _suggestions :: L.List Name Text
--     }

data Name
    = Inbox
    | Library
    | Import
    deriving (Eq, Ord, Show)

app :: App State () Name
app = App
    { appDraw = drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = pure
    , appAttrMap = const theMap
    }

drawFileInfo :: Bool -> Lib.FileInfo -> Widget Name
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

drawUI :: State -> [Widget Name]
drawUI (State {_focus, _inbox, _import}) =
    let
        inboxWidget =
            L.renderList drawFileInfo (_focus == Inbox) _inbox

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
                $ L.renderList (\_ s -> str (T.unpack s)) (_focus == Import) _import

        ui =
            case _focus of
                Import ->
                    [importWidget, mainScreen]
                _ ->
                    [mainScreen]
    in
        ui

handleEvent :: State -> BrickEvent Name Event -> EventM Name (Next State)
handleEvent s@(State {_focus, _inbox, _import}) (VtyEvent e) =
    case (e, _focus) of
        (V.EvKey (V.KChar 'q') [V.MCtrl], _) -> halt s
        (V.EvKey (V.KChar 'w') [V.MCtrl], _) -> halt s

        (V.EvKey V.KEsc [], _) ->
            continue $ State Inbox _inbox _import

        (V.EvKey V.KEnter [], Inbox) ->
            case L.listSelectedElement _inbox of
                Nothing            -> continue s
                Just (_, fileInfo) -> handleFileSelect s fileInfo

        (ev, Inbox) -> do
                newL <- L.handleListEvent ev _inbox
                let newState = State _focus newL _import
                continue newState

        (ev, Import) -> do
                newL <- L.handleListEvent ev _import
                let newState = State _focus _inbox newL
                continue newState
        _ ->
            continue s
handleEvent s _ = continue s

handleFileSelect :: State -> Lib.FileInfo -> EventM Name (Next State)
handleFileSelect State{_focus, _inbox, _import} fileInfo =
    do
        config <- liftIO $ Lib.getDefaultConfig
        nameSuggestions <- liftIO $ Lib.fileNameSuggestions config (Lib._fileName fileInfo)

        let
            newImport =
                L.list Import (Vec.fromList nameSuggestions) 1

        continue $ State Import _inbox newImport

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (L.listAttr,            V.brightWhite `on` V.black)
    , (L.listSelectedAttr,    V.black `on` V.brightWhite)
    -- , (customAttr,            fg V.cyan)
    ]
