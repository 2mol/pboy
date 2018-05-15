module Main where

import           Brick
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
import qualified Brick.Widgets.List         as L
import           Control.Monad              (void)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Vector                as Vec
import qualified Graphics.Vty               as V
-- import           Lens.Micro                 ((^.))
import Control.Monad.IO.Class (liftIO)

import qualified Lib

main :: IO ()
main = do
    config <- Lib.getDefaultConfig
    inboxList <- Lib.listFiles (Lib.inboxDir config)
    let
        fileList = L.list Inbox (Vec.fromList inboxList) 1
        suggestions = L.list Import (Vec.fromList []) 1
        initState = State Inbox fileList suggestions

    void $ defaultMain app initState

-- the application state
data State = State
    { _focus     :: Name
    , _inboxList :: L.List Name FilePath
    , _import    :: L.List Name Text
    }

type Event = ()

-- data ImportScreen = ImportScreen
--     { _suggestions :: L.List Name Text
--     }

-- view names:
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


listDrawElement :: Bool -> FilePath -> Widget Name
listDrawElement sel f =
    let
        selStr s =
            if sel
                then withAttr L.listSelectedAttr (str s)
                else str s
    in selStr f

drawUI :: State -> [Widget Name]
drawUI (State {_focus, _inboxList, _import}) =
    let
        inboxWidget =
            L.renderList listDrawElement (_focus == Inbox) _inboxList

        libraryWidget =
            C.center (str " ")

        mainScreen =
            withBorderStyle BS.unicodeRounded
                $ B.borderWithLabel (str "PAPERBOY")
                $ libraryWidget <=> B.hBorder <=> inboxWidget

        importWidget =
            C.centerLayer
                $ B.borderWithLabel (str "Import - Suggested File Names")
                $ padAll 1 $ hLimit 64 $ vLimit 16
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
handleEvent s@(State {_focus, _inboxList, _import}) (VtyEvent e) =
    case (e, _focus) of
        (V.EvKey (V.KChar 'q') [V.MCtrl], _) -> halt s
        (V.EvKey (V.KChar 'w') [V.MCtrl], _) -> halt s

        (V.EvKey V.KEsc [], _) ->
            continue $ State Inbox _inboxList _import

        (V.EvKey V.KEnter [], Inbox) ->
            case L.listSelectedElement _inboxList of
                Nothing            -> continue s
                Just (_, fileName) -> handleFileSelect s fileName

        (ev, Inbox) -> do
                newL <- L.handleListEvent ev _inboxList
                let newState = State _focus newL _import
                continue newState

        (ev, Import) -> do
                newL <- L.handleListEvent ev _import
                let newState = State _focus _inboxList newL
                continue newState
        _ ->
            continue s
handleEvent s _ = continue s

handleFileSelect :: State -> FilePath -> EventM Name (Next State)
handleFileSelect State{_focus, _inboxList, _import} fileName =
    do
        config <- liftIO $ Lib.getDefaultConfig
        nameSuggestions <- liftIO $ Lib.fileNameSuggestions config fileName

        let
            newImport =
                L.list Import (Vec.fromList nameSuggestions) 1

        continue $ State Import _inboxList newImport

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (L.listAttr,            V.brightWhite `on` V.black)
    , (L.listSelectedAttr,    V.black `on` V.brightWhite)
    -- , (customAttr,            fg V.cyan)
    ]
