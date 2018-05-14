module Main where

import           Brick
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
import qualified Brick.Widgets.List         as L
import           Control.Monad              (void)
import qualified Data.Vector                as Vec
import qualified Graphics.Vty               as V
import Lens.Micro ((^.))

import qualified Lib

main :: IO ()
main = do
    config <- Lib.getDefaultConfig
    inboxFiles <- Lib.listFiles (Lib.inboxDir config)
    let
        fileList = L.list Inbox (Vec.fromList inboxFiles) 1
        initState = State fileList
    void $ defaultMain app initState

-- the application state
data State = State
    { inboxFiles :: L.List Name FilePath
    }

data Event = Nope

-- view names:
data Name = Inbox | Library
    deriving (Eq, Ord, Show)

app :: App State Event Name
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
drawUI (State {inboxFiles}) =
    let
        inboxWidget =
            L.renderList listDrawElement True inboxFiles

        libraryWidget =
            C.center (str "Library here")

        screen =
            withBorderStyle BS.unicodeRounded
                $ B.borderWithLabel (str "PAPERBOY")
                $ libraryWidget <=> B.hBorder <=> inboxWidget
    in
        [screen]

handleEvent :: State -> BrickEvent Name Event -> EventM Name (Next State)
handleEvent s@(State {inboxFiles}) (VtyEvent e) =
    case e of
        V.EvKey V.KEsc [] -> halt s

        V.EvKey V.KEnter [] ->
            case inboxFiles ^. L.listSelectedL of
                Nothing -> continue s
                Just i -> continue $ State (L.listRemove i inboxFiles)

        ev -> do
                newInbox <- L.handleListEvent ev inboxFiles
                let newState = State newInbox
                continue newState
handleEvent s _ = continue s

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (L.listAttr,            V.brightWhite `on` V.black)
    , (L.listSelectedAttr,    V.black `on` V.brightWhite)
    -- , (customAttr,            fg V.cyan)
    ]
