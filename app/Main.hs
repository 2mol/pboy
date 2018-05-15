module Main where

import           Brick
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
import qualified Brick.Widgets.List         as L
import           Control.Monad              (void)
import qualified Data.Vector                as Vec
import qualified Graphics.Vty               as V
import           Lens.Micro ((^.), (.~))

import qualified Lib

main :: IO ()
main = do
    config <- Lib.getDefaultConfig
    inboxList <- Lib.listFiles (Lib.inboxDir config)
    let
        fileList = L.list Inbox (Vec.fromList inboxList) 1
        initState = State fileList Nah
    void $ defaultMain app initState

-- the application state
data State = State
    { inboxList :: L.List Name FilePath
    , filenameSelect :: FilenameSelect
    }

data FilenameSelect = Nah

-- data Event = Nope

-- view names:
data Name = Inbox | Library
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
drawUI (State {inboxList}) =
    let
        inboxWidget =
            L.renderList listDrawElement True inboxList

        libraryWidget =
            C.center (str "Library here")

        screen =
            withBorderStyle BS.unicodeRounded
                $ B.borderWithLabel (str "PAPERBOY")
                $ libraryWidget <=> B.hBorder <=> inboxWidget
    in
        [screen]

handleEvent :: State -> BrickEvent Name () -> EventM Name (Next State)
handleEvent s@(State {inboxList}) (VtyEvent e) =
    case e of
        V.EvKey V.KEsc [] -> halt s

        V.EvKey V.KEnter [] ->
            case inboxList ^. L.listSelectedL of
                Nothing -> continue s
                Just i -> continue $ State (L.listRemove i inboxList) Nah

        ev -> do
                newInbox <- L.handleListEvent ev inboxList
                let newState = State newInbox Nah
                continue newState
handleEvent s _ =
    continue s

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (L.listAttr,            V.brightWhite `on` V.black)
    , (L.listSelectedAttr,    V.black `on` V.brightWhite)
    -- , (customAttr,            fg V.cyan)
    ]
