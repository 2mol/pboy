module Main where

import           Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
import qualified Lib

main :: IO ()
main = simpleMain (inboxScreen ["Fafifo/text.txt"])

-- the application state
data State = State
    { inboxFiles :: [FilePath]
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
    , appStartEvent = return
    , appAttrMap = const theMap
    }

inboxScreen :: [FilePath] -> Widget Name
inboxScreen files =
    withBorderStyle BS.unicode $
    B.borderWithLabel (str "PAPERBOY")
        $ C.center (str "Welcome!")

drawUI :: State -> [Widget Name]
drawUI (State {inboxFiles}) = [inboxScreen inboxFiles]

handleEvent :: State -> BrickEvent Name Event -> EventM Name (Next State)
handleEvent = undefined

theMap :: AttrMap
theMap = undefined
