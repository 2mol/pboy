module UI where

import Control.Monad.IO.Class (liftIO)

import           Brick
-- import qualified Brick.AttrMap              as A
import qualified Brick.Forms                as F
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
import qualified Brick.Widgets.Core         as BC
import qualified Brick.Widgets.Edit         as E
import qualified Brick.Widgets.List         as L
import           Data.Function              ((&))
-- import           Data.Monoid
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
    { _focus      :: Name
    , _library    :: L.List Name Lib.FileInfo
    , _inbox      :: L.List Name Lib.FileInfo
    , _fileImport :: FileImport
    }

type Event = ()

type FileImport = L.List Name Text

data Name
    = Inbox
    | Library
    | Import
    deriving (Eq, Ord, Show)

makeLenses ''State

initState :: IO State
initState = do
    config <- Lib.getDefaultConfig
    libraryFileInfos <- Lib.listFiles (config ^. Lib.libraryDir)
    inboxFileInfos <- Lib.listFiles (config ^. Lib.inboxDir)
    let
        libraryList = L.list Library (Vec.fromList libraryFileInfos) 1
        inboxList = L.list Inbox (Vec.fromList inboxFileInfos) 1
        suggestions = L.list Import (Vec.fromList []) 1
    pure $ State Inbox libraryList inboxList suggestions

app :: App State () Name
app = App
    { appDraw = drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = pure
    , appAttrMap = const theMap
    }

drawFileInfo :: Bool -> Lib.FileInfo -> Widget Name
drawFileInfo _ fileInfo =
    let
        fileLabel =
            [ str (Lib._fileName fileInfo)
            , fill ' '
            , str (fmt (dateDashF $ Lib._modTime fileInfo))
            ]

        fileLabelWidget =
            BC.vLimit 1 $ BC.hBox fileLabel
    in
        fileLabelWidget
        -- if elementHasFocus
        --     then fileLabelWidget
        --     else withAttr unfocusedList fileLabelWidget

drawUI :: State -> [Widget Name]
drawUI s =
    let
        inboxWidget =
            L.renderList drawFileInfo (s ^. focus == Inbox) (s ^. inbox)

        libraryWidget =
            L.renderList drawFileInfo (s ^. focus == Library) (s ^. library)

        mainScreen =
            withBorderStyle BS.unicodeRounded
                $ B.borderWithLabel (str "PAPERBOY")
                $ libraryWidget <=> B.hBorder <=> inboxWidget

        importWidget =
            C.centerLayer
                $ B.borderWithLabel (str "Import")
                $ padLeftRight 2 $ padTopBottom 1 $ hLimit 64 $ vLimit 16
                $ L.renderList (\_ t -> str (T.unpack t)) (s ^. focus == Import) (s ^. fileImport)

        ui =
            case s ^. focus of
                Import ->
                    [importWidget, mainScreen]
                _ ->
                    [mainScreen]
    in
        ui

handleEvent :: State -> BrickEvent Name Event -> EventM Name (Next State)
handleEvent s (VtyEvent e) =
    case (e, s ^. focus) of
        (V.EvKey (V.KChar 'q') [V.MCtrl], _) -> halt s
        (V.EvKey (V.KChar 'c') [V.MCtrl], _) -> halt s
        (V.EvKey (V.KChar 'c') [], _) ->
            continue $ cycleFocus s
            -- let newFocus = cycleFocus (s ^. focus)
            -- in continue (s & focus .~ newFocus)

        -- (V.EvKey V.KBackTab [], foc) -> halt s

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
                newFileNames <- L.handleListEvent ev (s ^. fileImport)
                continue (s & fileImport .~ newFileNames)
        _ ->
            continue s
handleEvent s _ = continue s

handleFileSelect :: State -> Lib.FileInfo -> EventM Name (Next State)
handleFileSelect s fileInfo =
    do
        config <- liftIO $ Lib.getDefaultConfig
        suggestions <- liftIO $ Lib.fileNameSuggestions config (Lib._fileName fileInfo)

        let
            newFileNames =
                L.list Import (Vec.fromList suggestions) 1

        continue $ s
            & focus .~ Import
            & fileImport .~ newFileNames

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (L.listAttr, V.brightWhite `on` V.black)
    , (L.listSelectedAttr, V.black `on` V.white)
    , (L.listSelectedFocusedAttr, V.black `on` V.brightWhite)
    ]

cycleFocus :: State -> State
cycleFocus s =
    let
        newFocus =
            case s ^. focus of
                Inbox   -> Library
                Library -> Inbox
                _       -> s ^. focus
    in
        s & focus .~ newFocus
