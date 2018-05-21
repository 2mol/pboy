module UI where

import Control.Monad.IO.Class (liftIO)

import Brick
-- import qualified Brick.AttrMap              as A
import qualified Brick.Focus as F
-- import           Brick.Forms                (Form, (@@=))
-- import qualified Brick.Forms                as F
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
import qualified Brick.Widgets.Core         as BC
import qualified Brick.Widgets.Edit         as E
import qualified Brick.Widgets.List         as L
import           Data.Function              ((&))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Vector                as Vec
import           Fmt                        (fmt)
import           Fmt.Time                   (dateDashF)
import qualified Graphics.Vty               as V
import           Lens.Micro                 ((.~), (^.), (%~))
import           Lens.Micro.TH              (makeLenses)

import qualified Config
import qualified Lib

data State = State
    { _focusRing  :: F.FocusRing Name
    , _library    :: L.List Name Lib.FileInfo
    , _inbox      :: L.List Name Lib.FileInfo
    , _fileImport :: FileImport
    }

type Event = ()

data FileImport = FileImport
    { _suggestions :: L.List Name Text
    , _nameEdit    :: Text
    }

data Name
    = Inbox
    | Library
    | Import
    | FileNameEdit
    deriving (Eq, Ord, Show)

makeLenses ''FileImport

makeLenses ''State


initState :: IO State
initState = do
    config <- Lib.getDefaultConfig
    libraryFileInfos <- Lib.listFiles (config ^. Config.libraryDir)
    inboxFileInfos <- Lib.listFiles (config ^. Config.inboxDir)
    let
        libraryList = L.list Library (Vec.fromList libraryFileInfos) 1
        inboxList = L.list Inbox (Vec.fromList inboxFileInfos) 1
        fileImportInit = FileImport (L.list Import (Vec.fromList []) 1) ""
    pure
        $ State
        { _focusRing = initFocus
        , _library = libraryList
        , _inbox = inboxList
        , _fileImport = fileImportInit
        }

initFocus :: F.FocusRing Name
initFocus = F.focusRing [Inbox, Library]

app :: App State () Name
app = App
    { appDraw = drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handleEvent
    , appStartEvent = pure
    , appAttrMap = const theMap
    }

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (L.listAttr, V.brightWhite `on` V.black)
    , (L.listSelectedAttr, V.black `on` V.white)
    , (L.listSelectedFocusedAttr, V.black `on` V.brightWhite)
    , (E.editAttr, V.white `on` V.black)
    , (E.editFocusedAttr, V.black `on` V.yellow)
    ]

drawUI :: State -> [Widget Name]
drawUI s =
    let
        focus = F.focusGetCurrent (s ^. focusRing)

        inboxWidget =
            L.renderList drawFileInfo (focus == Just Inbox) (s ^. inbox)

        libraryWidget =
            L.renderList drawFileInfo (focus == Just Library) (s ^. library)

        mainScreen =
            withBorderStyle BS.unicodeRounded
                $ B.borderWithLabel (str "PAPERBOY")
                $ libraryWidget <=> B.hBorder <=> inboxWidget

        importWidget =
            drawImportWidget s

        ui =
            case focus of
                Just Import ->
                    [importWidget, mainScreen]
                _ ->
                    [mainScreen]
    in
        ui

handleEvent :: State -> BrickEvent Name Event -> EventM Name (Next State)
handleEvent s (VtyEvent e) =
    case e of
        V.EvKey (V.KChar 'c') [V.MCtrl] -> halt s

        V.EvKey (V.KChar '\t') [] ->
            continue $ s & focusRing %~ F.focusNext

        V.EvKey V.KEsc [] ->
            continue (s & focusRing .~ initFocus)

        _ ->
            let
                focus = F.focusGetCurrent (s ^. focusRing)
            in
            case focus of
                Just Inbox ->
                    handleInboxEvent s e

                Just Import ->
                    handleImportScreenEvent s e
                _ ->
                    continue s
handleEvent s _ = continue s

handleInboxEvent :: State -> V.Event -> EventM Name (Next State)
handleInboxEvent s e =
    let
        importAction =
            case L.listSelectedElement (s ^. inbox) of
                Just (_, fileInfo) -> beginFileImport s fileInfo
                _ -> continue s
    in
    case e of
        V.EvKey V.KEnter [] ->
            importAction

        V.EvKey (V.KChar ' ') [] ->
            importAction

        _ -> do
            newInbox <- L.handleListEvent e (s ^. inbox)
            continue (s & inbox .~ newInbox)

beginFileImport :: State -> Lib.FileInfo -> EventM Name (Next State)
beginFileImport s fileInfo = do
    config <- liftIO $ Lib.getDefaultConfig
    sugg <- liftIO $ Lib.fileNameSuggestions config (Lib._fileName fileInfo)

    let
        newFileNames =
            L.list Import (Vec.fromList sugg) 1

    continue $ s
        & focusRing .~ (F.focusRing [Import])
        & (fileImport . suggestions) .~ newFileNames

handleImportScreenEvent :: State -> V.Event -> EventM Name (Next State)
handleImportScreenEvent s e = do
    newFileNames <- L.handleListEvent e (s ^. fileImport ^. suggestions)
    continue (s & (fileImport . suggestions .~ newFileNames))

--

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

drawImportWidget :: State -> Widget Name
drawImportWidget s =
    C.centerLayer
        $ B.borderWithLabel (str "Import")
        $ padLeftRight 2 $ padTopBottom 1 $ hLimit 64 $ vLimit 16
        $ vBox
            [ str "suggestions:"
            , B.hBorder
            , L.renderList (\_ t -> str (T.unpack t)) (F.focusGetCurrent (s ^. focusRing) == Just Import) (s ^. fileImport ^. suggestions)
            ]
