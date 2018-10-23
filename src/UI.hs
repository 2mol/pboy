{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module UI where

import Control.Monad.IO.Class (liftIO)

import           Brick
import qualified Brick.Focus                as F
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
import qualified Brick.Widgets.Core         as BC
import qualified Brick.Widgets.Dialog       as D
import qualified Brick.Widgets.Edit         as E
import qualified Brick.Widgets.List         as L
import           Control.Monad              (void)
import           Data.Function              ((&))
import           Data.List.NonEmpty         (NonEmpty (..))
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Time.Calendar         as Time
import qualified Data.Time.Clock            as Time
import qualified Data.Vector                as Vec
import qualified Graphics.Vty               as V
import           Lens.Micro                 ((%~), (.~), (^.))
import           Lens.Micro.TH              (makeLenses)
import           Path                       (Abs, File, Path)
import qualified Path

import qualified Config
import qualified Lib

import Data.Version (showVersion)
import Paths_pboy   (version)

pboyVersion :: String
pboyVersion = showVersion version

data State = State
    { _config    :: Config.Config
    , _focusRing :: F.FocusRing Name
    , _library   :: L.List Name Lib.FileInfo
    , _inbox     :: L.List Name Lib.FileInfo
    , _popup     :: Popup
    }

data Popup
    = NoPopup
    | FileImportPopup FileImport
    | HelpPopup
    | NoConfigPopup FilePath

data FileImport = FileImport
    { _currentFile :: Path Abs File
    , _suggestions :: L.List Name Text
    , _nameEdit    :: E.Editor Text Name
    }

data Name
    = Inbox
    | Library
    | NameSuggestions
    | FileNameEdit
    | HelpScreen
    deriving (Eq, Ord, Show)

makeLenses ''FileImport
makeLenses ''State

type Event = ()

main :: IO ()
main =
    void $ initState >>= defaultMain app

initState :: IO State
initState = do
    confResult <- Config.tryGetConfig

    case confResult of
        Right conf -> do
            libraryFileInfos <- Lib.listFiles (conf ^. Config.libraryDir)
            inboxFileInfos <- Lib.listFiles (conf ^. Config.inboxDir)
            let
                libraryList = L.list Library (Vec.fromList libraryFileInfos) 1
                inboxList = L.list Inbox (Vec.fromList inboxFileInfos) 1
            pure State
                { _config = conf
                , _focusRing = initFocus
                , _library = libraryList
                , _inbox = inboxList
                , _popup = NoPopup
                }

        Left _ -> do
            --TODO: if config doesn't exist yet, show a popup offering to create default
            -- Config.createConfig
            -- undefined

            defaultConfig <- Config.defaultConfig
            cpath <- Config.configPath

            pure State
                { _config = defaultConfig
                , _focusRing = initFocus
                , _library = L.list Library (Vec.fromList []) 1
                , _inbox = L.list Inbox (Vec.fromList []) 1
                , _popup = NoConfigPopup $ Path.fromAbsFile cpath
                }

initFocus :: F.FocusRing Name
initFocus = F.focusRing [Inbox, Library]

app :: App State Event Name
app = App
    { appDraw = drawUI
    , appChooseCursor = appCursor
    , appHandleEvent = handleEvent
    , appStartEvent = pure
    , appAttrMap = theMap
    }

theMap :: State -> AttrMap
theMap s =
    let
        selectColor =
            case F.focusGetCurrent (s ^. focusRing) of
                Just Library -> V.green
                _            -> V.yellow
    in
    attrMap V.defAttr
        [ (L.listAttr, V.brightWhite `on` V.black)
        , (L.listSelectedAttr, V.white `on` V.brightBlack)
        , (L.listSelectedFocusedAttr, V.black `on` selectColor)
        , (E.editAttr, V.brightWhite `on` V.blue)
        , (E.editFocusedAttr, V.black `on` V.yellow)
        -- , ("suggestionList", bg V.cyan)
        -- , ("fileNamePreview", V.brightWhite `on` V.green)
        , (D.dialogAttr, V.white `on` V.blue)
        , (D.buttonAttr, V.black `on` V.white)
        , (D.buttonSelectedAttr, bg V.yellow)
        ]

appCursor :: State -> [CursorLocation Name] -> Maybe (CursorLocation Name)
appCursor = F.focusRingCursor (^. focusRing)

drawUI :: State -> [Widget Name]
drawUI s =
    let
        focus = F.focusGetCurrent (s ^. focusRing)

        inboxWidget =
            L.renderList drawFileInfo (focus == Just Inbox) (s ^. inbox)

        libraryWidget =
            L.renderList drawFileInfo (focus == Just Library) (s ^. library)

        inboxLabel =
            "Inbox: " <> Path.fromAbsDir (s ^. config . Config.inboxDir)

        libraryLabel =
            "Library: " <> Path.fromAbsDir (s ^. config . Config.libraryDir)

        libraryAndInbox =
            withBorderStyle BS.unicodeRounded
                $ joinBorders . B.borderWithLabel (str " PAPERBOY ")
                $ vBox
                    [ libraryWidget
                    , B.hBorder
                    , inboxWidget
                    ]

        statusBar =
            vLimit 1 $ hBox
                [ str inboxLabel
                , fill ' '
                , str ( "v" ++ pboyVersion)
                , fill ' '
                , str libraryLabel
                ]

        mainScreen =
            libraryAndInbox <=> statusBar

        currentFocus = F.focusGetCurrent (s ^. focusRing)
    in
    case s ^. popup of
        FileImportPopup fi -> [drawImportWidget currentFocus fi, mainScreen]
        NoConfigPopup cp   -> [missingConfigScreen cp]
        HelpPopup          -> [helpScreen, mainScreen]
        _                  -> [mainScreen]

handleEvent :: State -> BrickEvent Name Event -> EventM Name (Next State)
handleEvent s (VtyEvent e) =
    let
        focus = F.focusGetCurrent (s ^. focusRing)
    in
    case (focus, e) of
        (_, V.EvKey (V.KChar 'c') [V.MCtrl])     -> halt s
        (Just Inbox,   V.EvKey V.KEsc [])        -> halt s
        (Just Library, V.EvKey V.KEsc [])        -> halt s
        (Just Inbox,   V.EvKey (V.KChar 'q') []) -> halt s
        (Just Library, V.EvKey (V.KChar 'q') []) -> halt s

        (_, V.EvKey V.KEsc []) ->
            continue $ s
                & focusRing .~ initFocus
                & popup .~ NoPopup

        (_, V.EvKey (V.KChar '\t') []) ->
            continue $ s & focusRing %~ F.focusNext

        _ ->
            case focus of
                Just Library ->
                    handleLibraryEvent s e

                Just Inbox ->
                    handleInboxEvent s e

                Just NameSuggestions ->
                    handleImportScreenEvent s e

                Just FileNameEdit ->
                    handleImportScreenEvent s e

                _ ->
                    continue s
handleEvent s _ = continue s

handleLibraryEvent :: State -> V.Event -> EventM Name (Next State)
handleLibraryEvent s e =
    let
        openFile fileName = do
            _ <- liftIO $ Lib.openFile fileName
            continue s

        openAction =
            case L.listSelectedElement (s ^. library) of
                Just (_, fileInfo) -> openFile (Lib._fileName fileInfo)
                _                  -> continue s

        renameAction =
            case L.listSelectedElement (s ^. library) of
                Just (_, fileInfo) -> beginFileImport s fileInfo
                _                  -> continue s
    in
    case e of
        V.EvKey V.KEnter [] ->
            openAction

        V.EvKey (V.KChar ' ') [] ->
            openAction

        V.EvKey (V.KChar 'r') [] ->
            renameAction

        _ -> do
            newLibrary <- L.handleListEvent e (s ^. library)
            continue (s & library .~ newLibrary)

handleInboxEvent :: State -> V.Event -> EventM Name (Next State)
handleInboxEvent s e =
    let
        importAction =
            case L.listSelectedElement (s ^. inbox) of
                Just (_, fileInfo) -> beginFileImport s fileInfo
                _                  -> continue s
    in
    case e of
        V.EvKey V.KEnter [] ->
            importAction

        V.EvKey (V.KChar ' ') [] ->
            importAction

        V.EvKey (V.KChar 'r') [] ->
            importAction

        _ -> do
            newInbox <- L.handleListEvent e (s ^. inbox)
            continue (s & inbox .~ newInbox)

beginFileImport :: State -> Lib.FileInfo -> EventM Name (Next State)
beginFileImport s fileInfo = do
    let originalFile = Lib._fileName fileInfo

    fileNameSuggestions <- liftIO $ Lib.fileNameSuggestions originalFile

    let
        fi =
            FileImport
            { _currentFile = originalFile
            , _suggestions = newFileNames
            , _nameEdit = E.editor FileNameEdit Nothing fileName
            }

        fileName :| sugg = fileNameSuggestions

        newFileNames =
            L.list NameSuggestions (Vec.fromList sugg) 1

        newState = s
            & focusRing .~ F.focusRing [FileNameEdit, NameSuggestions]
            & popup .~ FileImportPopup fi
            -- & (fileImport . currentFile) ?~ originalFile
            -- & (fileImport . suggestions) .~ newFileNames
            -- & (fileImport . nameEdit) .~ E.editor FileNameEdit Nothing fileName

    handleImportScreenEvent newState (V.EvKey V.KDown [])

handleImportScreenEvent :: State -> V.Event -> EventM Name (Next State)
handleImportScreenEvent s ev =
    let
        focus = F.focusGetCurrent (s ^. focusRing)
    in
    case (s ^. popup, focus, ev) of
        (FileImportPopup fi, _, V.EvKey (V.KChar 'o') [V.MCtrl]) ->
            do
                _ <- liftIO $ Lib.openFile (fi ^. currentFile)
                continue s

        (FileImportPopup fi, _, V.EvKey V.KEnter []) ->
            do
                let
                    conf = s ^. config

                    newFileName =
                        fi ^. nameEdit
                            & E.getEditContents
                            & T.unlines
                            & Lib.finalFileName

                _ <- liftIO $
                    (Lib.fileFile conf newFileName) (fi ^. currentFile)

                libraryFileInfos <- liftIO $ Lib.listFiles (conf ^. Config.libraryDir)
                inboxFileInfos <- liftIO $ Lib.listFiles (conf ^. Config.inboxDir)

                continue $ s
                    & focusRing .~ initFocus & popup .~ NoPopup
                    & library .~ L.list Library (Vec.fromList libraryFileInfos) 1
                    & inbox .~ L.list Inbox (Vec.fromList inboxFileInfos) 1

        (FileImportPopup fi, Just NameSuggestions, _) ->
            do
                suggestionList <-
                    L.handleListEvent ev (fi ^. suggestions)

                let
                    newSuggestion =
                        case L.listSelectedElement suggestionList of
                            Just (_, t) ->
                                E.editor FileNameEdit Nothing t

                            _ -> fi ^. nameEdit

                newEdit <- E.handleEditorEvent (V.EvKey V.KDown []) newSuggestion

                let
                    newFileImport = fi
                        & suggestions .~ suggestionList
                        & nameEdit .~ newEdit

                continue $ s & popup .~ FileImportPopup newFileImport

        (FileImportPopup fi, Just FileNameEdit, _) -> do
            newEdit <- E.handleEditorEvent ev (fi ^. nameEdit)
            let newFileImport = fi & nameEdit .~ newEdit
            continue $ s & popup .~ FileImportPopup newFileImport

        _ -> continue s

--

drawFileInfo :: Bool -> Lib.FileInfo -> Widget Name
drawFileInfo _ fileInfo =
    let
        fileLabel =
            [ str (Path.fromRelFile $ Path.filename $ Lib._fileName fileInfo)
            , fill ' '
            , str (Time.showGregorian . Time.utctDay $ Lib._modTime fileInfo)
            ]

        fileLabelWidget =
            BC.vLimit 1 $ BC.hBox fileLabel
    in
        fileLabelWidget


drawImportWidget :: Maybe Name -> FileImport -> Widget Name
drawImportWidget focus fileImportData =
    C.centerLayer
        $ B.borderWithLabel (str " Import ")
        $ padLeftRight 2 $ padTopBottom 1 $ hLimit 70 $ vLimit 20
        $ vBox
            [ str "new filename:"
            , B.hBorder
            , vLimit 1
                $ E.renderEditor
                    (str . T.unpack . T.unlines)
                    (focus == Just FileNameEdit)
                    (fileImportData ^. nameEdit)
            , vLimit 1 (fill ' ')
            , str "suggestions:"
            , B.hBorder
            , vLimit 6 -- $ withAttr "suggestionList"
                $ L.renderList
                    (\_ t -> str (T.unpack t))
                    (focus == Just NameSuggestions)
                    (fileImportData ^. suggestions)
            -- , B.hBorder
            -- , vLimit 1 (fill ' ')
            -- , str "filename preview:"
            -- , withAttr "fileNamePreview" $ str (fileNamePreview $ s ^. fileImport ^. nameEdit)
            , fill ' '
            , str
                "[Tab]    - switch between editor and suggestions.\n\
                \[Enter]  - rename the file and move it to your library folder.\n\
                \[Ctrl-o] - open the file that you're currently renaming."
            ]

data HelpChoice = HelpClose

helpScreen :: Widget Name
helpScreen =
    D.renderDialog d
        $ C.hCenter
        $ padAll 1
        $ str
            "This is the dialog body."
    where
        d = D.dialog (Just " Welcome to Paperboy ") (Just (0, choices)) 75

        choices = [("Close", HelpClose)]

data ConfigChoice = ConfigCreate | ConfigAbort

missingConfigScreen :: FilePath -> Widget Name
missingConfigScreen configPath =
    D.renderDialog d
        $ C.hCenter
        $ padAll 1
        $ str
            ("I will create a config at " <> configPath)
    where
        d = D.dialog (Just " Welcome to Paperboy ") (Just (0, choices)) 75

        choices = [("Create Config", ConfigCreate), ("Abort", ConfigAbort)]
