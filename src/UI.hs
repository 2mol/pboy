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
import qualified Brick.Widgets.Edit         as E
import qualified Brick.Widgets.List         as L
import           Control.Monad              (void)
import           Data.Function              ((&))
import           Data.List.NonEmpty         (NonEmpty (..))
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Vector                as Vec
import           Fmt                        (fmt)
import           Fmt.Time                   (dateDashF)
import qualified Graphics.Vty               as V
import           Lens.Micro                 ((%~), (.~), (?~), (^.))
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
    { _config     :: Config.Config
    , _focusRing  :: F.FocusRing Name
    , _library    :: L.List Name Lib.FileInfo
    , _inbox      :: L.List Name Lib.FileInfo
    , _fileImport :: FileImport
    }


data FileImport = FileImport
    { _currentFile :: Maybe (Path Abs File)
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
    conf <- Config.getOrCreateConfig
    libraryFileInfos <- Lib.listFiles (conf ^. Config.libraryDir)
    inboxFileInfos <- Lib.listFiles (conf ^. Config.inboxDir)
    let
        libraryList = L.list Library (Vec.fromList libraryFileInfos) 1
        inboxList = L.list Inbox (Vec.fromList inboxFileInfos) 1
    pure
        $ State
        { _config = conf
        , _focusRing = initFocus
        , _library = libraryList
        , _inbox = inboxList
        , _fileImport = fileImportInit
        }


initFocus :: F.FocusRing Name
initFocus = F.focusRing [Inbox, Library]


fileImportInit :: FileImport
fileImportInit =
    FileImport
    { _currentFile = Nothing
    , _suggestions = L.list NameSuggestions (Vec.fromList []) 1
    , _nameEdit = E.editor FileNameEdit Nothing ""
    }


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

        importWidget =
            drawImportWidget s

        ui =
            case focus of
                Just NameSuggestions ->
                    [importWidget, mainScreen]

                Just FileNameEdit ->
                    [importWidget, mainScreen]

                _ ->
                    [mainScreen]
    in
        ui


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
                & fileImport .~ fileImportInit

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

        _ -> do
            newInbox <- L.handleListEvent e (s ^. inbox)
            continue (s & inbox .~ newInbox)


beginFileImport :: State -> Lib.FileInfo -> EventM Name (Next State)
beginFileImport s fileInfo = do
    let originalFile = Lib._fileName fileInfo
    fileNameSuggestions <- liftIO $ Lib.fileNameSuggestions originalFile

    let
        fileName :| sugg = fileNameSuggestions

        newFileNames =
            L.list NameSuggestions (Vec.fromList sugg) 1

        newState = s
            & focusRing .~ F.focusRing [FileNameEdit, NameSuggestions]
            & (fileImport . currentFile) ?~ originalFile
            & (fileImport . suggestions) .~ newFileNames
            & (fileImport . nameEdit) .~ E.editor FileNameEdit Nothing fileName

    handleImportScreenEvent newState (V.EvKey V.KDown [])


handleImportScreenEvent :: State -> V.Event -> EventM Name (Next State)
handleImportScreenEvent s e =
    let
        focus = F.focusGetCurrent (s ^. focusRing)
    in
    case (focus, e) of
        (_, V.EvKey (V.KChar 'o') [V.MCtrl]) ->
            do
                _ <- liftIO $
                    maybe
                        (pure (Left "failed to open preview"))
                        Lib.openFile (s ^. fileImport . currentFile)
                continue s

        (_, V.EvKey V.KEnter []) ->
            do
                let
                    conf = s ^. config

                    newFileName =
                        (s ^. fileImport . nameEdit)
                            & E.getEditContents
                            & T.unlines
                            & Lib.finalFileName

                _ <- liftIO $
                    maybe
                        (pure ())
                        (\cf -> Lib.fileFile conf cf newFileName)
                        (s ^. fileImport . currentFile)

                libraryFileInfos <- liftIO $ Lib.listFiles (conf ^. Config.libraryDir)
                inboxFileInfos <- liftIO $ Lib.listFiles (conf ^. Config.inboxDir)

                continue $ s
                    & focusRing .~ initFocus & fileImport .~ fileImportInit
                    & library .~ L.list Library (Vec.fromList libraryFileInfos) 1
                    & inbox .~ L.list Inbox (Vec.fromList inboxFileInfos) 1

        (Just NameSuggestions, _) ->
            do
                suggestionList <- L.handleListEvent e (s ^. fileImport . suggestions)

                let
                    newSuggestion =
                        case L.listSelectedElement suggestionList of
                            Just (_, t) ->
                                E.editor FileNameEdit Nothing t

                            _ -> s ^. fileImport . nameEdit

                newEdit <- E.handleEditorEvent (V.EvKey V.KDown []) newSuggestion

                continue $  s
                    & (fileImport . suggestions .~ suggestionList)
                    & (fileImport . nameEdit .~ newEdit)

        (Just FileNameEdit, _) ->
            continue =<< handleEventLensed s (fileImport . nameEdit) E.handleEditorEvent e

        _ -> continue s

--

drawFileInfo :: Bool -> Lib.FileInfo -> Widget Name
drawFileInfo _ fileInfo =
    let
        fileLabel =
            [ str (Path.fromRelFile $ Path.filename $ Lib._fileName fileInfo)
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
        $ B.borderWithLabel (str " Import ")
        $ padLeftRight 2 $ padTopBottom 1 $ hLimit 70 $ vLimit 20
        $ vBox
            [ str "new filename:"
            , B.hBorder
            , vLimit 1
                $ E.renderEditor
                    (str . T.unpack . T.unlines)
                    (F.focusGetCurrent (s ^. focusRing) == Just FileNameEdit)
                    (s ^. fileImport . nameEdit)
            , vLimit 1 (fill ' ')
            , str "suggestions:"
            , B.hBorder
            , vLimit 6 -- $ withAttr "suggestionList"
                $ L.renderList
                    (\_ t -> str (T.unpack t))
                    (F.focusGetCurrent (s ^. focusRing) == Just NameSuggestions)
                    (s ^. fileImport . suggestions)
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
