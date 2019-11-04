{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module UI where

import           Control.Monad (join, void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Function ((&))
import           Data.List (intercalate)
import           Data.Monoid ((<>))

import           Brick
import qualified Brick.Focus as F
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Core as BC
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Time.Calendar as Time
import qualified Data.Time.Clock as Time
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import           Lens.Micro ((%~), (.~), (?~), (^.))
import           Lens.Micro.TH (makeLenses)
import           Path (Abs, File, Path)
import qualified Path

import qualified Config
import qualified Lib

import           Data.Version (showVersion)
import           Paths_pboy (version)


pboyVersion :: String
pboyVersion = showVersion version


data State = State
    { _config       :: Config.Config
    , _configPath   :: Path Abs File
    , _focusRing    :: F.FocusRing ResourceName
    , _firstStart   :: Maybe (D.Dialog ConfigChoice)
    , _help         :: Maybe (D.Dialog HelpChoice)
    , _library      :: L.List ResourceName Lib.FileInfo
    , _inbox        :: L.List ResourceName Lib.FileInfo
    , _fileImport   :: Maybe FileImport
    , _libSortOrder :: Lib.SortType
    }


data ConfigChoice = ConfigCreate | ConfigAbort


data HelpChoice = HelpClose


data FileImport = FileImport
    { _currentFile :: Path Abs File
    , _suggestions :: L.List ResourceName Text
    , _nameEdit    :: E.Editor Text ResourceName
    }


data ResourceName
    = FirstStart (Path Abs File)
    | Help
    | Library
    | Inbox
    | NameSuggestions
    | FileNameEdit
    deriving (Eq, Ord, Show)


makeLenses ''FileImport
makeLenses ''State


main :: IO ()
main =
    void $ initState >>= defaultMain app


initState :: IO State
initState = do
    cpath <- Config.getConfigPath
    confResult <- Config.tryGetConfig cpath

    case confResult of
        Right conf ->
            refreshFiles State
                { _config = conf
                , _configPath = cpath
                , _focusRing = inboxFocus
                , _firstStart = Nothing
                , _help = Nothing
                , _library = L.list Library [] 1
                , _inbox = L.list Inbox [] 1
                , _fileImport = Nothing
                , _libSortOrder = Lib.SortDate
                }

        Left _ -> do
            defaultConfig <- Config.defaultConfig

            pure State
                { _config = defaultConfig
                , _configPath = cpath
                , _firstStart = Just firstStartDialog
                , _help = Nothing
                , _focusRing = F.focusRing [FirstStart cpath]
                , _library = L.list Library [] 1
                , _inbox = L.list Inbox [] 1
                , _fileImport = Nothing
                , _libSortOrder = Lib.SortDate
                }


refreshFiles :: State -> IO State
refreshFiles s = do
    libraryFileInfos_ <- Lib.listFiles (s ^. config . Config.libraryDir)
    inboxFileInfos_ <- mapM Lib.listFiles (s ^. config . Config.inboxDirs)
    let
        libraryFileInfos = Lib.sortFileInfo (s ^. libSortOrder) libraryFileInfos_
        inboxFileInfos = Lib.sortFileInfoByDate $ join inboxFileInfos_
        libraryList = L.list Library (Vec.fromList libraryFileInfos) 1
        inboxList = L.list Inbox (Vec.fromList inboxFileInfos) 1
    pure $ s
        & library .~ libraryList
        & inbox .~ inboxList


cycleLibSort :: State -> IO State
cycleLibSort s = do
    let val = case (s ^. libSortOrder) of
                Lib.SortName -> Lib.SortDate
                Lib.SortDate -> Lib.SortName
    pure $ s & libSortOrder .~ val


inboxFocus :: F.FocusRing ResourceName
inboxFocus = F.focusRing [Inbox, Library]


type Event = ()


app :: App State Event ResourceName
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
        selectedBGColor =
            case F.focusGetCurrent (s ^. focusRing) of
                Just Library -> V.green
                Just Inbox   -> V.yellow
                _            -> V.brightWhite
    in
    attrMap V.defAttr
        [ (L.listAttr, V.white `on` V.black)
        , (L.listSelectedAttr, V.black `on` V.brightBlack)
        , (L.listSelectedFocusedAttr, V.black `on` selectedBGColor)
        , (E.editAttr, V.brightWhite `on` V.blue)
        , (E.editFocusedAttr, V.black `on` V.yellow)
        , (D.dialogAttr, V.brightWhite `on` V.blue)
        , (D.buttonAttr, V.black `on` V.white)
        , (D.buttonSelectedAttr, bg V.yellow)
        ]


appCursor :: State -> [CursorLocation ResourceName] -> Maybe (CursorLocation ResourceName)
appCursor = F.focusRingCursor (^. focusRing)


drawUI :: State -> [Widget ResourceName]
drawUI s =
    case (currentFocus, s ^. fileImport) of
        (Just (FirstStart cpath), _)    -> [missingConfigScreen cpath (s ^. firstStart)]
        (Just Help, _)                  -> [helpScreen (s ^. configPath) (s ^. help), mainScreen]
        (Just Library, _)               -> [mainScreen]
        (Just Inbox, _)                 -> [mainScreen]
        (Just NameSuggestions, Just fi) -> [drawImportWidget currentFocus fi, mainScreen]
        (Just FileNameEdit, Just fi)    -> [drawImportWidget currentFocus fi, mainScreen]
        _ -> []
    where
        focus = F.focusGetCurrent (s ^. focusRing)

        inboxWidget =
            L.renderList drawFileInfo (focus == Just Inbox) (s ^. inbox)

        libraryWidget =
            L.renderList drawFileInfo (focus == Just Library) (s ^. library)

        inboxDirs = s ^. config . Config.inboxDirs

        homeDirText =
            s ^. config . Config.homeDir
                & Path.fromAbsDir
                & T.pack

        shortenDir = T.unpack . T.replace homeDirText "~/" . T.pack

        inboxLabel =
            inboxDirs
                & map Path.fromAbsDir
                & map shortenDir
                & intercalate ","

        libraryLabel =
            s ^. config . Config.libraryDir
                & Path.fromAbsDir
                & shortenDir

        title = " PAPERBOY " <> "v" <> pboyVersion <> " "

        libraryAndInbox =
            withBorderStyle BS.unicodeRounded
                $ joinBorders . B.borderWithLabel (str title)
                $ vBox
                    [ libraryWidget
                    , B.hBorder
                    , inboxWidget
                    ]

        statusBar =
            vLimit 1 $ hBox
                [ str $
                    (if length inboxDirs > 1 then "[Inboxes]" else "[Inbox]")
                    <> " "
                    <> inboxLabel
                , fill ' '
                , str " press h for help "
                , fill ' '
                , str $ libraryLabel <> " [Library]"
                ]

        mainScreen =
            libraryAndInbox <=> statusBar

        currentFocus = F.focusGetCurrent (s ^. focusRing)


handleEvent :: State -> BrickEvent ResourceName Event -> EventM ResourceName (Next State)
handleEvent s (VtyEvent e) =
    let
        focus = F.focusGetCurrent (s ^. focusRing)

        cycleFocus = continue $ s & focusRing %~ F.focusNext

        openHelp =
            continue $ s
                & focusRing .~ F.focusRing [Help]
                & help ?~ helpDialog

        backToMain =
            continue $ s
                & focusRing .~ inboxFocus
                & help .~ Nothing

        cycleLibSortAndReferesh = do
            cycleState <- liftIO $ cycleLibSort s
            newState <- liftIO $ refreshFiles cycleState
            continue $ newState
    in
    case (focus, e) of
        (_, V.EvKey (V.KChar 'c') [V.MCtrl])            -> halt s
        (Just Inbox,          V.EvKey V.KEsc [])        -> halt s
        (Just Library,        V.EvKey V.KEsc [])        -> halt s
        (Just (FirstStart _), V.EvKey V.KEsc [])        -> halt s
        (Just Inbox,          V.EvKey (V.KChar 'q') []) -> halt s
        (Just Library,        V.EvKey (V.KChar 'q') []) -> halt s
        (Just (FirstStart _), V.EvKey (V.KChar 'q') []) -> halt s

        (Just (FirstStart cpath), V.EvKey V.KEnter []) ->
            case D.dialogSelection =<< s ^. firstStart of
                Just ConfigCreate -> do
                    _ <- liftIO $ Config.createConfig cpath
                    newState <- liftIO initState
                    continue newState
                _ ->
                    halt s

        (Just Help, V.EvKey V.KEnter []) -> backToMain

        (_, V.EvKey V.KEsc []) -> backToMain

        (Just Inbox,   V.EvKey (V.KChar '\t') []) -> cycleFocus
        (Just Library, V.EvKey (V.KChar '\t') []) -> cycleFocus
        (Just NameSuggestions, V.EvKey (V.KChar '\t') []) -> cycleFocus
        (Just FileNameEdit,    V.EvKey (V.KChar '\t') []) -> cycleFocus

        (Just Inbox,   V.EvKey (V.KChar 'h') []) -> openHelp
        (Just Library, V.EvKey (V.KChar 'h') []) -> openHelp
        (Just Help,    V.EvKey (V.KChar 'h') []) -> backToMain

        (Just Inbox, V.EvKey (V.KChar 'l') [])   -> cycleLibSortAndReferesh
        (Just Library, V.EvKey (V.KChar 'l') []) -> cycleLibSortAndReferesh
        
        _ ->
            case (focus, s ^. fileImport) of
                (Just Library, _) ->
                    handleLibraryEvent s e

                (Just Inbox, _) ->
                    handleInboxEvent s e

                (Just NameSuggestions, Just fi) ->
                    handleImportScreenEvent fi s e

                (Just FileNameEdit, Just fi) ->
                    handleImportScreenEvent fi s e

                (Just (FirstStart _), _) ->
                    handleFirstStartEvent s e

                _ ->
                    continue s
handleEvent s _ = continue s


handleFirstStartEvent :: State -> V.Event -> EventM ResourceName (Next State)
handleFirstStartEvent s e =
    case s ^. firstStart of
        Just dialog -> do
            newDialog <- D.handleDialogEvent e dialog
            continue (s & firstStart ?~ newDialog)
        Nothing -> continue s


openAction :: State -> EventM n (Next State)
openAction s =
    let
        openFile fileName = do
            _ <- liftIO $ Lib.openFile fileName
            continue s
    in
    case L.listSelectedElement (s ^. library) of
        Just (_, fileInfo) -> openFile (Lib._fileName fileInfo)
        _                  -> continue s


handleLibraryEvent :: State -> V.Event -> EventM ResourceName (Next State)
handleLibraryEvent s e =
    let renameAction =
            case L.listSelectedElement (s ^. library) of
                Just (_, fileInfo) -> beginFileImport s fileInfo
                _                  -> continue s
    in
    case e of
        V.EvKey V.KEnter [] ->
            openAction s

        V.EvKey (V.KChar ' ') [] ->
            openAction s

        V.EvKey (V.KChar 'r') [] ->
            renameAction

        V.EvKey (V.KChar 'o') [] ->
            openAction s

        _ -> do
            newLibrary <- L.handleListEvent e (s ^. library)
            continue (s & library .~ newLibrary)


handleInboxEvent :: State -> V.Event -> EventM ResourceName (Next State)
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

        V.EvKey (V.KChar 'o') [] ->
            openAction s

        _ -> do
            newInbox <- L.handleListEvent e (s ^. inbox)
            continue (s & inbox .~ newInbox)


handleImportScreenEvent :: FileImport -> State -> V.Event -> EventM ResourceName (Next State)
handleImportScreenEvent fi s ev =
    let
        focus = F.focusGetCurrent (s ^. focusRing)
    in
    case (focus, ev) of
        (_, V.EvKey (V.KChar 'o') [V.MCtrl]) ->
            do
                _ <- liftIO $ Lib.openFile (fi ^. currentFile)
                continue s

        (_, V.EvKey V.KEnter []) ->
            do
                let
                    conf = s ^. config

                    newFileName =
                        fi ^. nameEdit
                            & E.getEditContents
                            & T.unlines
                            & Lib.finalFileName conf

                _ <- liftIO $
                    Lib.fileFile conf newFileName (fi ^. currentFile)

                newState <- liftIO $ refreshFiles s

                continue $ newState
                    & focusRing .~ inboxFocus

        (Just NameSuggestions, _) ->
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

                continue $ s & fileImport ?~ newFileImport

        (Just FileNameEdit, _) -> do
            newEdit <- E.handleEditorEvent ev (fi ^. nameEdit)
            let newFileImport = fi & nameEdit .~ newEdit
            continue $ s & fileImport ?~ newFileImport

        _ -> continue s


beginFileImport :: State -> Lib.FileInfo -> EventM ResourceName (Next State)
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

        (fileName, nameSuggestions) = fileNameSuggestions

        newFileNames =
            L.list NameSuggestions (Vec.fromList nameSuggestions) 1

        newState = s
            & focusRing .~ F.focusRing [FileNameEdit, NameSuggestions]
            & fileImport ?~ fi

    handleImportScreenEvent fi newState (V.EvKey V.KDown [])


--


drawFileInfo :: Bool -> Lib.FileInfo -> Widget ResourceName
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


drawImportWidget :: Maybe ResourceName -> FileImport -> Widget ResourceName
drawImportWidget focus fi =
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
                    (fi ^. nameEdit)
            , vLimit 1 (fill ' ')
            , str "suggestions:"
            , B.hBorder
            , vLimit 6
                $ L.renderList
                    (\_ t -> str (T.unpack t))
                    (focus == Just NameSuggestions)
                    (fi ^. suggestions)
            , fill ' '
            , str
                "[Esc]    - cancel.\n\
                \[Tab]    - switch between editor and suggestions.\n\
                \[Enter]  - rename the file and move it to your library folder.\n\
                \[Ctrl-o] - open the file that you're currently renaming."
            ]


helpDialog :: D.Dialog HelpChoice
helpDialog =
    D.dialog (Just " Help ") (Just (0, choices)) 75
    where choices = [("Cool", HelpClose)]


helpScreen :: Path Abs File -> Maybe (D.Dialog HelpChoice) -> Widget ResourceName
helpScreen cpath (Just d) =
    D.renderDialog d
        $ C.hCenter
        $ padAll 1
        $ vBox $ map str
            [ "Welcome to PAPERBOY!"
            , "===================="
            , " "
            , "[Tab] - switch between inbox and library."
            , " "
            , "[Enter] or [Space]:"
            , "    - from inbox: start import/rename."
            , "    - from library: open pdf."
            , "[r] - rename file"
            , "[o] - open file"
            , " "
            , "[Esc] or [q] - quit from main screen."
            , "[Ctrl-c]     - quit from any screen."
            , "[h]          - this help screen."
            , "[l]          - switch library sort mode"
            , " "
            , "Your config file is at"
            , Path.fromAbsFile cpath
            , " "
            , "enjoy!"
            ]
helpScreen _ _ = str ""


firstStartDialog :: D.Dialog ConfigChoice
firstStartDialog =
    D.dialog (Just " Welcome to PAPERBOY ") (Just (0, choices)) 75
    where
        choices = [("Create Config", ConfigCreate), ("Abort Mission", ConfigAbort)]


missingConfigScreen :: Path Abs File -> Maybe (D.Dialog ConfigChoice) -> Widget ResourceName
missingConfigScreen cpath (Just d) =
    D.renderDialog d
        $ padAll 1
        $ vBox
            [ C.hCenter (str "I will create a config file at")
            , vLimit 1 (fill ' ')
            , C.hCenter (str $ Path.fromAbsFile cpath)
            ]
missingConfigScreen _ _ = str ""
