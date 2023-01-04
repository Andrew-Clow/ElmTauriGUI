module Main exposing (..)

import Browser
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Html
import Main.Config as Config exposing (Config, ConfigMsg(..), PersistentConfig)
import Maybe.Extra
import Task exposing (Task)
import TaskPort exposing (Error)
import Tauri
import Tauri.BaseDir as BaseDir exposing (BaseDir(..))
import Tauri.DateTime as DateTime exposing (DateTime)
import Tauri.Dialog as Dialog2 exposing (InfoWarningOrError(..), TitleOrAppName(..))
import Tauri.FS as FS
import Tauri.FSInBaseDir as FSInBaseDir
import Tauri.Modified as Modified
import Tauri.Path as Path
import Time


type alias TaskErrorResultMsg a =
    Task Error (Result Msg a)


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias FilePath =
    String


type alias Model =
    { answerWas : { text : String, good : Good }
    , textFileContent : Maybe String
    , showPathButtons : Bool
    , config : PersistentConfig
    , zone : Time.Zone
    }


good : Model -> String -> Model
good model text =
    { model | answerWas = { text = text, good = Good } }


bad : Model -> String -> Model
bad model text =
    { model | answerWas = { text = text, good = Bad } }


type Good
    = Good
    | Bad
    | Neutral
    | Broken


type Button
    = AskDialog
    | ConfirmDialog
    | MessageDialog
    | OpenDirectoriesDialog
    | OpenFileDialog
    | SaveDialog
    | ReadTextFile
    | CopyFile
    | ChooseToCreateDir
    | CheckRealFileExists
    | CheckFakeFileExists
    | ReadDir
    | RemoveDir
    | RemoveFile
    | RenameFile
    | WriteTextFileContaining String
    | GetPath BaseDir.BaseDir
    | ConfigMsg Config.ConfigMsg
    | GetModified


type Msg
    = -- setup ----------------------------------------------------------------
      IgnoreTauriFeedback --    noop for boring events
    | InteropError TaskPort.InteropError -- TaskPort setup
    | JSReturnError TaskPort.JSError --     TaskPort setup
    | GotConfig (Result String Config.PersistentConfig) --  app setup
    | GotTimeZone Time.Zone --                              app setup
      -- interaction ----------------------------------------------------------
      -- Dialog
    | Pressed Button
    | YesNo { pressedYes : Bool }
    | OKCancel { pressedOK : Bool }
    | GotStrings (List String)
    | GotFilePath String
    | GotFileContents Tauri.FileContents
    | Say String
      -- FS
    | Cancelled
    | Copied { from : FilePath, to : FilePath }
    | CreatedDir FilePath
    | Created FilePath
    | Existence Bool FilePath
    | Folder Tauri.FolderContents
    | RemovedFile FilePath
    | Renamed (Result String { from : FilePath, to : FilePath })
    | ToggleTextBox
    | EditedTextBox String
    | WroteTextFile { filePath : FilePath, fileWas : Tauri.FileWas }
    | NoSuchFolder FilePath
      -- Path
    | ToggleShowPathButtons
    | GotPath BaseDir.BaseDir FilePath
      -- Modified
    | GotModified { filePath : FilePath, modified : DateTime }


init : flags -> ( Model, Cmd Msg )
init =
    \_ ->
        ( { answerWas = { text = "", good = Neutral }
          , textFileContent = Nothing
          , showPathButtons = False
          , config = Config.default
          , zone = Time.utc
          }
        , Cmd.batch
            [ Config.init GotConfig
            , Task.perform GotTimeZone Time.here
            ]
        )


view : Model -> Html.Html Msg
view model =
    Element.layout [ Element.padding 20 ] <|
        Element.column [ Element.spacing 25 ]
            [ Element.text "Hello! This is just to experiment with using Tauri with elm."
            , Element.column [ Element.spacing 9 ]
                [ Element.text "Dialog"
                , Element.row [ Element.spacing 10 ]
                    [ button AskDialog
                    , button ConfirmDialog
                    , button MessageDialog
                    ]
                , Element.row [ Element.spacing 10 ]
                    [ button OpenDirectoriesDialog
                    , button OpenFileDialog
                    , button SaveDialog
                    ]
                , Element.text " "
                , Element.text "FS"
                , Element.row [ Element.spacing 10 ]
                    [ button ReadTextFile
                    , button CopyFile
                    , button RemoveFile
                    , button RenameFile
                    , greenButton "Write Text File (if different)" ToggleTextBox
                    ]
                , Element.row [ Element.spacing 10 ]
                    [ button CheckRealFileExists
                    , button CheckFakeFileExists
                    , button ChooseToCreateDir
                    , button ReadDir
                    , button RemoveDir
                    ]
                , Element.text " "
                , Element.text "Persistence"
                , Element.row [ Element.spacing 10 ]
                    [ button <| ConfigMsg (ChangeCheesePerPageBy 1)
                    , button <| ConfigMsg (ChangeCheesePerPageBy -1)
                    ]
                , Element.row [ Element.spacing 10 ]
                    [ button <| ConfigMsg (AddCheese <| Config.Hard "Gruyere")
                    , button <| ConfigMsg (RemoveCheese <| Config.Hard "Gruyere")
                    ]
                , Element.text " "
                , Element.row [ Element.spacing 30 ]
                    [ Element.column [ Element.spacing 10 ]
                        [ Element.text "Path"
                        , Element.row [ Element.spacing 10 ]
                            [ greenButton "Get Path..." ToggleShowPathButtons
                            ]
                        ]
                    , Element.column [ Element.spacing 10 ]
                        [ Element.text "Modified"
                        , Element.row [ Element.spacing 10 ]
                            [ button GetModified
                            ]
                        ]
                    ]
                ]
            , if not model.showPathButtons then
                Element.none

              else
                Element.column [ Element.spacing 5 ] <|
                    List.map
                        (Element.row [ Element.spacing 5 ]
                            << List.map (button << GetPath)
                        )
                        [ [ App, AppConfig, AppData, AppLocalData, AppLog ]
                        , [ Audio, Cache, BaseDir.Config, Data, Desktop, Document ]
                        , [ Download, Executable, Home, LocalData, Log, Picture ]
                        , [ Public, Resource, Runtime, Temp, Template, Video ]
                        ]
            , Element.el
                [ Element.Font.color <|
                    case model.answerWas.good of
                        Good ->
                            Element.rgb255 125 208 125

                        Bad ->
                            Element.rgb255 210 120 142

                        Neutral ->
                            Element.rgb255 85 116 208

                        Broken ->
                            Element.rgb255 250 0 0
                ]
              <|
                Element.text model.answerWas.text
            , case model.textFileContent of
                Nothing ->
                    Element.none

                Just content ->
                    Element.row [ Element.spacing 10 ]
                        [ Element.Input.text []
                            { onChange = EditedTextBox
                            , text = content
                            , placeholder = Just (Element.Input.placeholder [] <| Element.text "Contents to write to text file.")
                            , label = Element.Input.labelLeft [] <| Element.text ""
                            }
                        , button (WriteTextFileContaining content)
                        ]
            ]


button : Button -> Element Msg
button b =
    greenButton (buttonName b) (Pressed b)


greenButton : String -> Msg -> Element Msg
greenButton string msg =
    Element.Input.button
        [ Element.Background.color <| Element.rgb255 200 255 200
        , Element.Border.rounded 10
        , Element.padding 8
        , Element.Border.shadow
            { offset = ( 2, 2 )
            , size = 2
            , blur = 2
            , color = Element.rgb255 150 244 150
            }
        ]
        { onPress = Just msg, label = Element.text string }


buttonName : Button -> String
buttonName btn =
    case btn of
        AskDialog ->
            "Ask Dialog"

        ConfirmDialog ->
            "Confirm Dialog"

        MessageDialog ->
            "Message Dialog"

        OpenDirectoriesDialog ->
            "Open Directories Dialog"

        OpenFileDialog ->
            "Open File Dialog"

        SaveDialog ->
            "Save Dialog"

        ReadTextFile ->
            "Read Text File"

        CopyFile ->
            "Copy File"

        ChooseToCreateDir ->
            "Create Dir"

        ReadDir ->
            "Read Dir"

        RemoveDir ->
            "Remove Dir"

        RemoveFile ->
            "Remove File"

        RenameFile ->
            "Rename File"

        GetPath baseDir ->
            BaseDir.toString baseDir

        ConfigMsg configMsg ->
            Config.configMsgToString configMsg

        GetModified ->
            "Get File Modified Date/Time"

        WriteTextFileContaining _ ->
            "Save"

        CheckRealFileExists ->
            "Check File Exists"

        CheckFakeFileExists ->
            "Check File Doesn't Exist"


toCmd : (a -> Msg) -> Task Error a -> Cmd Msg
toCmd =
    Tauri.toCmd3 InteropError JSReturnError


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Pressed btn ->
            ( good model <| buttonName btn, press model btn )

        YesNo result ->
            ( good model <| iff result.pressedYes "Yes" "No", Cmd.none )

        OKCancel result ->
            ( good model <| iff result.pressedOK "OK" "Cancel", Cmd.none )

        IgnoreTauriFeedback ->
            ( model, Cmd.none )

        GotStrings list ->
            ( good model <| String.join "\n" list, Cmd.none )

        GotFileContents fileContents ->
            let
                showFileContents : Tauri.FileContents -> String
                showFileContents { filePath, contents } =
                    filePath ++ "\n\n" ++ contents
            in
            ( good model <| showFileContents fileContents, Cmd.none )

        Copied fromTo ->
            ( { model | answerWas = { text = "Copied " ++ fromTo.from ++ " to " ++ fromTo.to, good = Good } }, Cmd.none )

        CreatedDir filePath ->
            ( { model | answerWas = { text = "Created directory\n " ++ filePath, good = Good } }
            , FS.createDir filePath (Created filePath) |> toCmd identity
            )

        Created filePath ->
            ( { model | answerWas = { text = filePath, good = Good } }, Cmd.none )

        Existence exists filePath ->
            ( { model | answerWas = { text = filePath ++ iff exists "\n exists." "\n doesn't exist.", good = Good } }
            , Cmd.none
            )

        Folder folderContents ->
            ( { model | answerWas = { text = showFolderContents folderContents, good = Good } }, Cmd.none )

        InteropError interopError ->
            ( { model | answerWas = { text = "Interop Error:\n" ++ TaskPort.interopErrorToString interopError, good = Broken } }, Cmd.none )

        JSReturnError jSError ->
            ( { model | answerWas = { text = "Error Returned from Javascript:\n" ++ TaskPort.errorToString (TaskPort.JSError jSError), good = Bad } }, Cmd.none )

        RemovedFile filePath ->
            ( good model <| "Removed " ++ filePath, Cmd.none )

        Renamed result ->
            case result of
                Ok record ->
                    ( { model | answerWas = { text = "Renamed\n " ++ record.from ++ "\n to \n " ++ record.to, good = Good } }, Cmd.none )

                Err string ->
                    ( bad model string, Cmd.none )

        ToggleTextBox ->
            case model.textFileContent of
                Nothing ->
                    ( { model | textFileContent = Just "", answerWas = { text = "Writing a text file...", good = Neutral } }, Cmd.none )

                Just _ ->
                    ( { model | textFileContent = Nothing, answerWas = { text = "Cancelled writing a text file", good = Neutral } }, Cmd.none )

        WroteTextFile { filePath, fileWas } ->
            ( good { model | textFileContent = Nothing } <| filePath ++ " \n " ++ Tauri.fileWasToString fileWas, Cmd.none )

        EditedTextBox string ->
            ( { model | textFileContent = Just string }, Cmd.none )

        ToggleShowPathButtons ->
            ( { model | showPathButtons = not model.showPathButtons }, Cmd.none )

        GotPath baseDir filePath ->
            ( good model <| BaseDir.toString baseDir ++ ":\n" ++ filePath, Cmd.none )

        GotConfig result ->
            case result of
                Ok newConfig ->
                    ( good { model | config = newConfig } <|
                        "saved config: \n cheesesPerPage = "
                            ++ String.fromInt (Config.getCheesesPerPage newConfig)
                            ++ "\n cheeses = \n   "
                            ++ String.join "\n   " (List.map Config.cheeseToString <| Config.getCheeses newConfig)
                    , Cmd.none
                    )

                Err error ->
                    ( { model | answerWas = { text = error, good = Bad } }, Cmd.none )

        GotTimeZone zone ->
            ( { model | zone = zone }, Cmd.none )

        GotModified modified ->
            ( good model <| modified.filePath ++ "\n" ++ DateTime.dateTimeToString modified.modified, Cmd.none )

        Cancelled ->
            ( { model | answerWas = { text = "Cancelled", good = Neutral } }, Cmd.none )

        NoSuchFolder filePath ->
            ( bad model <| "Folder doesn't exist: \n" ++ filePath, Cmd.none )

        GotFilePath path ->
            ( good model path, Cmd.none )

        Say string ->
            ( good model string, Cmd.none )


iff : Bool -> a -> a -> a
iff true x y =
    if true then
        x

    else
        y


press : Model -> Button -> Cmd Msg
press model btn =
    case btn of
        ConfirmDialog ->
            Dialog2.confirm "Is this really a confirmation question?"
                { title = Just "Confirm" -- defaults to the app name
                , dialogType = Warning
                }
                { ok = Say "OK", cancel = Say "Cancel" }
                |> toCmd identity

        AskDialog ->
            Dialog2.ask "Is this really a question?"
                { title = Nothing -- defaults to the app name
                , dialogType = Info
                }
                { yes = Say "Yes", no = Say "No" }
                |> toCmd identity

        MessageDialog ->
            Dialog2.message "Here's a little message for you"
                { title = Nothing, dialogType = Info }
                IgnoreTauriFeedback
                |> toCmd identity

        OpenDirectoriesDialog ->
            Dialog2.openDirectories
                { defaultPath = Nothing
                , recursive = True
                , title = Just "Please pick a directory or directories"
                }
                { cancelled = Cancelled, chose = GotStrings }
                |> toCmd identity

        OpenFileDialog ->
            Dialog2.openFile
                { defaultPath = Nothing
                , filters = [ { extensions = [ "txt", "elm", "md" ], name = "Texty Files" } ]
                , title = Just "Please pick a file"
                }
                { chose = GotFilePath, cancelled = Cancelled }
                |> toCmd identity

        SaveDialog ->
            Dialog2.save
                { defaultPath = Nothing
                , filters = [ { extensions = [ "txt", "elm", "md" ], name = "Texty Files" } ]
                , title = Just "What do you want me to save it as?"
                }
                { chose = GotFilePath, cancelled = Cancelled }
                |> toCmd identity

        ReadTextFile ->
            Dialog2.openFile
                { defaultPath = Nothing
                , filters = [ { extensions = [ "txt", "elm", "md" ], name = "Texty Files" } ]
                , title = Just "Pick a text file to read"
                }
                { cancelled = Err Cancelled, chose = Ok }
                |> Tauri.andThenWithoutResult FS.readTextFile
                |> Tauri.resultToMsg identity GotFileContents
                |> toCmd identity

        CopyFile ->
            let
                open : TaskErrorResultMsg FilePath
                open =
                    Dialog2.openFile
                        { defaultPath = Nothing
                        , filters = [ { extensions = [ "txt", "elm", "md" ], name = "Texty Files" } ]
                        , title = Just "Pick a text file to copy"
                        }
                        { cancelled = Err <| Say "Didn't pick a file to copy", chose = Ok }

                saveDialog : FilePath -> TaskErrorResultMsg { from : FilePath, to : FilePath }
                saveDialog fromFile =
                    Dialog2.save { defaultPath = Nothing, filters = [], title = Just "What should I save it as?" }
                        { cancelled = Err <| Say "Didn't pick a file name to save it to"
                        , chose = \toFile -> Ok { from = fromFile, to = toFile }
                        }

                areYouSure fromTo =
                    Dialog2.ask
                        ("Are you sure you want to copy \n" ++ Path.toFileName fromTo.from ++ "\n to \n" ++ Path.toFileName fromTo.to ++ "\n?")
                        { title = Just "Are you sure?", dialogType = Warning }
                        { no = Err Cancelled, yes = Ok fromTo }

                copy : { from : FilePath, to : FilePath } -> Task Error Msg
                copy fromTo =
                    FS.copyFile fromTo (Copied fromTo)
            in
            open
                |> Tauri.andThen saveDialog
                |> Tauri.andThen areYouSure
                |> Tauri.andThenWithoutResult copy
                |> toCmd Tauri.resultsCombine

        ChooseToCreateDir ->
            Dialog2.save
                { defaultPath = Nothing
                , filters = []
                , title = Just "Please enter the name of your new folder."
                }
                { cancelled = Cancelled, chose = CreatedDir }
                |> toCmd identity

        ReadDir ->
            let
                openDir : Bool -> TaskErrorResultMsg FilePath
                openDir recursive =
                    Dialog2.openDirectory { defaultPath = Nothing, recursive = recursive, title = Just "Choose a directory to read" }
                        { cancelled = Err Cancelled, chose = Ok }

                getExistence : FilePath -> TaskErrorResultMsg FilePath
                getExistence filePath =
                    FS.exists filePath { yes = Ok, no = NoSuchFolder >> Err }

                readDir : FilePath -> Task Error Tauri.FolderContents
                readDir filePath =
                    FSInBaseDir.readDir Home { recursive = True } filePath
            in
            Dialog2.ask "Recursively?" { title = Nothing, dialogType = Info } { yes = True, no = False }
                |> Task.andThen openDir
                |> Tauri.andThen getExistence
                |> Tauri.andThenWithoutResult readDir
                |> Tauri.resultToMsg identity Folder
                |> toCmd identity

        RemoveDir ->
            let
                areYouSure : FilePath -> TaskErrorResultMsg FilePath
                areYouSure filePath =
                    Dialog2.ask
                        ("Are you sure you want to remove " ++ Path.toFileName filePath ++ " ?")
                        { title = Just "Are you sure?", dialogType = Warning }
                        { yes = Ok filePath, no = Err Cancelled }

                remove : FilePath -> Task Error Msg
                remove filePath =
                    FS.removeDir filePath (RemovedFile filePath)
            in
            Dialog2.openDirectory
                { defaultPath = Nothing, recursive = False, title = Just "Choose a directory to read" }
                { cancelled = Err Cancelled, chose = Ok }
                |> Tauri.andThen areYouSure
                |> Tauri.andThenWithoutResult remove
                |> toCmd Tauri.resultsCombine

        RemoveFile ->
            let
                ask : FilePath -> TaskErrorResultMsg FilePath
                ask filePath =
                    Dialog2.ask
                        ("Are you sure you want to remove " ++ Path.toFileName filePath ++ " ?")
                        { title = Just "Are you sure?", dialogType = Warning }
                        { yes = Ok filePath, no = Err Cancelled }

                remove : FilePath -> Task Error Msg
                remove filePath =
                    FS.removeFile filePath (RemovedFile filePath)
            in
            Dialog2.openFile
                { defaultPath = Nothing, filters = [], title = Just "Choose file to delete" }
                { cancelled = Err Cancelled, chose = Ok }
                |> Tauri.andThen ask
                |> Tauri.andThenWithoutResult remove
                |> toCmd Tauri.resultsCombine

        RenameFile ->
            let
                open : TaskErrorResultMsg FilePath
                open =
                    Dialog2.openFile
                        { defaultPath = Nothing
                        , filters = [ { extensions = [ "txt", "elm", "md" ], name = "Texty Files" } ]
                        , title = Just "Pick a text file to copy"
                        }
                        { cancelled = Err <| Say "Didn't pick a file to rename", chose = Ok }

                saveDialog : FilePath -> TaskErrorResultMsg { from : FilePath, to : FilePath }
                saveDialog fromFile =
                    Dialog2.save { defaultPath = Nothing, filters = [], title = Just "What should I rename it as?" }
                        { cancelled = Err <| Say "Didn't pick a file name to rename it to"
                        , chose = \toFile -> Ok { from = fromFile, to = toFile }
                        }

                areYouSure fromTo =
                    Dialog2.ask
                        ("Are you sure you want to rename \n" ++ Path.toFileName fromTo.from ++ "\n to \n" ++ Path.toFileName fromTo.to ++ "\n?")
                        { title = Just "Are you sure?", dialogType = Warning }
                        { no = Err Cancelled, yes = Ok fromTo }

                rename : { from : FilePath, to : FilePath } -> Task Error Msg
                rename fromTo =
                    FS.renameFile fromTo (Copied fromTo)
            in
            open
                |> Tauri.andThen saveDialog
                |> Tauri.andThen areYouSure
                |> Tauri.andThenWithoutResult rename
                |> toCmd Tauri.resultsCombine

        GetPath baseDir ->
            Path.get baseDir
                |> toCmd (GotPath baseDir)

        ConfigMsg configMsg ->
            Config.updateFromDisk GotConfig configMsg

        GetModified ->
            Dialog2.openFile
                { defaultPath = Nothing
                , filters = []
                , title = Just "Pick a file to see when it was last modified."
                }
                { cancelled = Err Cancelled, chose = Ok }
                |> Tauri.andThenWithoutResult (Modified.getFileModifiedDateTime model.zone)
                |> Tauri.resultToMsg identity GotModified
                |> toCmd identity

        WriteTextFileContaining string ->
            Dialog2.save { defaultPath = Nothing, filters = [], title = Just "Save as" }
                { cancelled = Err Cancelled, chose = Ok }
                |> Tauri.andThenWithoutResult
                    (\filePath -> FS.writeTextFileIfDifferent { filePath = filePath, contents = string })
                |> Tauri.resultToMsg identity WroteTextFile
                |> toCmd identity

        CheckRealFileExists ->
            Dialog2.openFile
                { defaultPath = Nothing, filters = [], title = Just "Pick an existing file" }
                { cancelled = Err Cancelled, chose = Ok }
                |> Tauri.andThenWithoutResult (\filePath -> FS.exists filePath { yes = Existence True, no = Existence False })
                |> toCmd Tauri.resultsCombine

        CheckFakeFileExists ->
            Dialog2.save
                { defaultPath = Nothing, filters = [], title = Just "Pretend to create a file" }
                { cancelled = Err Cancelled, chose = Ok }
                |> Tauri.andThenWithoutResult (\filePath -> FS.exists filePath { yes = Existence True, no = Existence False })
                |> toCmd Tauri.resultsCombine


showFolderContents : Tauri.FolderContents -> String
showFolderContents (Tauri.FolderContents list) =
    "\n" ++ (String.join "\n" <| List.map showFile list) ++ "\n"


showFile : Tauri.FileEntry -> String
showFile { folderContents, name, path } =
    let
        folder =
            case folderContents of
                Nothing ->
                    ""

                Just (Tauri.FolderContents _) ->
                    " -> " ++ (indent 4 <| Maybe.Extra.unwrap "" showFolderContents folderContents)
    in
    Maybe.withDefault "" name
        ++ folder


indent : Int -> String -> String
indent n string =
    let
        indentation =
            "\n" ++ String.repeat n " "
    in
    String.join indentation <| String.lines string
