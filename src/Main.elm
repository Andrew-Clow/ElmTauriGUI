module Main exposing (..)

import Browser
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Html
import Json.Decode
import Main.Config as Config exposing (Config, ConfigMsg(..), PersistentConfig)
import Maybe.Extra
import Task exposing (Task)
import TaskPort exposing (Error)
import Tauri
import Tauri.BaseDir as BaseDir exposing (BaseDir(..))
import Tauri.Constant exposing (..)
import Tauri.DateTime as DateTime exposing (DateTime)
import Tauri.Dialog as Dialog
import Tauri.Dialog2 as Dialog2 exposing (InfoWarningOrError(..), TitleOrAppName(..))
import Tauri.FS as FS
import Tauri.FS2 as FS2
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
    , readFilePath : Maybe FilePath
    , saveFilePath : Maybe FilePath
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
    | TestbaseDirectoryIsTotal
    | ConfigMsg Config.ConfigMsg
    | GetModified


type Msg
    = Pressed Button
    | IgnoreTauriFeedback
    | InteropError TaskPort.InteropError
    | JSReturnError TaskPort.JSError
    | YesNo { pressedYes : Bool }
    | OKCancel { pressedOK : Bool }
    | GotMaybeString (Maybe String)
    | GotMaybeStrings (Maybe (List String))
    | NoReadFileSpecified
    | GotFilePath (Maybe String)
    | NoSaveFileSpecified
    | GotSaveFilePath (Maybe String)
    | GotFileContents Tauri.FileContents
    | Cancelled
    | Copied (Result String { from : FilePath, to : FilePath })
    | CreateDir (Maybe FilePath)
    | Created FilePath
    | Existence Bool FilePath
    | Folder Tauri.FolderContents
    | RemovedFile FilePath
    | Renamed (Result String { from : FilePath, to : FilePath })
    | ToggleTextBox
    | WroteTextFile (Maybe { filePath : FilePath, fileWas : Tauri.FileWas })
    | EditedTextBox String
    | ToggleShowPathButtons
    | GotPath BaseDir.BaseDir FilePath
    | GotNumbers (List Int)
    | GotConfig (Result String Config.PersistentConfig)
    | GotTimeZone Time.Zone
    | GotModified (Maybe { filePath : FilePath, modified : DateTime })
    | NoSuchFolder FilePath


init : flags -> ( Model, Cmd Msg )
init =
    \_ ->
        ( { answerWas = { text = "", good = Neutral }
          , readFilePath = Nothing
          , saveFilePath = Nothing
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
                , Element.text "Note that if I open a dialog to ask for something,"
                , Element.text "you need to press the button again afterwards."
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
            , Element.column [ Element.spacing 7, Element.Font.color <| Element.rgb255 9 85 165 ]
                [ Element.text <| "Read: " ++ Maybe.withDefault "" model.readFilePath
                , Element.text <| "Save: " ++ Maybe.withDefault "" model.saveFilePath
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

        TestbaseDirectoryIsTotal ->
            "Temp Test"

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

        GotMaybeString result ->
            ( good model <| showMaybe result, Cmd.none )

        GotMaybeStrings result ->
            ( good model <| showMaybeList result, Cmd.none )

        GotFilePath result ->
            ( good { model | readFilePath = result } <| showMaybe result, Cmd.none )

        GotSaveFilePath result ->
            ( good { model | saveFilePath = result } <| showMaybe result, Cmd.none )

        NoReadFileSpecified ->
            ( { model | answerWas = { text = "No file specified", good = Bad } }
            , Dialog.openFile
                { defaultPath = Nothing
                , filters = [ { extensions = [ "txt", "elm", "md" ], name = "Texty Files" } ]
                , title = Just "Please pick a text file"
                }
                |> toCmd GotFilePath
            )

        NoSaveFileSpecified ->
            ( { model | answerWas = { text = "No save destination specified", good = Bad } }
            , Dialog.save
                { defaultPath = Nothing
                , filters = [ { extensions = [ "txt", "elm", "md" ], name = "Texty Files" } ]
                , title = Just "Please pick a text file to save to"
                }
                |> toCmd GotSaveFilePath
            )

        GotFileContents fileContents ->
            let
                showFileContents : Tauri.FileContents -> String
                showFileContents { filePath, contents } =
                    filePath ++ "\n\n" ++ contents
            in
            ( good model <| showFileContents fileContents, Cmd.none )

        Copied result ->
            case result of
                Ok record ->
                    ( { model | answerWas = { text = "Copied " ++ record.from ++ " to " ++ record.to, good = Good } }, Cmd.none )

                Err string ->
                    ( { model | answerWas = { text = string, good = Neutral } }, Cmd.none )

        CreateDir result ->
            case result of
                Just filePath ->
                    ( { model | answerWas = { text = "Saving as " ++ filePath, good = Good } }
                    , FS2.createDir filePath (Created filePath) |> toCmd identity
                    )

                Nothing ->
                    ( { model | answerWas = { text = "Save cancelled", good = Neutral } }, Cmd.none )

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

        WroteTextFile maybe ->
            case maybe of
                Nothing ->
                    ( { model | answerWas = { text = "Cancelled save", good = Neutral } }, Cmd.none )

                Just { filePath, fileWas } ->
                    ( good { model | textFileContent = Nothing } <| filePath ++ " \n " ++ Tauri.fileWasToString fileWas, Cmd.none )

        EditedTextBox string ->
            ( { model | textFileContent = Just string }, Cmd.none )

        ToggleShowPathButtons ->
            ( { model | showPathButtons = not model.showPathButtons }, Cmd.none )

        GotPath baseDir filePath ->
            ( good model <| BaseDir.toString baseDir ++ ":\n" ++ filePath, Cmd.none )

        GotNumbers ints ->
            ( good model <| String.join " " <| List.map String.fromInt ints, Cmd.none )

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

        GotModified maybeModified ->
            case maybeModified of
                Nothing ->
                    ( model, Cmd.none )

                Just modified ->
                    ( good model <| modified.filePath ++ "\n" ++ DateTime.dateTimeToString modified.modified, Cmd.none )

        Cancelled ->
            ( { model | answerWas = { text = "Cancelled", good = Neutral } }, Cmd.none )

        NoSuchFolder filePath ->
            ( bad model <| "Folder doesn't exist: \n" ++ filePath, Cmd.none )


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
            Dialog.confirmOptions "Is this really a confirmation question?"
                { title = Just "Confirm" -- defaults to the app name
                , dialogType = Just Dialog.Warning -- called type at the typescript end. Defaults to Info
                }
                |> toCmd OKCancel

        AskDialog ->
            Dialog.askOptions "Is this really a question?"
                { title = Nothing -- defaults to the app name
                , dialogType = Nothing -- called type at the typescript end. Defaults to Info
                }
                |> toCmd YesNo

        MessageDialog ->
            Dialog.message "Here's a little message for you" |> toCmd (always IgnoreTauriFeedback)

        OpenDirectoriesDialog ->
            Dialog.openDirectories
                { defaultPath = Nothing
                , recursive = True
                , title = Just "Please pick a directory or directories"
                }
                |> toCmd GotMaybeStrings

        OpenFileDialog ->
            Dialog.openFile
                { defaultPath = Nothing
                , filters = [ { extensions = [ "txt", "elm", "md" ], name = "Texty Files" } ]
                , title = Just "Please pick a file"
                }
                |> toCmd GotFilePath

        SaveDialog ->
            Dialog.save
                { defaultPath = Nothing
                , filters = [ { extensions = [ "txt", "elm", "md" ], name = "Texty Files" } ]
                , title = Just "What do you want me to save it as?"
                }
                |> toCmd GotSaveFilePath

        ReadTextFile ->
            Dialog2.openFile { defaultPath = Nothing, filters = [ { extensions = [ "txt", "elm", "md" ], name = "Texty Files" } ], title = Just "Pick a text file to read" }
                (Err Cancelled)
                Ok
                |> Tauri.andThenDefinitely FS2.readTextFile
                |> Tauri.resultToMsg identity GotFileContents
                |> toCmd identity

        CopyFile ->
            Dialog.openFile { defaultPath = Nothing, filters = [ { extensions = [ "txt", "elm", "md" ], name = "Texty Files" } ], title = Just "Pick a text file to copy" }
                |> Dialog.ifNotPickedOne "Didn't pick a file to copy"
                    (\fromFilePath ->
                        Dialog.save { defaultPath = Nothing, filters = [], title = Just "What should I save it as?" }
                            |> Dialog.ifNotPickedOne "Didn't pick a file name to save it to"
                                (\toFilePath ->
                                    Dialog.askOptions
                                        ("Are you sure you want to copy \n" ++ fromFilePath ++ "\n to \n" ++ toFilePath ++ "\n?")
                                        { title = Just "Are you sure?", dialogType = Just Dialog.Warning }
                                        |> Dialog.ifNo "Cancelled copy"
                                            (FS.copyFile { from = fromFilePath, to = toFilePath }
                                                |> Task.map (always <| Ok { from = fromFilePath, to = toFilePath })
                                            )
                                )
                    )
                |> toCmd Copied

        ChooseToCreateDir ->
            Dialog.save { defaultPath = Nothing, filters = [], title = Just "Please enter the name of your new folder." } |> toCmd CreateDir

        ReadDir ->
            let
                openDir : Bool -> TaskErrorResultMsg FilePath
                openDir recursive =
                    Dialog2.openDirectory { defaultPath = Nothing, recursive = recursive, title = Just "Choose a directory to read" }
                        (Err Cancelled)
                        Ok

                getExistence : FilePath -> TaskErrorResultMsg FilePath
                getExistence filePath =
                    FS2.exists filePath { yes = Ok, no = NoSuchFolder >> Err }

                readDir : FilePath -> Task Error Tauri.FolderContents
                readDir filePath =
                    FSInBaseDir.readDir Home { recursive = True } filePath
            in
            Dialog2.ask "Recursively?" AppNameAsTitle Info { yes = True, no = False }
                |> Task.andThen openDir
                |> Tauri.andThen getExistence
                |> Tauri.andThenDefinitely readDir
                |> Tauri.resultToMsg identity Folder
                |> toCmd identity

        RemoveDir ->
            let
                areYouSure : FilePath -> TaskErrorResultMsg FilePath
                areYouSure filePath =
                    Dialog2.ask
                        ("Are you sure you want to remove " ++ filePath ++ " ?")
                        (Title "Are you sure?")
                        Warning
                        { yes = Ok filePath, no = Err Cancelled }

                remove : FilePath -> Task Error Msg
                remove filePath =
                    FS2.removeDir filePath (RemovedFile filePath)
            in
            Dialog2.openDirectory { defaultPath = Nothing, recursive = False, title = Just "Choose a directory to read" } (Err Cancelled) Ok
                |> Tauri.andThen areYouSure
                |> Tauri.andThenDefinitely remove
                |> Tauri.bothResults
                |> toCmd identity

        RemoveFile ->
            let
                ask : FilePath -> TaskErrorResultMsg FilePath
                ask filePath =
                    Dialog2.ask
                        ("Are you sure you want to remove " ++ filePath ++ " ?")
                        (Title "Are you sure?")
                        Warning
                        { yes = Ok filePath, no = Err Cancelled }

                remove : FilePath -> Task Error Msg
                remove filePath =
                    FS2.removeFile filePath (RemovedFile filePath)
            in
            Dialog2.openFile { defaultPath = Nothing, filters = [], title = Just "Choose file to delete" } (Err Cancelled) Ok
                |> Tauri.andThen ask
                |> Tauri.andThenDefinitely remove
                |> Tauri.bothResults
                |> toCmd identity

        RenameFile ->
            Dialog.openFile { defaultPath = Nothing, filters = [ { extensions = [ "txt", "elm", "md" ], name = "Texty Files" } ], title = Just "Pick a text file to rename" }
                |> Dialog.ifNotPickedOne "Didn't pick a file to rename"
                    (\fromFilePath ->
                        Dialog.save { defaultPath = Nothing, filters = [], title = Just "What should I rename it to?" }
                            |> Dialog.ifNotPickedOne "Didn't pick a new name"
                                (\toFilePath ->
                                    Dialog.askOptions
                                        ("Are you sure you want to rename \n" ++ fromFilePath ++ "\n to \n" ++ toFilePath ++ "\n?")
                                        { title = Just "Are you sure?", dialogType = Just Dialog.Warning }
                                        |> Dialog.ifNo "Cancelled rename"
                                            (FS.renameFile { from = fromFilePath, to = toFilePath }
                                                |> Task.map (always <| Ok { from = fromFilePath, to = toFilePath })
                                            )
                                )
                    )
                |> toCmd Renamed

        GetPath baseDir ->
            Path.get baseDir
                |> toCmd (GotPath baseDir)

        TestbaseDirectoryIsTotal ->
            let
                go baseDir =
                    TaskPort.call
                        { function = "BaseDirNo"
                        , valueDecoder = Json.Decode.int
                        , argsEncoder = BaseDir.encodeBaseDirectory
                        }
                        baseDir
            in
            toCmd GotNumbers <|
                Task.sequence <|
                    List.map go <|
                        List.concat
                            [ [ App, AppConfig, AppData, AppLocalData, AppLog ]
                            , [ Audio, Cache, BaseDir.Config, Data, Desktop, Document ]
                            , [ Download, Executable, Home, LocalData, Log, Picture ]
                            , [ Public, Resource, Runtime, Temp, Template, Video ]
                            ]

        ConfigMsg configMsg ->
            Config.updateFromDisk GotConfig configMsg

        GetModified ->
            Dialog.openFile { defaultPath = Nothing, filters = [], title = Just "Pick a file to see when it was last modified." }
                |> Dialog.ifPickedOne (Modified.getFileModifiedDateTime model.zone)
                |> toCmd GotModified

        WriteTextFileContaining string ->
            Dialog.save { defaultPath = Nothing, filters = [], title = Just "Save as" }
                |> Dialog.ifPickedOne
                    (\filePath ->
                        FS.writeTextFileIfDifferent { filePath = filePath, contents = string }
                            |> Task.map (\fileWas -> { filePath = filePath, fileWas = fileWas })
                    )
                |> toCmd WroteTextFile

        CheckRealFileExists ->
            Dialog2.openFile { defaultPath = Nothing, filters = [], title = Just "Pick an existing file" } (Err Cancelled) Ok
                |> Tauri.andThenDefinitely (\filePath -> FS2.exists filePath { yes = Existence True, no = Existence False })
                |> Tauri.bothResults
                |> toCmd identity

        CheckFakeFileExists ->
            Dialog2.save { defaultPath = Nothing, filters = [], title = Just "Pretend to create a file" } (Err Cancelled) Ok
                |> Tauri.andThenDefinitely (\filePath -> FS2.exists filePath { yes = Existence True, no = Existence False })
                |> Tauri.bothResults
                |> toCmd identity


cmdSucceed : msg -> Cmd msg
cmdSucceed msg =
    Task.succeed msg |> Task.perform identity


ignoreNothing : (a -> Msg) -> (Maybe a -> Msg)
ignoreNothing toMsg m =
    case m of
        Just a ->
            toMsg a

        Nothing ->
            IgnoreTauriFeedback


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

                Just (Tauri.FolderContents list) ->
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


showMaybeList : Maybe (List String) -> String
showMaybeList m =
    case m of
        Nothing ->
            "Nothing"

        Just list ->
            String.join "\n" list


showMaybe : Maybe String -> String
showMaybe m =
    Maybe.withDefault "Nothing" m
