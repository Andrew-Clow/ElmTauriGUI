module Main exposing (..)

import Browser
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Html
import Json.Decode
import Maybe.Extra
import Task exposing (Task)
import TaskPort
import Tauri
import Tauri.BaseDir as BaseDir exposing (BaseDir(..))
import Tauri.Dialog as Dialog
import Tauri.FS as FS
import Tauri.FSInBaseDir as FSInBaseDir
import Tauri.Path as Path


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
    , directory : Maybe FilePath
    , textFileContent : Maybe String
    , showPathButtons : Bool
    }


good : Model -> String -> Model
good model text =
    { model | answerWas = { text = text, good = Good } }


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
    | CheckExists
    | ChooseDir
    | ReadDir
    | RemoveDir
    | RemoveFile
    | RenameFile
    | WriteTextFile
    | GetPath BaseDir.BaseDir
    | TestbaseDirectoryIsTotal
    | NewCopyFile


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
    | GotFileContents FS.FileContents
    | ConfirmCopy { from : FilePath, to : FilePath } { pressedYes : Bool }
    | Copied { from : FilePath, to : FilePath }
    | CreateDir (Maybe FilePath)
    | Created FilePath
    | Existence FilePath Bool
    | Directory FilePath
    | NoFolderSpecified
    | Folder FS.FolderContents
    | ConfirmRemoveDir FilePath { pressedYes : Bool }
    | Removed FilePath
    | ConfirmRemoveFile FilePath { pressedYes : Bool }
    | ConfirmRename { from : FilePath, to : FilePath } { pressedYes : Bool }
    | Renamed { from : FilePath, to : FilePath }
    | EnableTextBox
    | WroteTextFile FilePath
    | EditedTextBox String
    | ToggleShowPathButtons
    | GotPath BaseDir.BaseDir FilePath
    | GotNumbers (List Int)


init : flags -> ( Model, Cmd msg )
init =
    \_ ->
        ( { answerWas = { text = "", good = Neutral }
          , readFilePath = Nothing
          , saveFilePath = Nothing
          , directory = Nothing
          , textFileContent = Nothing
          , showPathButtons = False
          }
        , Cmd.none
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
                    , button NewCopyFile
                    , button RemoveFile
                    , button RenameFile
                    , greenButton "WriteTextFile" <| writeTextFileLogic model
                    ]
                , Element.row [ Element.spacing 10 ]
                    [ button CheckExists
                    , button ChooseDir
                    , button ChooseToCreateDir
                    , button ReadDir
                    , button RemoveDir
                    ]
                , Element.text " "
                , Element.text "Path"
                , Element.row [ Element.spacing 10 ]
                    [ greenButton "Get Path..." ToggleShowPathButtons
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
                        , [ Audio, Cache, Config, Data, Desktop, Document ]
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
                    Element.Input.text []
                        { onChange = EditedTextBox
                        , text = content
                        , placeholder = Just (Element.Input.placeholder [] <| Element.text "Contents to write to text file.")
                        , label = Element.Input.labelLeft [] <| Element.text ""
                        }
            ]


writeTextFileLogic : Model -> Msg
writeTextFileLogic model =
    case model.textFileContent of
        Just _ ->
            Pressed WriteTextFile

        Nothing ->
            EnableTextBox


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

        CheckExists ->
            "Check Exists"

        ReadDir ->
            "Read Dir"

        ChooseDir ->
            "Choose Dir"

        RemoveDir ->
            "Remove Dir"

        RemoveFile ->
            "Remove File"

        RenameFile ->
            "Rename File"

        WriteTextFile ->
            "Write File"

        GetPath baseDir ->
            BaseDir.toString baseDir

        TestbaseDirectoryIsTotal ->
            "Temp Test"

        NewCopyFile ->
            "Copy File"


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
                showFileContents : FS.FileContents -> String
                showFileContents { filePath, contents } =
                    filePath ++ "\n\n" ++ contents
            in
            ( good model <| showFileContents fileContents, Cmd.none )

        ConfirmCopy record value ->
            if value.pressedYes then
                ( { model | answerWas = { text = "Copying " ++ record.from ++ " to " ++ record.to, good = Neutral } }
                , FS.copyFile record |> toCmd (always <| Copied record)
                )

            else
                ( { model | answerWas = { text = "Not copying " ++ record.from ++ " to " ++ record.to, good = Neutral } }, Cmd.none )

        Copied record ->
            ( { model | answerWas = { text = "Copied " ++ record.from ++ " to " ++ record.to, good = Good } }, Cmd.none )

        CreateDir result ->
            case result of
                Just filePath ->
                    ( { model | answerWas = { text = "Saving as " ++ filePath, good = Good } }
                    , FS.createDir filePath |> toCmd (\_ -> Created filePath)
                    )

                Nothing ->
                    ( { model | answerWas = { text = "Save cancelled", good = Neutral } }, Cmd.none )

        Created filePath ->
            ( { model | answerWas = { text = filePath, good = Good } }, Cmd.none )

        Existence filePath bool ->
            ( { model | answerWas = { text = filePath ++ iff bool " exists." " doesn't exist.", good = Good } }
            , Cmd.none
            )

        Directory filePath ->
            ( { model | directory = Just filePath, answerWas = { text = filePath, good = Good } }, Cmd.none )

        NoFolderSpecified ->
            ( { model | answerWas = { text = "No folder specified", good = Bad } }
            , Dialog.openDirectory
                { defaultPath = Nothing
                , recursive = False
                , title = Just "Please pick a folder"
                }
                |> toCmd (ignoreNothing Directory)
            )

        Folder folderContents ->
            ( { model | answerWas = { text = showFolderContents folderContents, good = Good } }, Cmd.none )

        InteropError interopError ->
            ( { model | answerWas = { text = "Interop Error:\n" ++ TaskPort.interopErrorToString interopError, good = Broken } }, Cmd.none )

        JSReturnError jSError ->
            ( { model | answerWas = { text = "Error Returned from Javascript:\n" ++ TaskPort.errorToString (TaskPort.JSError jSError), good = Bad } }, Cmd.none )

        ConfirmRemoveDir filePath value ->
            if value.pressedYes then
                ( { model | answerWas = { text = "Removing " ++ filePath, good = Neutral } }
                , FS.removeDir filePath |> toCmd (always <| Removed filePath)
                )

            else
                ( { model | answerWas = { text = "Not removing " ++ filePath, good = Neutral } }, Cmd.none )

        Removed filePath ->
            ( good model <| "Removed " ++ filePath, Cmd.none )

        ConfirmRemoveFile filePath value ->
            if value.pressedYes then
                ( { model | answerWas = { text = "Removing " ++ filePath, good = Neutral } }
                , FS.removeFile filePath |> toCmd (always <| Removed filePath)
                )

            else
                ( { model | answerWas = { text = "Not removing " ++ filePath, good = Neutral } }, Cmd.none )

        ConfirmRename record value ->
            if value.pressedYes then
                ( { model | answerWas = { text = "Rename " ++ record.from ++ " to " ++ record.to, good = Neutral } }
                , FS.renameFile record |> toCmd (always <| Renamed record)
                )

            else
                ( { model | answerWas = { text = "Not renaming " ++ record.from ++ " to " ++ record.to, good = Neutral } }, Cmd.none )

        Renamed record ->
            ( { model | answerWas = { text = "Renamed " ++ record.from ++ " to " ++ record.to, good = Good } }, Cmd.none )

        EnableTextBox ->
            ( { model | textFileContent = Just "" }, Cmd.none )

        WroteTextFile filePath ->
            ( good { model | textFileContent = Nothing } <| "Wrote text file " ++ filePath, Cmd.none )

        EditedTextBox string ->
            ( { model | textFileContent = Just string }, Cmd.none )

        ToggleShowPathButtons ->
            ( { model | showPathButtons = not model.showPathButtons }, Cmd.none )

        GotPath baseDir filePath ->
            ( good model <| BaseDir.toString baseDir ++ ":\n" ++ filePath, Cmd.none )

        GotNumbers ints ->
            ( good model <| String.join " " <| List.map String.fromInt ints, Cmd.none )


showFolderContents : FS.FolderContents -> String
showFolderContents (FS.FolderContents list) =
    "\n" ++ (String.join "\n" <| List.map showFile list) ++ "\n"


showFile : FS.FileEntry -> String
showFile { folderContents, name, path } =
    Maybe.withDefault "" name
        ++ "  "
        ++ path
        ++ "  "
        ++ (indent 4 <| Maybe.Extra.unwrap "" showFolderContents folderContents)


indent : Int -> String -> String
indent n string =
    let
        indentation =
            "\n" ++ String.repeat n " "
    in
    String.join indentation <| String.lines string


iff : Bool -> a -> a -> a
iff true x y =
    if true then
        x

    else
        y


toCmd : (a -> Msg) -> Task TaskPort.Error a -> Cmd Msg
toCmd =
    Tauri.toCmd3 InteropError JSReturnError


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
            case model.readFilePath of
                Nothing ->
                    cmdSucceed NoReadFileSpecified

                Just filePath ->
                    FS.readTextFile filePath
                        |> toCmd GotFileContents

        CopyFile ->
            case ( model.readFilePath, model.saveFilePath ) of
                ( Just from, Just to ) ->
                    Dialog.askOptions
                        ("Are you sure you want to copy\n" ++ from ++ "\nto\n" ++ to ++ "\n?")
                        { title = Just "Are you sure?", dialogType = Just Dialog.Warning }
                        |> toCmd (ConfirmCopy { from = from, to = to })

                ( Nothing, _ ) ->
                    cmdSucceed NoReadFileSpecified

                ( Just _, Nothing ) ->
                    cmdSucceed NoSaveFileSpecified

        ChooseToCreateDir ->
            Dialog.save { defaultPath = Nothing, filters = [], title = Just "Please enter the name of your new folder." } |> toCmd CreateDir

        CheckExists ->
            case model.saveFilePath of
                Just saveFilePath ->
                    FS.exists saveFilePath
                        |> toCmd (Existence saveFilePath)

                Nothing ->
                    case model.readFilePath of
                        Just filePath ->
                            FS.exists filePath
                                |> toCmd (Existence filePath)

                        Nothing ->
                            cmdSucceed NoReadFileSpecified

        ChooseDir ->
            Dialog.openDirectory { defaultPath = Nothing, recursive = False, title = Nothing }
                |> toCmd (ignoreNothing Directory)

        ReadDir ->
            case model.directory of
                Just filePath ->
                    FSInBaseDir.readDir Home { recursive = True } filePath |> toCmd Folder

                Nothing ->
                    cmdSucceed NoFolderSpecified

        RemoveDir ->
            case model.directory of
                Just filePath ->
                    Dialog.askOptions
                        ("Are you sure you want to remove" ++ filePath ++ "?")
                        { title = Just "Are you sure?", dialogType = Just Dialog.Warning }
                        |> toCmd (ConfirmRemoveDir filePath)

                Nothing ->
                    cmdSucceed NoFolderSpecified

        RemoveFile ->
            case model.readFilePath of
                Just filePath ->
                    Dialog.askOptions
                        ("Are you sure you want to remove" ++ filePath ++ "?")
                        { title = Just "Are you sure?", dialogType = Just Dialog.Warning }
                        |> toCmd (ConfirmRemoveFile filePath)

                Nothing ->
                    cmdSucceed NoReadFileSpecified

        RenameFile ->
            case ( model.readFilePath, model.saveFilePath ) of
                ( Just from, Just to ) ->
                    Dialog.askOptions
                        ("Are you sure you want to rename\n" ++ from ++ "\nto\n" ++ to ++ "\n?")
                        { title = Just "Are you sure?", dialogType = Just Dialog.Warning }
                        |> toCmd (ConfirmRename { from = from, to = to })

                ( Nothing, _ ) ->
                    cmdSucceed NoReadFileSpecified

                ( Just _, Nothing ) ->
                    cmdSucceed NoSaveFileSpecified

        WriteTextFile ->
            case model.textFileContent of
                Nothing ->
                    cmdSucceed EnableTextBox

                Just contents ->
                    case model.saveFilePath of
                        Just filePath ->
                            FS.writeTextFile { filePath = filePath, contents = contents }
                                |> toCmd (always <| WroteTextFile filePath)

                        Nothing ->
                            Dialog.save
                                { defaultPath = Nothing
                                , filters = [ { extensions = [ "txt", "elm", "md" ], name = "Texty Files" } ]
                                , title = Just "What do you want me to save it as?"
                                }
                                |> toCmd GotSaveFilePath

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
                            , [ Audio, Cache, Config, Data, Desktop, Document ]
                            , [ Download, Executable, Home, LocalData, Log, Picture ]
                            , [ Public, Resource, Runtime, Temp, Template, Video ]
                            ]

        NewCopyFile ->
            Dialog.openFile { defaultPath = Nothing, filters = [], title = Just "File to copy?" }
                |> Task.andThen
                    (Maybe.Extra.unwrap (Task.succeed IgnoreTauriFeedback)
                        -- succeed is a misnomer there
                        (\source ->
                            Dialog.save { defaultPath = Nothing, filters = [], title = Just "File to save?" }
                                |> Task.andThen
                                    (Maybe.Extra.unwrap (Task.succeed IgnoreTauriFeedback)
                                        -- succeed is a misnomer there too
                                        (\target ->
                                            FS.copyFile { from = source, to = target }
                                                |> Task.map (always <| Copied { from = source, to = target })
                                        )
                                    )
                        )
                    )
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
