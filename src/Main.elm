module Main exposing (..)

import Browser
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import Html
import Task
import TaskPort
import Tauri.Dialog
import Tauri.FS


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
    }


type Good
    = Good
    | Bad
    | Neutral


type Button
    = Ask
    | Confirm
    | Message
    | OpenDirectories
    | OpenFile
    | Save
    | ReadTextFile
    | CopyFile
    | ChooseToCreateDir
    | Exists


type Msg
    = Pressed Button
    | YesNo (TaskPort.Result { pressedYes : Bool })
    | OKCancel (TaskPort.Result { pressedOK : Bool })
    | GotMaybeString (TaskPort.Result (Maybe String))
    | GotMaybeStrings (TaskPort.Result (Maybe (List String)))
    | IgnoreTauriFeedback
    | GotFilePath (TaskPort.Result (Maybe String))
    | GotSaveFilePath (TaskPort.Result (Maybe String))
    | NoReadFileSpecified
    | NoSaveFileSpecified
    | GotFileContents (TaskPort.Result Tauri.FS.FileContents)
    | ConfirmCopy { from : FilePath, to : FilePath } (Result TaskPort.Error { pressedYes : Bool })
    | Copied { from : FilePath, to : FilePath }
    | TaskPortError String
    | CreateDir (TaskPort.Result (Maybe FilePath))
    | Created FilePath
    | Existence FilePath Bool


init : flags -> ( Model, Cmd msg )
init =
    \_ ->
        ( { answerWas = { text = "", good = Neutral }
          , readFilePath = Nothing
          , saveFilePath = Nothing
          }
        , Cmd.none
        )


view : Model -> Html.Html Msg
view model =
    Element.layout [ Element.padding 20 ] <|
        Element.column [ Element.spacing 25 ]
            [ Element.text "Hello! This is just to experiment with using Tauri with elm."
            , Element.column [ Element.spacing 7 ]
                [ Element.text "Dialog"
                , Element.row [ Element.spacing 10 ]
                    [ button "Ask." <| Pressed Ask
                    , button "Confirm" <| Pressed Confirm
                    , button "Message" <| Pressed Message
                    , button "Open Directories" <| Pressed OpenDirectories
                    , button "Open File" <| Pressed OpenFile
                    , button "Save" <| Pressed Save
                    ]
                , Element.text " "
                , Element.text "FS"
                , Element.row [ Element.spacing 10 ]
                    [ button "Read Text File" <| Pressed ReadTextFile
                    , button "Copy File" <| Pressed CopyFile
                    , button "Create Folder" <| Pressed ChooseToCreateDir
                    , button "Existence" <| Pressed Exists
                    ]
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
                ]
              <|
                Element.text model.answerWas.text
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Pressed btn ->
            ( { model | answerWas = { text = show btn, good = Neutral } }, press model btn )

        YesNo result ->
            ( { model | answerWas = boolResultToString result .pressedYes { true = "Yes", false = "No" } }, Cmd.none )

        OKCancel result ->
            ( { model | answerWas = boolResultToString result .pressedOK { true = "OK", false = "Cancel" } }, Cmd.none )

        IgnoreTauriFeedback ->
            ( model, Cmd.none )

        GotMaybeString result ->
            ( { model | answerWas = resultToString result showMaybe }, Cmd.none )

        GotMaybeStrings result ->
            ( { model | answerWas = resultToString result showMaybeList }, Cmd.none )

        GotFilePath result ->
            ( { model | answerWas = resultToString result showMaybe, readFilePath = pathFromResult result }, Cmd.none )

        GotSaveFilePath result ->
            ( { model | answerWas = resultToString result showMaybe, saveFilePath = pathFromResult result }, Cmd.none )

        NoReadFileSpecified ->
            ( { model | answerWas = { text = "No file specified", good = Bad } }
            , Tauri.Dialog.openFile
                { defaultPath = Nothing
                , filters = [ { extensions = [ "txt", "elm", "md" ], name = "Texty Files" } ]
                , title = Just "Please pick a text file"
                }
                GotFilePath
            )

        NoSaveFileSpecified ->
            ( { model | answerWas = { text = "No save destination specified", good = Bad } }
            , Tauri.Dialog.save
                { defaultPath = Nothing
                , filters = [ { extensions = [ "txt", "elm", "md" ], name = "Texty Files" } ]
                , title = Just "Please pick a text file to save to"
                }
                GotSaveFilePath
            )

        GotFileContents fileContents ->
            let
                showFileContents : Tauri.FS.FileContents -> String
                showFileContents { filePath, contents } =
                    filePath ++ "\n\n" ++ contents
            in
            ( { model | answerWas = resultToString fileContents showFileContents }, Cmd.none )

        ConfirmCopy record result ->
            case result of
                Ok value ->
                    if value.pressedYes then
                        ( { model | answerWas = { text = "Copying " ++ record.from ++ " to " ++ record.to, good = Neutral } }
                        , Tauri.FS.copyFile record (always <| Copied record)
                        )

                    else
                        ( { model | answerWas = { text = "Not copying " ++ record.from ++ " to " ++ record.to, good = Neutral } }, Cmd.none )

                Err error ->
                    ( { model | answerWas = { text = TaskPort.errorToString error, good = Bad } }, Cmd.none )

        Copied record ->
            ( { model | answerWas = { text = "Copied " ++ record.from ++ " to " ++ record.to, good = Good } }, Cmd.none )

        TaskPortError string ->
            ( { model | answerWas = { text = string, good = Bad } }, Cmd.none )

        CreateDir result ->
            case result of
                Ok (Just filePath) ->
                    ( { model | answerWas = { text = "Saving as " ++ filePath, good = Good } }
                    , Tauri.FS.createDirIn filePath Tauri.FS.home { createParentsIfAbsent = True } <|
                        splitMsg (TaskPort.errorToString >> TaskPortError) (\_ -> Created filePath)
                    )

                Ok Nothing ->
                    ( { model | answerWas = { text = "Save cancelled", good = Neutral } }, Cmd.none )

                Err err ->
                    ( { model | answerWas = { text = TaskPort.errorToString err, good = Bad } }, Cmd.none )

        Created filePath ->
            ( { model | answerWas = { text = filePath, good = Good } }, Cmd.none )

        Existence filePath bool ->
            ( { model | answerWas = { text = filePath ++ iff bool " exists." " doesn't exist.", good = Good } }
            , Cmd.none
            )


iff : Bool -> a -> a -> a
iff true x y =
    if true then
        x

    else
        y


pathFromResult : TaskPort.Result (Maybe FilePath) -> Maybe FilePath
pathFromResult r =
    case r of
        Ok (Just fp) ->
            Just fp

        _ ->
            Nothing


splitMsg : (TaskPort.Error -> msg) -> (a -> msg) -> Result TaskPort.Error a -> msg
splitMsg errToMsg toMsg r =
    case r of
        Ok value ->
            toMsg value

        Err error ->
            errToMsg error


replaceTaskPortResult : (String -> msg) -> msg -> Result TaskPort.Error a -> msg
replaceTaskPortResult errToMsg okMsg result =
    case result of
        Ok _ ->
            okMsg

        Err error ->
            errToMsg <| TaskPort.errorToString error


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


show : Button -> String
show btn =
    case btn of
        Ask ->
            "Ask"

        Confirm ->
            "Confirm"

        Message ->
            "Message"

        OpenDirectories ->
            "Open Directories"

        OpenFile ->
            "Open File"

        Save ->
            "Save"

        ReadTextFile ->
            "Read Text File"

        CopyFile ->
            "Copy File"

        ChooseToCreateDir ->
            "Create Folder"

        Exists ->
            "Exists"


press : Model -> Button -> Cmd Msg
press model btn =
    case btn of
        Confirm ->
            Tauri.Dialog.confirmOptions_WarningDoesNotActuallyGiveCancelOption "Is this really a confirmation question?"
                { title = Just "Confirm" -- defaults to the app name
                , dialogType = Just Tauri.Dialog.Warning -- called type at the typescript end. Defaults to Info
                }
                OKCancel

        Ask ->
            Tauri.Dialog.askOptions "Is this really a question?"
                { title = Nothing -- defaults to the app name
                , dialogType = Nothing -- called type at the typescript end. Defaults to Info
                }
                YesNo

        Message ->
            Tauri.Dialog.message "Here's a little message for you" <| always IgnoreTauriFeedback

        OpenDirectories ->
            Tauri.Dialog.openDirectories
                { defaultPath = Nothing
                , recursive = True
                , title = Just "Please pick a directory or directories"
                }
                GotMaybeStrings

        OpenFile ->
            Tauri.Dialog.openFile
                { defaultPath = Nothing
                , filters = [ { extensions = [ "txt", "elm", "md" ], name = "Texty Files" } ]
                , title = Just "Please pick a file"
                }
                GotFilePath

        Save ->
            Tauri.Dialog.save
                { defaultPath = Nothing
                , filters = [ { extensions = [ "txt", "elm", "md" ], name = "Texty Files" } ]
                , title = Just "What do you want me to save it as?"
                }
                GotSaveFilePath

        ReadTextFile ->
            case model.readFilePath of
                Nothing ->
                    Task.succeed NoReadFileSpecified |> Task.perform identity

                Just filePath ->
                    Tauri.FS.readTextFile filePath GotFileContents

        CopyFile ->
            case ( model.readFilePath, model.saveFilePath ) of
                ( Just from, Just to ) ->
                    Tauri.Dialog.askOptions
                        ("Are you sure you want to copy\n" ++ from ++ "\nto\n" ++ to ++ "\n?")
                        { title = Just "Are you sure?", dialogType = Just Tauri.Dialog.Warning }
                        (ConfirmCopy { from = from, to = to })

                ( Nothing, a ) ->
                    Task.succeed NoReadFileSpecified |> Task.perform identity

                ( Just _, Nothing ) ->
                    Task.succeed NoSaveFileSpecified |> Task.perform identity

        ChooseToCreateDir ->
            Tauri.Dialog.save { defaultPath = Nothing, filters = [], title = Just "Please enter the name of your new folder." } CreateDir

        Exists ->
            case model.saveFilePath of
                Just saveFilePath ->
                    Tauri.FS.exists saveFilePath (splitMsg (TaskPort.errorToString >> TaskPortError) (Existence saveFilePath))

                Nothing ->
                    case model.readFilePath of
                        Just filePath ->
                            Tauri.FS.exists filePath (splitMsg (TaskPort.errorToString >> TaskPortError) (Existence filePath))

                        Nothing ->
                            Task.succeed NoReadFileSpecified |> Task.perform identity


resultToString : TaskPort.Result a -> (a -> String) -> { text : String, good : Good }
resultToString result toString =
    case result of
        Ok value ->
            { text = toString value, good = Good }

        Err error ->
            { text = TaskPort.errorToString error, good = Bad }


boolResultToString : TaskPort.Result a -> (a -> Bool) -> { true : String, false : String } -> { text : String, good : Good }
boolResultToString result toBool { true, false } =
    let
        decide a =
            if toBool a then
                true

            else
                false
    in
    resultToString result decide


button : String -> Msg -> Element Msg
button string msg =
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
