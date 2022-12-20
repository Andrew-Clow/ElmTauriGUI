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
    , filePath : Maybe FilePath
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


type Msg
    = Pressed Button
    | YesNo (TaskPort.Result { pressedYes : Bool })
    | OKCancel (TaskPort.Result { pressedOK : Bool })
    | GotMaybeString (TaskPort.Result (Maybe String))
    | GotMaybeStrings (TaskPort.Result (Maybe (List String)))
    | IgnoreTauriFeedback
    | GotFilePath (TaskPort.Result (Maybe String))
    | NoFileSpecified
    | GotFileContents (TaskPort.Result Tauri.FS.FileContents)



--  | GotFile (TaskPort.Result String)


init : flags -> ( Model, Cmd msg )
init =
    \_ -> ( { answerWas = { text = "", good = Neutral }, filePath = Nothing }, Cmd.none )


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
                    ]
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
            let
                pathFromResult : TaskPort.Result (Maybe FilePath) -> Maybe FilePath
                pathFromResult r =
                    case r of
                        Ok (Just fp) ->
                            Just fp

                        _ ->
                            Nothing
            in
            ( { model | answerWas = resultToString result showMaybe, filePath = pathFromResult result }, Cmd.none )

        NoFileSpecified ->
            ( { model | answerWas = { text = "No file specified", good = Bad } }
            , Tauri.Dialog.openFile
                { defaultPath = Nothing
                , filters = [ { extensions = [ "txt", "elm", "md" ], name = "Texty Files" } ]
                , title = Just "Please pick a text file"
                }
                GotFilePath
            )

        GotFileContents fileContents ->
            let
                showFileContents : Tauri.FS.FileContents -> String
                showFileContents { filePath, contents } =
                    filePath ++ "\n\n" ++ contents
            in
            ( { model | answerWas = resultToString fileContents showFileContents }, Cmd.none )


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


press : Model -> Button -> Cmd Msg
press model btn =
    case btn of
        Confirm ->
            Tauri.Dialog.confirmOptions "Is this really a confirmation question?"
                { title = Just "Confirm" -- defaults to the app name
                , dialogType = Just Tauri.Dialog.Error -- called type at the typescript end. Defaults to Info
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
                GotMaybeString

        ReadTextFile ->
            case model.filePath of
                Nothing ->
                    Task.succeed NoFileSpecified |> Task.perform identity

                Just filePath ->
                    Tauri.FS.readTextFile filePath GotFileContents


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
