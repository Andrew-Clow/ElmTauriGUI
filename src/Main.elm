module Main exposing (..)

import Browser
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Input
import Html
import TaskPort
import Tauri.Dialog


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    { it : String }


type Button
    = Ask
    | Confirm
    | Message
    | OpenDirectory


type Msg
    = Pressed Button
    | YesNo (TaskPort.Result { pressedYes : Bool })
    | OKCancel (TaskPort.Result { pressedOK : Bool })
    | GotMaybeString (TaskPort.Result (Maybe String))
    | IgnoreTauriFeedback



--  | GotFile (TaskPort.Result String)


init : flags -> ( Model, Cmd msg )
init =
    \_ -> ( { it = "" }, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Element.layout [ Element.padding 20 ] <|
        Element.column [ Element.spacing 10 ]
            [ Element.text "Hello!"
            , Element.row [ Element.spacing 10 ]
                [ button "Ask." <| Pressed Ask
                , button "Confirm" <| Pressed Confirm
                , button "Message" <| Pressed Message
                , button "Open" <| Pressed OpenDirectory
                ]
            , Element.text model.it
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Pressed btn ->
            ( { model | it = show btn }, press btn )

        YesNo result ->
            ( { model | it = boolResultToString result .pressedYes { true = "Yes", false = "No" } }, Cmd.none )

        OKCancel result ->
            ( { model | it = boolResultToString result .pressedOK { true = "OK", false = "Cancel" } }, Cmd.none )

        IgnoreTauriFeedback ->
            ( model, Cmd.none )

        GotMaybeString result ->
            let
                it =
                    resultToString result showMaybe
            in
            ( { model | it = it }, Cmd.none )



-- ( { model | it = "Yo." }, Tauri.FS.readTextFile "C:\\Users\\Andrew\\Dropbox\\NotWork\\prog\\elm\\ElmTauri\\elm.json" GotFile )
-- ( { model | it = "Yo." }, Tauri.FS.readTextFile "C:\\Users\\Andrew\\Dropbox\\NotWork\\stuff\\temp\\this\\hello.txt" GotFile )
--    ( { model | it = "Yo." }, Cmd.none )


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

        OpenDirectory ->
            "Open Directory"


press : Button -> Cmd Msg
press btn =
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

        OpenDirectory ->
            Tauri.Dialog.open { defaultPath = Nothing, directory = True, filters = Nothing, multiple = Tauri.Dialog.MultiSelectFalse, recursive = True, title = Just "pick a directory" } GotMaybeString


resultToString : TaskPort.Result a -> (a -> String) -> String
resultToString result toString =
    case result of
        Ok value ->
            toString value

        Err error ->
            TaskPort.errorToString error


boolResultToString : TaskPort.Result a -> (a -> Bool) -> { true : String, false : String } -> String
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
