module Main exposing (..)

import Browser
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Font
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
    { it : { text : String, good : Good } }


type Good
    = Good
    | Bad
    | Neutral


type Button
    = Ask
    | Confirm
    | Message
    | OpenDirectory
    | OpenFiles
    | Save


type Msg
    = Pressed Button
    | YesNo (TaskPort.Result { pressedYes : Bool })
    | OKCancel (TaskPort.Result { pressedOK : Bool })
    | GotMaybeString (TaskPort.Result (Maybe String))
    | GotMaybeStrings (TaskPort.Result (Maybe (List String)))
    | IgnoreTauriFeedback



--  | GotFile (TaskPort.Result String)


init : flags -> ( Model, Cmd msg )
init =
    \_ -> ( { it = { text = "", good = Neutral } }, Cmd.none )


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
                    , button "Open Directory" <| Pressed OpenDirectory
                    , button "Open Files" <| Pressed OpenFiles
                    , button "Save" <| Pressed Save
                    ]
                ]
            , Element.el
                [ Element.Font.color <|
                    case model.it.good of
                        Good ->
                            Element.rgb255 125 208 125

                        Bad ->
                            Element.rgb255 210 120 142

                        Neutral ->
                            Element.rgb255 85 116 208
                ]
              <|
                Element.text model.it.text
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Pressed btn ->
            ( { model | it = { text = show btn, good = Neutral } }, press btn )

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

        GotMaybeStrings result ->
            let
                it =
                    resultToString result showMaybeList
            in
            ( { model | it = it }, Cmd.none )



-- ( { model | it = "Yo." }, Tauri.FS.readTextFile "C:\\Users\\Andrew\\Dropbox\\NotWork\\prog\\elm\\ElmTauri\\elm.json" GotFile )
-- ( { model | it = "Yo." }, Tauri.FS.readTextFile "C:\\Users\\Andrew\\Dropbox\\NotWork\\stuff\\temp\\this\\hello.txt" GotFile )
--    ( { model | it = "Yo." }, Cmd.none )


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

        OpenDirectory ->
            "Open Directory"

        OpenFiles ->
            "Open Files"

        Save ->
            "Save"


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
            Tauri.Dialog.openDirectory
                { defaultPath = Just "C:/Users/Andrew/Dropbox"
                , recursive = True
                , title = Just "Please pick a directory"
                }
                GotMaybeString

        OpenFiles ->
            Tauri.Dialog.openFiles
                { defaultPath = Just "C:/Users/Andrew/Dropbox"
                , filters = [ { extensions = [ "txt", "elm", "md" ], name = "Texty Files" } ]
                , title = Just "Please pick some files"
                }
                GotMaybeStrings

        Save ->
            Tauri.Dialog.save
                { defaultPath = Just "C:/Users/Andrew/Dropbox"
                , filters = [ { extensions = [ "txt", "elm", "md" ], name = "Texty Files" } ]
                , title = Just "What do you want me to save it as?"
                }
                GotMaybeString


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
