module Main.Config exposing (..)

import Codec exposing (Codec)
import Tauri.Persistence as Persistence exposing (Persist, PersistentData)


type Cheese
    = Hard String
    | Soft String


type alias Config =
    { cheesesPerPage : Int, cheeses : List Cheese }


type alias PersistentConfig =
    PersistentData Config


type ConfigMsg
    = ChangeCheesePerPageBy Int
    | AddCheese Cheese
    | RemoveCheese Cheese


configMsgToString : ConfigMsg -> String
configMsgToString cmsg =
    case cmsg of
        ChangeCheesePerPageBy int ->
            "Change Cheese per page by " ++ String.fromInt int

        AddCheese cheese ->
            "Add cheese: " ++ cheeseToString cheese

        RemoveCheese cheese ->
            "Remove cheese: " ++ cheeseToString cheese


persist : Persist Config ConfigMsg
persist =
    { default =
        { cheesesPerPage = 4
        , cheeses =
            [ Hard "Double Gloucester"
            , Soft "Camembert"
            , Soft "Aiket"
            ]
        }
    , update = updateConfig
    , filename = "config.json"
    , jsonCodec = configCodec
    }


default : PersistentConfig
default =
    Persistence.default persist


init : (Result String PersistentConfig -> msg) -> Cmd msg
init toMsg =
    Persistence.init persist |> Persistence.toCmd1 toMsg


updateFromCurrentValue : (Result String PersistentConfig -> msg) -> ConfigMsg -> PersistentConfig -> Cmd msg
updateFromCurrentValue toMsg msg persistentConfig =
    Persistence.updateFromCurrentValue persist msg persistentConfig
        |> Persistence.toCmd1 toMsg


updateFromDisk : (Result String PersistentConfig -> msg) -> ConfigMsg -> Cmd msg
updateFromDisk toMsg msg =
    Persistence.updateFromDisk persist msg |> Persistence.toCmd1 toMsg


getCheesesPerPage : PersistentConfig -> Int
getCheesesPerPage =
    Persistence.get .cheesesPerPage


getCheeses : PersistentConfig -> List Cheese
getCheeses =
    Persistence.get .cheeses


updateConfig : ConfigMsg -> Config -> Config
updateConfig msg config =
    case msg of
        ChangeCheesePerPageBy int ->
            { config | cheesesPerPage = config.cheesesPerPage + int }

        AddCheese cheese ->
            { config | cheeses = cheese :: config.cheeses }

        RemoveCheese cheese ->
            { config | cheeses = List.filter ((/=) cheese) config.cheeses }


cheeseCodec : Codec Cheese
cheeseCodec =
    Codec.custom
        (\hard soft value ->
            case value of
                Hard c ->
                    hard c

                Soft c ->
                    soft c
        )
        |> Codec.variant1 "Hard" Hard Codec.string
        |> Codec.variant1 "Soft" Soft Codec.string
        |> Codec.buildCustom


configCodec : Codec Config
configCodec =
    Codec.object (\cheesesPerPage cheeses -> { cheesesPerPage = cheesesPerPage, cheeses = cheeses })
        |> Codec.field "cheesesPerPage" .cheesesPerPage Codec.int
        |> Codec.field "cheeses" .cheeses (Codec.list cheeseCodec)
        |> Codec.buildObject


cheeseToString : Cheese -> String
cheeseToString c =
    case c of
        Hard string ->
            "Hard " ++ string

        Soft string ->
            "Soft " ++ string
