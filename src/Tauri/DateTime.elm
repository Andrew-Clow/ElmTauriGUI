module Tauri.DateTime exposing (..)

import Time



-- Note that as usual you shouldn't store the strings in your model, store the Time.Posix.
-- Note that ISO 8601 is like "2022-12-31T23:59" but this is like "2022-12-31 23:59"


type alias DateTime =
    { year : Int, month : Int, day : Int, hour : Int, minute : Int }


posixToDateTime : Time.Zone -> Time.Posix -> DateTime
posixToDateTime zone time =
    let
        part function =
            function zone time
    in
    { year = part Time.toYear
    , month = part Time.toMonth |> toMonthNo
    , day = part Time.toDay
    , hour = part Time.toHour
    , minute = part Time.toMinute
    }


posixToString : Time.Zone -> Time.Posix -> String
posixToString zone time =
    posixToDateTime zone time |> dateTimeToString


dateTimeToString : DateTime -> String
dateTimeToString dateTime =
    let
        year =
            String.fromInt dateTime.year

        month =
            twoDigit dateTime.month

        day =
            twoDigit dateTime.day

        hour =
            twoDigit dateTime.hour

        minute =
            twoDigit dateTime.minute
    in
    year ++ "-" ++ month ++ "-" ++ day ++ " " ++ hour ++ ":" ++ minute


compare : DateTime -> DateTime -> Order
compare first second =
    let
        firstAnswer : List Order -> Order
        firstAnswer orders =
            case orders of
                [] ->
                    EQ

                LT :: _ ->
                    LT

                GT :: _ ->
                    GT

                EQ :: os ->
                    firstAnswer os

        cmpOn field =
            Basics.compare (field first) (field second)
    in
    firstAnswer <| List.map cmpOn [ .year, .month, .day, .hour, .minute ]


twoDigit : Int -> String
twoDigit int =
    let
        string =
            String.fromInt int
    in
    if String.length string == 1 then
        "0" ++ string

    else
        string


toMonthNo : Time.Month -> Int
toMonthNo month =
    case month of
        Time.Jan ->
            1

        Time.Feb ->
            2

        Time.Mar ->
            3

        Time.Apr ->
            4

        Time.May ->
            5

        Time.Jun ->
            6

        Time.Jul ->
            7

        Time.Aug ->
            8

        Time.Sep ->
            9

        Time.Oct ->
            10

        Time.Nov ->
            11

        Time.Dec ->
            12
