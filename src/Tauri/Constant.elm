module Tauri.Constant exposing (..)


type OK
    = OK


type CANCEL
    = CANCEL


trueOrFalseResult : true -> false -> Bool -> Result false true
trueOrFalseResult true false bool =
    if bool then
        Ok true

    else
        Err false


okOrCancel : Bool -> Result CANCEL OK
okOrCancel =
    trueOrFalseResult OK CANCEL


type YES
    = YES


type NO
    = NO


yesOrNo : Bool -> Result NO YES
yesOrNo =
    trueOrFalseResult YES NO


resultToBool : Result false true -> Bool
resultToBool result =
    case result of
        Ok _ ->
            True

        Err _ ->
            False
