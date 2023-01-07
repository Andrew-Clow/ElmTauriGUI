module Tauri.TaskUtils exposing (..)

import Maybe.Extra
import Result.Extra
import Task exposing (Task)



{- -- Utility functions ---------------------------------------------------------------------------------------------------

-}
-- Tracking errors as data in the return value, keeping them out of the fail


andThen : (a -> Task error (Result msg b)) -> Task error (Result msg a) -> Task error (Result msg b)
andThen nextTask firstTask =
    firstTask |> resultTask (Err >> Task.succeed) nextTask


andThenWithoutResult : (a -> Task error b) -> Task error (Result msg a) -> Task error (Result msg b)
andThenWithoutResult nextTask firstTask =
    firstTask |> resultTask (Err >> Task.succeed) (nextTask >> Task.map Ok)



-- Tracking errors from the result into the same fail type


errToFail : Result e a -> Task e a
errToFail result =
    case result of
        Err e ->
            Task.fail e

        Ok a ->
            Task.succeed a


andErrToFail : (b -> Task err c) -> Task err (Result err b) -> Task err c
andErrToFail next current =
    current
        |> Task.andThen errToFail
        |> Task.andThen next


combineErrToFail : Task err (Result err a) -> Task err a
combineErrToFail task =
    task
        |> Task.andThen errToFail



-- Different return types


alwaysTask : Task error b -> Task error a -> Task error b
alwaysTask second first =
    first |> Task.andThen (always second)


alwaysMsg : msg -> Task error ignored -> Task error msg
alwaysMsg msg task =
    task |> Task.map (always msg)



-- Maybe


maybeTask : Task error b -> (a -> Task error b) -> Task error (Maybe a) -> Task error b
maybeTask nothing just task =
    task |> Task.andThen (Maybe.Extra.unwrap nothing just)


maybeToMsg : b -> (a -> b) -> Task error (Maybe a) -> Task error b
maybeToMsg nothing just task =
    task |> Task.map (Maybe.Extra.unwrap nothing just)



-- Result


resultTask : (err -> Task error b) -> (a -> Task error b) -> Task error (Result err a) -> Task error b
resultTask err ok task =
    task |> Task.andThen (Result.Extra.unpack err ok)


resultToMsg : (err -> b) -> (a -> b) -> Task error (Result err a) -> Task error b
resultToMsg err ok task =
    task |> Task.map (Result.Extra.unpack err ok)


mapErr : (err -> newErr) -> Task error (Result err a) -> Task error (Result newErr a)
mapErr f task =
    task |> Task.map (Result.mapError f)


mapOk : (a -> b) -> Task error (Result err a) -> Task error (Result err b)
mapOk f task =
    task |> Task.map (Result.map f)


mapErrAndOk : (err -> newErr) -> (a -> b) -> Task error (Result err a) -> Task error (Result newErr b)
mapErrAndOk mapE mapO task =
    task |> Task.map (Result.mapError mapE >> Result.map mapO)


resultsAreMsgs : Task error (Result msg msg) -> Task error msg
resultsAreMsgs task =
    task |> Task.map (Result.Extra.unpack identity identity)



-- Bool


boolTask : { true : Task error b, false : Task error b } -> Task error Bool -> Task error b
boolTask thenTasks task =
    task |> Task.andThen (iff thenTasks.true thenTasks.false)


boolToMsg : { true : b, false : b } -> Task error Bool -> Task error b
boolToMsg msgs task =
    task |> Task.map (iff msgs.true msgs.false)


iff : a -> a -> Bool -> a
iff t f bool =
    if bool then
        t

    else
        f



--


resultsCombine : Result a a -> a
resultsCombine result =
    case result of
        Ok a ->
            a

        Err a ->
            a
