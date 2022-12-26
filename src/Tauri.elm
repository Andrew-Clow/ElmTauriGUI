module Tauri exposing (..)

import Task exposing (Task)
import TaskPort



{-
   For example, you might want to do something like

   import Tauri.FSInBaseDir exposing (FilePath)
   import Tauri.BaseDir

   readTextFile : FilePath -> (String -> Msg) -> (String -> Msg) -> Cmd Msg
   readTextFile filePath jsErrorTag toMsg =
        Tauri.FSInBaseDir.readTextFile Tauri.BaseDir.Home filePath
            |> Tauri.toCmd3 TauriTaskPortBorkedIt (Tauri.jsErrorToString >> jsErrorTag) toMsg

            -- TauriTaskPortBorkedIt is a general catch-all for interop, and shouldn't happen.
            -- We've put everything relative to the user's Home directory

   update model msg = case msg of
        ....
        Load file ->
            ( { model | doc = Loading }
            , readTextFile doc LoadError (Loaded file)
            )

            -- LoadError is a more specific tag for this filesystem operation.
            -- You may prefer to not turn it into a string and pattern match on it, so see the TaskPort docs,
            -- `type JSError` section in https://package.elm-lang.org/packages/lobanov/elm-taskport/latest/TaskPort#JSError

-}
-- Files


type alias FilePath =
    String


type alias FileContents =
    { filePath : FilePath
    , contents : String
    }



-- Folders


type alias FileEntry =
    { name : Maybe String
    , path : FilePath
    , folderContents : Maybe FolderContents
    }


type FolderContents
    = FolderContents (List FileEntry)



-- Turn your Tasks into Cmds:


toCmd1 : (Result TaskPort.Error a -> msg) -> Task TaskPort.Error a -> Cmd msg
toCmd1 toMsg task =
    task |> Task.attempt toMsg


toCommand1 : (Result String a -> msg) -> Task TaskPort.Error a -> Cmd msg
toCommand1 toMsg task =
    toCmd1 (Result.mapError errorToString >> toMsg) task



-- Tag your interop errors separately because interop errors shouldn't ever happen (fixable bugs)
-- and so you can use a catch-all message tag like
--
--    toCmd = Tauri.toCmd2 InteropError


toCmd2 : (TaskPort.InteropError -> msg) -> (Result TaskPort.JSError a -> msg) -> Task TaskPort.Error a -> Cmd msg
toCmd2 tagInteropError toMsg task =
    let
        toMessage : Result TaskPort.Error a -> msg
        toMessage result =
            case result of
                Ok value ->
                    toMsg <| Ok value

                Err error ->
                    case error of
                        TaskPort.InteropError interopError ->
                            tagInteropError interopError

                        TaskPort.JSError jSError ->
                            toMsg <| Err jSError
    in
    task |> Task.attempt toMessage


toCommand2 : (String -> msg) -> (Result String a -> msg) -> Task TaskPort.Error a -> Cmd msg
toCommand2 tagInteropError toMsg task =
    toCmd2 (interopErrorToString >> tagInteropError) (Result.mapError jsErrorToString >> toMsg) task



-- Tag your InteropErrors and JSErrors separately from your data.
-- You can use a catch-all message tag like
--
--    toCmd = Tauri.toCmd3 InteropError
--
-- but reduce case statements with
--
--     (model,readFile "this.txt" |> toCmd LoadError (Loaded "this.txt")


toCmd3 : (TaskPort.InteropError -> msg) -> (TaskPort.JSError -> msg) -> (a -> msg) -> Task TaskPort.Error a -> Cmd msg
toCmd3 tagInteropError tagJSError toMsg task =
    let
        toMessage : Result TaskPort.Error a -> msg
        toMessage result =
            case result of
                Ok value ->
                    toMsg value

                Err error ->
                    case error of
                        TaskPort.InteropError interopError ->
                            tagInteropError interopError

                        TaskPort.JSError jSError ->
                            tagJSError jSError
    in
    task |> Task.attempt toMessage


toCommand3 : (String -> msg) -> (String -> msg) -> (a -> msg) -> Task TaskPort.Error a -> Cmd msg
toCommand3 tagInteropError tagJSError toMsg task =
    toCmd3 (interopErrorToString >> tagInteropError) (jsErrorToString >> tagJSError) toMsg task



-- FileWas - for only writing files when they changed in writeFileIfDifferent - useful for keeping timestamps unchanged for ftp purposes.


type FileWas
    = WasDifferent
    | WasSame
    | WasAbsent


fileWasToString : FileWas -> String
fileWasToString fw =
    case fw of
        WasDifferent ->
            "was different so I saved the new version"

        WasSame ->
            "was the same so I didn't do anything"

        WasAbsent ->
            "was absent so I saved it"



-- Reexporting some TaskPort stuff:


jsErrorToString : TaskPort.JSError -> String
jsErrorToString err =
    TaskPort.JSError err |> TaskPort.errorToString


interopErrorToString : TaskPort.InteropError -> String
interopErrorToString =
    TaskPort.interopErrorToString


errorToString : TaskPort.Error -> String
errorToString =
    TaskPort.errorToString
