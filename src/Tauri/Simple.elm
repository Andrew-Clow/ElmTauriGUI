module Tauri.Simple exposing (..)

import Json.Decode
import Json.Encode
import Task exposing (Task)
import TaskPort
import Tauri.Dialog exposing (InfoWarningOrError)
import Tauri.TaskUtils


type FilePath
    = String


type alias Title =
    String


stringErrors : (Result String a -> msg) -> (Result TaskPort.Error a -> msg)
stringErrors toMsg =
    Result.mapError TaskPort.errorToString >> toMsg



{-
   Dialogs

   Example:
       type Msg = LaunchMissiles (Result String Bool)

       ( model, Tauri.Simple.askDialog
           "Shall we launch the missiles?"
           (Just "Are you sure?")
           Warning
           LaunchMissiles
       )

-}
-- ask -----------------------------------------------------------------------------------------------------------------


askDialog : String -> Maybe Title -> InfoWarningOrError -> (Result TaskPort.Error Bool -> msg) -> Cmd msg
askDialog question title infoWarningOrError toMsg =
    TaskPort.call
        { function = "askOptions"
        , valueDecoder = Json.Decode.bool
        , argsEncoder = encodeMessageDialogOptions title infoWarningOrError
        }
        question
        |> Task.attempt toMsg


encodeMessageDialogOptions : Maybe Title -> InfoWarningOrError -> String -> Json.Encode.Value
encodeMessageDialogOptions title infoWarningOrError msg =
    Json.Encode.list identity
        [ Json.Encode.string msg
        , Json.Encode.object
            [ ( "title", Tauri.Dialog.encodeNothingAsNull Json.Encode.string title )
            , ( "type", Tauri.Dialog.encodeDialogType infoWarningOrError )
            ]
        ]



-- confirm -------------------------------------------------------------------------------------------------------------


confirm : String -> Maybe Title -> InfoWarningOrError -> (Result TaskPort.Error Bool -> msg) -> Cmd msg
confirm question options { ok, cancel } =
    TaskPort.call
        { function = "confirmOptions"
        , valueDecoder = Json.Decode.bool |> Json.Decode.map (Tauri.TaskUtils.iff ok cancel)
        , argsEncoder = Tauri.Dialog.encodeMessageDialogOptions options
        }
        question



-- message -------------------------------------------------------------------------------------------------------------


message : String -> { title : Maybe String, dialogType : InfoWarningOrError } -> answer -> Task TaskPort.Error answer
message question options ok =
    TaskPort.call
        { function = "messageOptions"
        , valueDecoder = Json.Decode.succeed ok -- should be Json.Decode.null (), but it's returning true for some reason.
        , argsEncoder = Tauri.Dialog.encodeMessageDialogOptions options
        }
        question



{- ---------------------------------------------------------------------------------------------------------------------

    Open Dialogs

   ---------------------------------------------------------------------------------------------------------------------
-}
-- open Files


type alias DialogFilter =
    { extensions : List String -- Extensions to filter, without a . prefix, eg ["svg", "png"]
    , name : String -- filter's name
    }


type alias FileDialogOptions =
    { defaultPath : Maybe String -- initial directory or file path
    , filters : List DialogFilter
    , title : Maybe String -- The title of the dialog window.
    }


openFile : FileDialogOptions -> { cancelled : answer, chose : FilePath -> answer } -> Task TaskPort.Error answer
openFile options { cancelled, chose } =
    TaskPort.call
        { function = "openDlg"
        , valueDecoder = Tauri.Dialog.decodeMaybeString
        , argsEncoder = Tauri.Dialog.encodeFileDialogOptions { multiple = False }
        }
        options
        |> Tauri.TaskUtils.maybeToMsg cancelled chose


openFiles : FileDialogOptions -> { cancelled : answer, chose : List FilePath -> answer } -> Task TaskPort.Error answer
openFiles options { cancelled, chose } =
    TaskPort.call
        { function = "openDlg"
        , valueDecoder = Tauri.Dialog.decodeMaybeStrings
        , argsEncoder = Tauri.Dialog.encodeFileDialogOptions { multiple = True }
        }
        options
        |> Tauri.TaskUtils.maybeToMsg cancelled chose



-- open Directories


type alias DirectoryDialogOptions =
    { defaultPath : Maybe String -- initial directory
    , recursive : Bool -- If directory is true, indicates that it will be read recursively later. Defines whether subdirectories will be allowed on the scope or not.
    , title : Maybe String -- The title of the dialog window.
    }


openDirectory : DirectoryDialogOptions -> { cancelled : answer, chose : FilePath -> answer } -> Task TaskPort.Error answer
openDirectory options { cancelled, chose } =
    TaskPort.call
        { function = "openDlg"
        , valueDecoder = Tauri.Dialog.decodeMaybeString
        , argsEncoder = Tauri.Dialog.encodeDirectoryDialogOptions { multiple = False }
        }
        options
        |> Tauri.TaskUtils.maybeToMsg cancelled chose


openDirectories : DirectoryDialogOptions -> { cancelled : answer, chose : List FilePath -> answer } -> Task TaskPort.Error answer
openDirectories options { cancelled, chose } =
    TaskPort.call
        { function = "openDlg"
        , valueDecoder = Tauri.Dialog.decodeMaybeStrings
        , argsEncoder = Tauri.Dialog.encodeDirectoryDialogOptions { multiple = True }
        }
        options
        |> Tauri.TaskUtils.maybeToMsg cancelled chose



-- Save ----------------------------------------------------------------------------------------------------------------


save : FileDialogOptions -> { cancelled : answer, chose : FilePath -> answer } -> Task TaskPort.Error answer
save options { cancelled, chose } =
    TaskPort.call
        { function = "save"
        , valueDecoder = Tauri.Dialog.decodeMaybeString
        , argsEncoder = Tauri.Dialog.encodeFileDialogOptions { multiple = False }
        }
        options
        |> Tauri.TaskUtils.maybeToMsg cancelled chose
