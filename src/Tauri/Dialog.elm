module Tauri.Dialog exposing (..)

import Json.Decode
import Json.Encode
import Task
import TaskPort


ask : String -> (TaskPort.Result { pressedYes : Bool } -> msg) -> Cmd msg
ask question toMsg =
    Task.attempt toMsg <|
        TaskPort.call
            { function = "ask"
            , valueDecoder = Json.Decode.bool |> Json.Decode.map (\bool -> { pressedYes = bool }) -- Yes or No
            , argsEncoder = Json.Encode.string
            }
            question


type alias MessageDialogOptions =
    { title : Maybe String -- defaults to the app name
    , dialogType : Maybe DialogType -- called type at the typescript end. Defaults to Info
    }


type DialogType
    = Info
    | Warning
    | Error


askOptions : String -> MessageDialogOptions -> (TaskPort.Result { pressedYes : Bool } -> msg) -> Cmd msg
askOptions question options toMsg =
    Task.attempt toMsg <|
        TaskPort.call
            { function = "askOptions"
            , valueDecoder = Json.Decode.bool |> Json.Decode.map (\bool -> { pressedYes = bool }) -- Yes or No
            , argsEncoder = encodeMessageDialogOptions options
            }
            question


confirm : String -> (TaskPort.Result { pressedOK : Bool } -> msg) -> Cmd msg
confirm question toMsg =
    Task.attempt toMsg <|
        TaskPort.call
            { function = "confirm"
            , valueDecoder = Json.Decode.bool |> Json.Decode.map (\bool -> { pressedOK = bool }) -- OK or Cancel
            , argsEncoder = Json.Encode.string
            }
            question


confirmOptions : String -> MessageDialogOptions -> (TaskPort.Result { pressedOK : Bool } -> msg) -> Cmd msg
confirmOptions question options toMsg =
    Task.attempt toMsg <|
        TaskPort.call
            { function = "confirmOptions"
            , valueDecoder = Json.Decode.bool |> Json.Decode.map (\bool -> { pressedOK = bool }) -- OK or Cancel
            , argsEncoder = encodeMessageDialogOptions options
            }
            question


message : String -> (TaskPort.Result () -> msg) -> Cmd msg
message question toMsg =
    Task.attempt toMsg <|
        TaskPort.call
            { function = "message"
            , valueDecoder = Json.Decode.null ()
            , argsEncoder = Json.Encode.string
            }
            question


messageOptions : String -> MessageDialogOptions -> (TaskPort.Result () -> msg) -> Cmd msg
messageOptions question options toMsg =
    Task.attempt toMsg <|
        TaskPort.call
            { function = "messageOptions"
            , valueDecoder = Json.Decode.null ()
            , argsEncoder = encodeMessageDialogOptions options
            }
            question


type alias OpenDialogOptions multiple =
    { defaultPath : Maybe String -- initial directory or file path
    , directory : Bool
    , filters : Maybe (List DialogFilter)
    , multiple : multiple -- Whether the dialog allows multiple selection or not.
    , recursive : Bool -- If directory is true, indicates that it will be read recursively later. Defines whether subdirectories will be allowed on the scope or not.
    , title : Maybe String -- The title of the dialog window.
    }


open : OpenDialogOptions SingleSelect -> (TaskPort.Result (Maybe String) -> msg) -> Cmd msg
open options toMsg =
    Task.attempt toMsg <|
        TaskPort.call
            { function = "openDlg"
            , valueDecoder = Json.Decode.maybe Json.Decode.string
            , argsEncoder = encodeOpenDialogOptions singleSelect
            }
            options


type alias DialogFilter =
    { extensions : List String -- Extensions to filter, without a . prefix, eg ["svg", "png"]
    , name : String -- filter's name
    }


type alias SaveDialogOptions =
    { defaultPath : Maybe String -- Initial directory or file path. If it's a directory path, the dialog interface will change to that folder. If it's not an existing directory, the file name will be set to the dialog's file name input and the dialog will be set to the parent folder.
    , filters : Maybe DialogFilter
    , title : Maybe String
    }


type FromElm boolTag filePathTag filePathsTag
    = Open { options : OpenDialogOptions SingleSelect } -- Open a file/directory selection dialog. The selected paths are added to the filesystem and asset protocol allowlist scopes. When security is more important than the easy of use of this API, prefer writing a dedicated command instead. Note that the allowlist scope change is not persisted, so the values are cleared when the application is restarted. You can save it to the filesystem using tauri-plugin-persisted-scope.
    | OpenMany { filePathsTag : filePathsTag, options : OpenDialogOptions MultiSelect } -- Open a file/directory selection dialog. The selected paths are added to the filesystem and asset protocol allowlist scopes. When security is more important than the easy of use of this API, prefer writing a dedicated command instead. Note that the allowlist scope change is not persisted, so the values are cleared when the application is restarted. You can save it to the filesystem using tauri-plugin-persisted-scope.
    | SaveAs { filePathTag : filePathTag, options : SaveDialogOptions } -- Open a file/directory save dialog. The selected path is added to the filesystem and asset protocol allowlist scopes. When security is more important than the easy of use of this API, prefer writing a dedicated command instead. Note that the allowlist scope change is not persisted, so the values are cleared when the application is restarted. You can save it to the filesystem using tauri-plugin-persisted-scope.


type MultiSelect
    = MultiSelectTrue


type SingleSelect
    = MultiSelectFalse



-- Encoding ------------------------------------------------------------------------------------------------------------
-- Encoding ------------------------------------------------------------------------------------------------------------
-- Encoding ------------------------------------------------------------------------------------------------------------
{-
   type alias MessageDialogOptions =
       { title : Maybe String -- defaults to the app name
       , dialogType : Maybe DialogType -- called type at the typescript end. Defaults to Info
       }
-}


encodeMessageDialogOptions : MessageDialogOptions -> String -> Json.Encode.Value
encodeMessageDialogOptions { title, dialogType } msg =
    Json.Encode.list identity
        [ Json.Encode.string msg
        , Json.Encode.object
            [ ( "title", encodeNothingAsNull Json.Encode.string title )
            , ( "type", encodeNothingAsNull encodeDialogType dialogType )
            ]
        ]


encodeNothingAsNull : (a -> Json.Encode.Value) -> Maybe a -> Json.Encode.Value
encodeNothingAsNull encoder maybe =
    case maybe of
        Nothing ->
            Json.Encode.null

        Just a ->
            encoder a



{-
   type DialogType
       = Info
       | Warning
       | Error
-}


encodeDialogType : DialogType -> Json.Encode.Value
encodeDialogType dialogType =
    Json.Encode.string <|
        case dialogType of
            Info ->
                "info"

            Warning ->
                "warning"

            Error ->
                "error"



{-
   type alias OpenDialogOptions multiple =
       { defaultPath : Maybe String -- initial directory or file path
       , directory : Bool
       , filters : Maybe (List DialogFilter)
       , multiple : multiple -- Whether the dialog allows multiple selection or not.
       , recursive : Bool -- If directory is true, indicates that it will be read recursively later. Defines whether subdirectories will be allowed on the scope or not.
       , title : Maybe String -- The title of the dialog window.
       }
-}


multiSelect : MultiSelect -> { multiple : Bool }
multiSelect _ =
    { multiple = True }


singleSelect : SingleSelect -> { multiple : Bool }
singleSelect _ =
    { multiple = False }


encodeOpenDialogOptions : (multiple -> { multiple : Bool }) -> OpenDialogOptions multiple -> Json.Encode.Value
encodeOpenDialogOptions isMultiSelect odo =
    Json.Encode.object
        [ ( "defaultPath", encodeNothingAsNull Json.Encode.string odo.defaultPath )
        , ( "directory", Json.Encode.bool odo.directory )
        , ( "filters", encodeNothingAsNull encodeFilters odo.filters )
        , ( "multiple", Json.Encode.bool <| .multiple <| isMultiSelect odo.multiple )
        , ( "recursive", Json.Encode.bool odo.recursive )
        , ( "title", encodeNothingAsNull Json.Encode.string odo.title )
        ]



{-

   type alias DialogFilter =
       { extensions : List String -- Extensions to filter, without a . prefix, eg ["svg", "png"]
       , name : String -- filter's name
       }
-}


encodeFilters : List DialogFilter -> Json.Encode.Value
encodeFilters dfs =
    let
        encodeFilter df =
            Json.Encode.object
                [ ( "extensions", Json.Encode.list Json.Encode.string df.extensions )
                , ( "name", Json.Encode.string df.name )
                ]
    in
    Json.Encode.list encodeFilter dfs
