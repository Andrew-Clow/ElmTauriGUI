module Tauri.Dialog exposing
    ( DialogType(..)
    , ask
    , askOptions
    , confirm
    , confirmOptions
    , message
    , messageOptions
    , openDirectories
    , openDirectory
    , openFile
    , openFiles
    , save
    )

{-
   Follow https://tauri.app/v1/guides/getting-started/prerequisites
   Follow https://tauri.app/v1/guides/getting-started/setup/
   elm install lobanov/elm-taskport
   Copy taskport.2.0.1.min.js into same folder as index.html
   In your index.html, before your elm app, you need whichever of these you're using:

     <script src="./taskport.2.0.1.min.js"></script>
     <script>
       TaskPort.install(); // can pass a settings object as a parameter, see https://elm.dmy.fr/packages/lobanov/elm-taskport/latest/
       TaskPort.register("readTextFile", (args) => {return window.__TAURI__.fs.readTextFile(args)});
       TaskPort.register("open", (args) => {return window.__TAURI__.dialog.open(args)});
       TaskPort.register("ask", (args) => {return window.__TAURI__.dialog.ask(args)});
       TaskPort.register("askOptions", (args) => {return window.__TAURI__.dialog.ask(args[0],{ title:args[1].title,type:args[1].type })});
       TaskPort.register("confirm", (args) => {return window.__TAURI__.dialog.confirm(args)});
       TaskPort.register("confirmOptions", (args) => {return window.__TAURI__.dialog.message(args[0],{ title:args[1].title,type:args[1].type })});
       TaskPort.register("message", (args) => {return window.__TAURI__.dialog.confirm(args)});
       TaskPort.register("messageOptions", (args) => {return window.__TAURI__.dialog.message(args[0],{ title:args[1].title,type:args[1].type })});
       TaskPort.register("openDlg", (args) => {return window.__TAURI__.dialog.open({defaultPath:args.defaultPath,directory:args.directory,filters:args.filters,multiple:args.multiple,recursive:args.recursive,title:args.title})});
       TaskPort.register("save", (args) => {return window.__TAURI__.dialog.save({defaultPath:args.defaultPath,directory:args.directory,filters:args.filters,multiple:args.multiple,recursive:args.recursive,title:args.title})});
     </script>



-}

import Json.Decode
import Json.Encode
import Task
import TaskPort



-- Messages and Questions ----------------------------------------------------------------------------------------------
--
-- ask (Yes/No), confirm (OK/Cancel), message (())


ask : String -> (TaskPort.Result { pressedYes : Bool } -> msg) -> Cmd msg
ask question toMsg =
    Task.attempt toMsg <|
        TaskPort.call
            { function = "ask"
            , valueDecoder = Json.Decode.bool |> Json.Decode.map (\bool -> { pressedYes = bool }) -- Yes or No
            , argsEncoder = Json.Encode.string
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


message : String -> (TaskPort.Result () -> msg) -> Cmd msg
message question toMsg =
    Task.attempt toMsg <|
        TaskPort.call
            { function = "message"
            , valueDecoder = Json.Decode.null ()
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


confirmOptions : String -> MessageDialogOptions -> (TaskPort.Result { pressedOK : Bool } -> msg) -> Cmd msg
confirmOptions question options toMsg =
    Task.attempt toMsg <|
        TaskPort.call
            { function = "confirmOptions"
            , valueDecoder = Json.Decode.bool |> Json.Decode.map (\bool -> { pressedOK = bool }) -- OK or Cancel
            , argsEncoder = encodeMessageDialogOptions options
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



-- Open ----------------------------------------------------------------------------------------------------------------
-- Files


type alias DialogFilter =
    { extensions : List String -- Extensions to filter, without a . prefix, eg ["svg", "png"]
    , name : String -- filter's name
    }


type alias FileDialogOptions =
    { defaultPath : Maybe String -- initial directory or file path
    , filters : List DialogFilter
    , title : Maybe String -- The title of the dialog window.
    }


openFile : FileDialogOptions -> (TaskPort.Result (Maybe String) -> msg) -> Cmd msg
openFile options toMsg =
    Task.attempt toMsg <|
        TaskPort.call
            { function = "openDlg"
            , valueDecoder = Json.Decode.maybe Json.Decode.string
            , argsEncoder = encodeFileDialogOptions { multiple = False }
            }
            options


openFiles : FileDialogOptions -> (TaskPort.Result (Maybe (List String)) -> msg) -> Cmd msg
openFiles options toMsg =
    Task.attempt toMsg <|
        TaskPort.call
            { function = "openDlg"
            , valueDecoder = Json.Decode.maybe <| Json.Decode.list Json.Decode.string
            , argsEncoder = encodeFileDialogOptions { multiple = True }
            }
            options



-- Directories


type alias DirectoryDialogOptions =
    { defaultPath : Maybe String -- initial directory
    , recursive : Bool -- If directory is true, indicates that it will be read recursively later. Defines whether subdirectories will be allowed on the scope or not.
    , title : Maybe String -- The title of the dialog window.
    }


openDirectory : DirectoryDialogOptions -> (TaskPort.Result (Maybe String) -> msg) -> Cmd msg
openDirectory options toMsg =
    Task.attempt toMsg <|
        TaskPort.call
            { function = "openDlg"
            , valueDecoder = Json.Decode.maybe Json.Decode.string
            , argsEncoder = encodeDirectoryDialogOptions { multiple = False }
            }
            options


openDirectories : DirectoryDialogOptions -> (TaskPort.Result (Maybe (List String)) -> msg) -> Cmd msg
openDirectories options toMsg =
    Task.attempt toMsg <|
        TaskPort.call
            { function = "openDlg"
            , valueDecoder = Json.Decode.maybe <| Json.Decode.list Json.Decode.string
            , argsEncoder = encodeDirectoryDialogOptions { multiple = True }
            }
            options



-- Save ----------------------------------------------------------------------------------------------------------------


save : FileDialogOptions -> (TaskPort.Result (Maybe String) -> msg) -> Cmd msg
save options toMsg =
    Task.attempt toMsg <|
        TaskPort.call
            { function = "save"
            , valueDecoder = Json.Decode.maybe Json.Decode.string
            , argsEncoder = encodeFileDialogOptions { multiple = False }
            }
            options



{-
   ------------------------------------------------------------------------------------------------------------------------

     Encoding

     Boring unexported bit where we encode all the arguments.

   ------------------------------------------------------------------------------------------------------------------------
-}


encodeNothingAsNull : (a -> Json.Encode.Value) -> Maybe a -> Json.Encode.Value
encodeNothingAsNull encoder maybe =
    case maybe of
        Nothing ->
            Json.Encode.null

        Just a ->
            encoder a



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

   type alias DirectoryDialogOptions =
       { defaultPath : Maybe String -- initial directory
       , recursive : Bool -- If directory is true, indicates that it will be read recursively later. Defines whether subdirectories will be allowed on the scope or not.
       , title : Maybe String -- The title of the dialog window.
       }

-}


encodeDirectoryDialogOptions : { multiple : Bool } -> DirectoryDialogOptions -> Json.Encode.Value
encodeDirectoryDialogOptions isMultiSelect options =
    Json.Encode.object
        [ ( "defaultPath", encodeNothingAsNull Json.Encode.string options.defaultPath )
        , ( "directory", Json.Encode.bool True )
        , ( "multiple", Json.Encode.bool <| .multiple <| isMultiSelect )
        , ( "recursive", Json.Encode.bool options.recursive )
        , ( "title", encodeNothingAsNull Json.Encode.string options.title )
        ]



{-
   type alias FileDialogOptions =
       { defaultPath : Maybe String -- initial directory or file path
       , filters : List DialogFilter
       , title : Maybe String -- The title of the dialog window.
       }
-}


encodeFileDialogOptions : { multiple : Bool } -> FileDialogOptions -> Json.Encode.Value
encodeFileDialogOptions isMultiSelect options =
    Json.Encode.object
        [ ( "defaultPath", encodeNothingAsNull Json.Encode.string options.defaultPath )
        , ( "directory", Json.Encode.bool False )
        , ( "filters", encodeFilters options.filters )
        , ( "multiple", Json.Encode.bool <| .multiple <| isMultiSelect )
        , ( "title", encodeNothingAsNull Json.Encode.string options.title )
        ]



{-
   type alias FileDialogOptions =
       { defaultPath : Maybe String -- initial directory or file path
       , filters : List DialogFilter
       , title : Maybe String -- The title of the dialog window.
       }
-}


encodeSaveDialogOptions : FileDialogOptions -> Json.Encode.Value
encodeSaveDialogOptions options =
    Json.Encode.object
        [ ( "defaultPath", encodeNothingAsNull Json.Encode.string options.defaultPath )
        , ( "filters", encodeFilters options.filters )
        , ( "title", encodeNothingAsNull Json.Encode.string options.title )
        ]



{-

   type alias DialogFilter =
       { extensions : List String -- Extensions to filter, without a . prefix, eg ["svg", "png"]
       , name : String -- filter's name
       }
-}


encodeFilters : List DialogFilter -> Json.Encode.Value
encodeFilters dfs =
    Json.Encode.list encodeFilter dfs


encodeFilter : DialogFilter -> Json.Encode.Value
encodeFilter df =
    Json.Encode.object
        [ ( "extensions", Json.Encode.list Json.Encode.string df.extensions )
        , ( "name", Json.Encode.string df.name )
        ]
