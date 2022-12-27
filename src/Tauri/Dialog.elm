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
    , warnCheckBefore
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
import Task exposing (Task)
import TaskPort



-- Messages and Questions ----------------------------------------------------------------------------------------------
--
-- ask (Yes/No), confirm (OK/Cancel), message (())


type alias MessageDialogOptions =
    { title : Maybe String -- defaults to the app name
    , dialogType : Maybe DialogType -- called type at the typescript end. Defaults to Info
    }


type DialogType
    = Info
    | Warning
    | Error


ask : String -> Task TaskPort.Error { pressedYes : Bool }
ask question =
    TaskPort.call
        { function = "ask"
        , valueDecoder = Json.Decode.bool |> Json.Decode.map (\bool -> { pressedYes = bool }) -- Yes or No
        , argsEncoder = Json.Encode.string
        }
        question


askOptions : String -> MessageDialogOptions -> Task TaskPort.Error { pressedYes : Bool }
askOptions question options =
    TaskPort.call
        { function = "askOptions"
        , valueDecoder = Json.Decode.bool |> Json.Decode.map (\bool -> { pressedYes = bool }) -- Yes or No
        , argsEncoder = encodeMessageDialogOptions options
        }
        question


warnCheckBefore :
    { title : Maybe String
    , question : String
    , wasCancelled : msg
    , errorMsg : TaskPort.Error -> err
    , taskIfYes : Task err msg
    }
    -> Task err msg
warnCheckBefore { title, question, wasCancelled, errorMsg, taskIfYes } =
    askOptions question { title = title, dialogType = Just Warning }
        |> Task.mapError errorMsg
        |> Task.andThen
            (\{ pressedYes } ->
                if pressedYes then
                    taskIfYes

                else
                    Task.succeed wasCancelled
            )


confirm : String -> Task TaskPort.Error { pressedOK : Bool }
confirm question =
    TaskPort.call
        { function = "confirm"
        , valueDecoder = Json.Decode.bool |> Json.Decode.map (\bool -> { pressedOK = bool }) -- OK or Cancel
        , argsEncoder = Json.Encode.string
        }
        question


confirmOptions : String -> MessageDialogOptions -> Task TaskPort.Error { pressedOK : Bool }
confirmOptions question options =
    TaskPort.call
        { function = "confirmOptions"
        , valueDecoder = Json.Decode.bool |> Json.Decode.map (\bool -> { pressedOK = bool }) -- OK or Cancel
        , argsEncoder = encodeMessageDialogOptions options
        }
        question


message : String -> Task TaskPort.Error ()
message question =
    TaskPort.call
        { function = "message"
        , valueDecoder = Json.Decode.succeed () -- should be Json.Decode.null (), but it's returning true for some reason.
        , argsEncoder = Json.Encode.string
        }
        question


messageOptions : String -> MessageDialogOptions -> Task TaskPort.Error ()
messageOptions question options =
    TaskPort.call
        { function = "messageOptions"
        , valueDecoder = Json.Decode.succeed () -- should be Json.Decode.null (), but it's returning true for some reason.
        , argsEncoder = encodeMessageDialogOptions options
        }
        question



-- Open ----------------------------------------------------------------------------------------------------------------
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


openFile : FileDialogOptions -> Task TaskPort.Error (Maybe String)
openFile options =
    TaskPort.call
        { function = "openDlg"
        , valueDecoder = Json.Decode.maybe Json.Decode.string
        , argsEncoder = encodeFileDialogOptions { multiple = False }
        }
        options


openFiles : FileDialogOptions -> Task TaskPort.Error (Maybe (List String))
openFiles options =
    TaskPort.call
        { function = "openDlg"
        , valueDecoder = Json.Decode.maybe <| Json.Decode.list Json.Decode.string
        , argsEncoder = encodeFileDialogOptions { multiple = True }
        }
        options



-- open Directories


type alias DirectoryDialogOptions =
    { defaultPath : Maybe String -- initial directory
    , recursive : Bool -- If directory is true, indicates that it will be read recursively later. Defines whether subdirectories will be allowed on the scope or not.
    , title : Maybe String -- The title of the dialog window.
    }


openDirectory : DirectoryDialogOptions -> Task TaskPort.Error (Maybe String)
openDirectory options =
    TaskPort.call
        { function = "openDlg"
        , valueDecoder = Json.Decode.maybe Json.Decode.string
        , argsEncoder = encodeDirectoryDialogOptions { multiple = False }
        }
        options


openDirectories : DirectoryDialogOptions -> Task TaskPort.Error (Maybe (List String))
openDirectories options =
    TaskPort.call
        { function = "openDlg"
        , valueDecoder = Json.Decode.maybe <| Json.Decode.list Json.Decode.string
        , argsEncoder = encodeDirectoryDialogOptions { multiple = True }
        }
        options



-- Save ----------------------------------------------------------------------------------------------------------------


save : FileDialogOptions -> Task TaskPort.Error (Maybe String)
save options =
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
