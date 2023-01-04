module Tauri.Dialog exposing (..)

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
import TaskPort exposing (Error)
import Tauri exposing (FilePath)



{- ---------------------------------------------------------------------------------------------------------------------

    Message and Question Dialogs

   ---------------------------------------------------------------------------------------------------------------------
-}
-- ask (Yes/No), confirm (OK/Cancel), message (OK)


type alias MessageDialogOptions =
    { title : Maybe String -- defaults to the app name
    , dialogType : InfoWarningOrError -- called type at the typescript end.
    }


type InfoWarningOrError
    = Info
    | Warning
    | Error


type TitleOrAppName
    = Title String
    | AppNameAsTitle



-- ask -----------------------------------------------------------------------------------------------------------------


ask : String -> { title : Maybe String, dialogType : InfoWarningOrError } -> { yes : answer, no : answer } -> Task Error answer
ask question options answers =
    TaskPort.call
        { function = "askOptions"
        , valueDecoder = Json.Decode.bool |> Json.Decode.map (Tauri.iff answers.yes answers.no)
        , argsEncoder = encodeMessageDialogOptions options
        }
        question



-- ask "do this thing" |> ifYES (this thing)
-- ask "do that thing" |> ifNo UserSaysNo (that thing)
-- ask "do another thing" |> ifNo "Computer says no." (another thing)
{- }
   ifYES : Task Error (Result msg intendedOutput) -> Task Error (Result NO YES) -> Task Error (Result msg intendedOutput)
   ifYES taskIfYES questionTask =
       questionTask
           |> andThen (always taskIfYES)

-}
-- confirm -------------------------------------------------------------------------------------------------------------


confirm : String -> { title : Maybe String, dialogType : InfoWarningOrError } -> { ok : answer, cancel : answer } -> Task Error answer
confirm question options { ok, cancel } =
    TaskPort.call
        { function = "confirmOptions"
        , valueDecoder = Json.Decode.bool |> Json.Decode.map (Tauri.iff ok cancel)
        , argsEncoder = encodeMessageDialogOptions options
        }
        question



{- }
   ifOK : Task Error (Result msg intendedOutput) -> Task Error (Result CANCEL OK) -> Task Error (Result msg intendedOutput)
   ifOK taskIfYes questionTask =
       questionTask |> and (always taskIfYes)

-}
-- message -------------------------------------------------------------------------------------------------------------


message : String -> { title : Maybe String, dialogType : InfoWarningOrError } -> answer -> Task Error answer
message question options ok =
    TaskPort.call
        { function = "messageOptions"
        , valueDecoder = Json.Decode.succeed ok -- should be Json.Decode.null (), but it's returning true for some reason.
        , argsEncoder = encodeMessageDialogOptions options
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


openFile : FileDialogOptions -> { cancelled : answer, chose : FilePath -> answer } -> Task Error answer
openFile options { cancelled, chose } =
    TaskPort.call
        { function = "openDlg"
        , valueDecoder = decodeMaybeString
        , argsEncoder = encodeFileDialogOptions { multiple = False }
        }
        options
        |> Tauri.maybeToMsg cancelled chose


openFiles : FileDialogOptions -> { cancelled : answer, chose : List FilePath -> answer } -> Task Error answer
openFiles options { cancelled, chose } =
    TaskPort.call
        { function = "openDlg"
        , valueDecoder = decodeMaybeStrings
        , argsEncoder = encodeFileDialogOptions { multiple = True }
        }
        options
        |> Tauri.maybeToMsg cancelled chose



-- open Directories


type alias DirectoryDialogOptions =
    { defaultPath : Maybe String -- initial directory
    , recursive : Bool -- If directory is true, indicates that it will be read recursively later. Defines whether subdirectories will be allowed on the scope or not.
    , title : Maybe String -- The title of the dialog window.
    }


openDirectory : DirectoryDialogOptions -> { cancelled : answer, chose : FilePath -> answer } -> Task Error answer
openDirectory options { cancelled, chose } =
    TaskPort.call
        { function = "openDlg"
        , valueDecoder = decodeMaybeString
        , argsEncoder = encodeDirectoryDialogOptions { multiple = False }
        }
        options
        |> Tauri.maybeToMsg cancelled chose


openDirectories : DirectoryDialogOptions -> { cancelled : answer, chose : List FilePath -> answer } -> Task Error answer
openDirectories options { cancelled, chose } =
    TaskPort.call
        { function = "openDlg"
        , valueDecoder = decodeMaybeStrings
        , argsEncoder = encodeDirectoryDialogOptions { multiple = True }
        }
        options
        |> Tauri.maybeToMsg cancelled chose



-- Save ----------------------------------------------------------------------------------------------------------------


save : FileDialogOptions -> { cancelled : answer, chose : FilePath -> answer } -> Task Error answer
save options { cancelled, chose } =
    TaskPort.call
        { function = "save"
        , valueDecoder = decodeMaybeString
        , argsEncoder = encodeFileDialogOptions { multiple = False }
        }
        options
        |> Tauri.maybeToMsg cancelled chose



-- ifPickedOne is to simplify handling the Maybe from picking a single file or directory.
-- eg:
-- save {defaultPath = Nothing, filters = [], title = Just "Save as.."}
--    |> ifPickedOne (\filename -> ....)


ifPickedOne : (s -> Task Error output) -> Task Error (Maybe s) -> Task Error (Maybe output)
ifPickedOne taskWithString pickTask =
    pickTask
        |> Task.andThen
            (\maybeS ->
                case maybeS of
                    Nothing ->
                        Task.succeed Nothing

                    Just s ->
                        Task.map Just <| taskWithString s
            )


ifNotPickedOne : nope -> (s -> Task Error (Result nope output)) -> Task Error (Maybe s) -> Task Error (Result nope output)
ifNotPickedOne nope taskWithString pickTask =
    pickTask
        |> Task.andThen
            (\maybeS ->
                case maybeS of
                    Nothing ->
                        Task.succeed <| Err nope

                    Just s ->
                        taskWithString s
            )


ifPickedSome : (s -> Task Error output) -> Task Error (Maybe (List s)) -> Task Error (Maybe (List output))
ifPickedSome taskWithString pickTask =
    pickTask
        |> Task.andThen
            (\maybeS ->
                case maybeS of
                    Nothing ->
                        Task.succeed Nothing

                    Just ss ->
                        Task.map Just <|
                            Task.sequence <|
                                List.map taskWithString ss
            )



-- Utilities -----------------------------------------------------------------------------------------------------------


onOk : (a -> msg) -> Task Error (Result msg a) -> Task Error msg
onOk toCmd task =
    let
        fromResult : Result msg a -> msg
        fromResult result =
            case result of
                Err msg ->
                    msg

                Ok a ->
                    toCmd a
    in
    Task.map fromResult task


fail : msg -> Task Error (Result msg Never)
fail msg =
    Task.succeed (Err msg)


onErr : msg -> Task Error (Result ignored value) -> Task Error (Result msg value)
onErr msg task =
    task |> Task.map (Result.mapError <| always msg)


andUse : (a -> errB -> msg) -> (a -> Task Error (Result errB okB)) -> Task Error (Result msg a) -> Task Error (Result msg okB)
andUse failureToMsg thenTask firstTask =
    let
        next firstResult =
            case firstResult of
                Err errMsg ->
                    Task.succeed (Err errMsg)

                Ok a ->
                    thenTask a
                        |> Task.map (Result.mapError <| failureToMsg a)
    in
    firstTask |> Task.andThen next


and : (errB -> msg) -> Task Error (Result errB okB) -> Task Error (Result msg ignored) -> Task Error (Result msg okB)
and failureToMsg thenTask firstTask =
    let
        next firstResult =
            case firstResult of
                Err firstErr ->
                    Task.succeed (Err firstErr)

                Ok _ ->
                    thenTask
                        |> Task.map (Result.mapError failureToMsg)
    in
    firstTask |> Task.andThen next


andThen : (a -> Task Error (Result msg b)) -> Task Error (Result msg a) -> Task Error (Result msg b)
andThen thenTask firstTask =
    let
        next result =
            case result of
                Err msg ->
                    Task.succeed (Err msg)

                Ok a ->
                    thenTask a
    in
    firstTask |> Task.andThen next


map : (a -> b) -> Task Error (Result msg a) -> Task Error (Result msg b)
map f =
    Task.map (Result.map f)


andMap : Task Error (Result msg a) -> Task Error (Result msg (a -> b)) -> Task Error (Result msg b)
andMap =
    map2 (|>)


map2Tee : (a -> Result msg b -> c) -> Task Error (Result msg a) -> (a -> Task Error (Result msg b)) -> Task Error (Result msg c)
map2Tee f firstTask secondTaskFunction =
    firstTask
        |> Task.andThen
            (\maybeA ->
                case maybeA of
                    Err msg ->
                        Task.succeed <| Err msg

                    Ok a ->
                        secondTaskFunction a
                            |> Task.map
                                (f a >> Ok)
            )


map2 : (a -> b -> c) -> Task Error (Result msg a) -> Task Error (Result msg b) -> Task Error (Result msg c)
map2 f =
    Task.map2 (Result.map2 f)


map3 : (a -> b -> c -> d) -> Task Error (Result msg a) -> Task Error (Result msg b) -> Task Error (Result msg c) -> Task Error (Result msg d)
map3 f =
    Task.map3 (Result.map3 f)


map4 : (a -> b -> c -> d -> e) -> Task Error (Result msg a) -> Task Error (Result msg b) -> Task Error (Result msg c) -> Task Error (Result msg d) -> Task Error (Result msg e)
map4 f =
    Task.map4 (Result.map4 f)


map5 : (a -> b -> c -> d -> e -> f) -> Task Error (Result msg a) -> Task Error (Result msg b) -> Task Error (Result msg c) -> Task Error (Result msg d) -> Task Error (Result msg e) -> Task Error (Result msg f)
map5 f =
    Task.map5 (Result.map5 f)



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
encodeMessageDialogOptions options msg =
    Json.Encode.list identity
        [ Json.Encode.string msg
        , Json.Encode.object
            [ ( "title", encodeNothingAsNull Json.Encode.string options.title )
            , ( "type", encodeDialogType options.dialogType )
            ]
        ]



{-
   type DialogType
       = Info
       | Warning
       | Error
-}


encodeDialogType : InfoWarningOrError -> Json.Encode.Value
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



-- Decoding Strings ------------------------------------------


decodeMaybeString : Json.Decode.Decoder (Maybe String)
decodeMaybeString =
    Json.Decode.maybe Json.Decode.string


decodeMaybeStrings : Json.Decode.Decoder (Maybe (List String))
decodeMaybeStrings =
    Json.Decode.maybe (Json.Decode.list Json.Decode.string)
