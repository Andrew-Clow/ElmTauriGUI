module Tauri.FS exposing (..)

import Json.Decode
import Json.Encode
import Task
import TaskPort


type alias FilePath =
    String


type alias FileContents =
    { filePath : FilePath
    , contents : String
    }


home : BaseDirectory
home =
    Home


appConfig : BaseDirectory
appConfig =
    AppConfig


type BaseDirectory
    = App
    | AppConfig
    | AppData
    | AppLocalData
    | AppLog
    | Audio
    | Cache
    | Config
    | Data
    | Desktop
    | Document
    | Download
    | Executable
    | Home
    | LocalData
    | Log
    | Picture
    | Public
    | Resource
    | Runtime
    | Temp
    | Template
    | Video



-- copyFile ------------------------------------------------------------------------------------------------------------


copyFile : { from : FilePath, to : FilePath } -> (TaskPort.Result () -> msg) -> Cmd msg
copyFile fromTo toMsg =
    TaskPort.call
        { function = "copyFile"
        , valueDecoder = Json.Decode.null ()
        , argsEncoder = \args -> Json.Encode.list Json.Encode.string [ args.from, args.to ]
        }
        fromTo
        |> Task.attempt toMsg


copyFileIn : { from : FilePath, to : FilePath } -> BaseDirectory -> (TaskPort.Result () -> msg) -> Cmd msg
copyFileIn fromTo baseDir toMsg =
    TaskPort.call
        { function = "copyFile"
        , valueDecoder = Json.Decode.null ()
        , argsEncoder =
            \args ->
                Json.Encode.list identity
                    [ Json.Encode.string args.from
                    , Json.Encode.string args.to
                    , Json.Encode.object [ ( "dir", encodeBaseDirectory baseDir ) ]
                    ]
        }
        fromTo
        |> Task.attempt toMsg



-- createDir -----------------------------------------------------------------------------------------------------------


createDir : FilePath -> (TaskPort.Result () -> msg) -> Cmd msg
createDir nameOfDirectory toMsg =
    TaskPort.call
        { function = "createDir"
        , valueDecoder = Json.Decode.null ()
        , argsEncoder = Json.Encode.string
        }
        nameOfDirectory
        |> Task.attempt toMsg


createDirIn : FilePath -> BaseDirectory -> { createParentsIfAbsent : Bool } -> (TaskPort.Result () -> msg) -> Cmd msg
createDirIn nameOfDirectory baseDir { createParentsIfAbsent } toMsg =
    TaskPort.call
        { function = "createDir"
        , valueDecoder = Json.Decode.null ()
        , argsEncoder = fsDirOptions baseDir { recursive = createParentsIfAbsent }
        }
        nameOfDirectory
        |> Task.attempt toMsg



-- exists --------------------------------------------------------------------------------------------------------------


exists : FilePath -> (TaskPort.Result Bool -> msg) -> Cmd msg
exists filePath toMsg =
    TaskPort.call
        { function = "exists"
        , valueDecoder = Json.Decode.bool
        , argsEncoder = Json.Encode.string
        }
        filePath
        |> Task.attempt toMsg


existsIn : FilePath -> BaseDirectory -> (TaskPort.Result Bool -> msg) -> Cmd msg
existsIn filePath baseDir toMsg =
    TaskPort.call
        { function = "exists"
        , valueDecoder = Json.Decode.bool
        , argsEncoder = fsOptions baseDir
        }
        filePath
        |> Task.attempt toMsg



-- readDir --------------------------------------------------------------------------------------------------------------


type alias FileEntry =
    { children : Maybe FileEntry
    , name : Maybe String
    , path : FilePath
    }



{-
   readDir : FilePath -> (Result TaskPort.Error (List FileEntry) -> msg) -> Cmd msg
   readDir filePath toMsg =
       TaskPort.call
           { function = "readDir"
           , valueDecoder = Json.Decode.list decodeFileEntry
           , argsEncoder = Json.Encode.string
           }
           filePath
           |> Task.attempt toMsg
-}
{-
   "readDir": true,
   "removeDir": true,
   "removeFile": true,
   "renameFile": true,
   "writeTextFile": true
-}
-- readTextFile --------------------------------------------------------------------------------------------------------


readTextFile : String -> (TaskPort.Result FileContents -> msg) -> Cmd msg
readTextFile filePath toMsg =
    TaskPort.call
        { function = "readTextFile"
        , valueDecoder = Json.Decode.string |> Json.Decode.map (\content -> { filePath = filePath, contents = content })
        , argsEncoder = Json.Encode.string
        }
        filePath
        |> Task.attempt toMsg



{-
   ------------------------------------------------------------------------------------------------------------------------

     Encoding / Decoding

     Boring unexported bit where we encode all the arguments.

   ------------------------------------------------------------------------------------------------------------------------
-}


fsOptions : BaseDirectory -> FilePath -> Json.Encode.Value
fsOptions baseDir filePath =
    Json.Encode.list identity
        [ Json.Encode.string filePath
        , Json.Encode.object [ ( "dir", encodeBaseDirectory baseDir ) ]
        ]


fsDirOptions : BaseDirectory -> { recursive : Bool } -> FilePath -> Json.Encode.Value
fsDirOptions baseDir { recursive } filePath =
    Json.Encode.list identity
        [ Json.Encode.string filePath
        , Json.Encode.object
            [ ( "dir", encodeBaseDirectory baseDir )
            , ( "recursive", Json.Encode.bool recursive )
            ]
        ]



--decodeFileEntry : Json.Decode.Decoder FileEntry
--decodeFileEntry = Json.Decode.


encodeBaseDirectory : BaseDirectory -> Json.Encode.Value
encodeBaseDirectory b =
    Json.Encode.string <|
        case b of
            App ->
                "App"

            AppConfig ->
                "AppConfig"

            AppData ->
                "AppData"

            AppLocalData ->
                "AppLocalData"

            AppLog ->
                "AppLog"

            Audio ->
                "Audio"

            Cache ->
                "Cache"

            Config ->
                "Config"

            Data ->
                "Data"

            Desktop ->
                "Desktop"

            Document ->
                "Document"

            Download ->
                "Download"

            Executable ->
                "Executable"

            Home ->
                "Home"

            LocalData ->
                "LocalData"

            Log ->
                "Log"

            Picture ->
                "Picture"

            Public ->
                "Public"

            Resource ->
                "Resource"

            Runtime ->
                "Runtime"

            Temp ->
                "Temp"

            Template ->
                "Template"

            Video ->
                "Video"
