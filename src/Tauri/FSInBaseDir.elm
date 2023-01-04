module Tauri.FSInBaseDir exposing
    ( copyFile
    , createDir
    , exists
    , readDir
    , readTextFile
    , removeDir
    , removeFile
    , renameFile
    , writeTextFile
    , writeTextFileIfDifferent
    )

import Json.Decode
import Json.Encode
import Task exposing (Task)
import TaskPort
import Tauri exposing (FileContents, FileEntry, FilePath, FileWas(..), FolderContents(..))
import Tauri.BaseDir exposing (BaseDir(..))
import Tauri.FS



-- exists --------------------------------------------------------------------------------------------------------------


exists : BaseDir -> FilePath -> Task TaskPort.Error Bool
exists baseDir filePath =
    TaskPort.call
        { function = "existsOptions"
        , valueDecoder = Json.Decode.bool
        , argsEncoder = encodeBaseDirAndString baseDir
        }
        filePath



-- readTextFile --------------------------------------------------------------------------------------------------------


readTextFile : BaseDir -> FilePath -> Task TaskPort.Error FileContents
readTextFile baseDir filePath =
    TaskPort.call
        { function = "readTextFileOptions"
        , valueDecoder = Json.Decode.string |> Json.Decode.map (\content -> { filePath = filePath, contents = content })
        , argsEncoder = encodeBaseDirAndString baseDir
        }
        filePath



-- writeTextFile -------------------------------------------------------------------------------------------------------


writeTextFile : BaseDir -> { filePath : FilePath, contents : String } -> Task TaskPort.Error ()
writeTextFile baseDir fileContents =
    TaskPort.call
        { function = "writeTextFileOptions"
        , valueDecoder = Json.Decode.null ()
        , argsEncoder = encodeBaseDirAndRecord2 baseDir .filePath .contents
        }
        fileContents


writeTextFileIfDifferent : BaseDir -> { filePath : FilePath, contents : String } -> Task TaskPort.Error FileWas
writeTextFileIfDifferent baseDir fileContents =
    exists baseDir fileContents.filePath
        |> Task.andThen
            (\itAlreadyExists ->
                if itAlreadyExists then
                    readTextFile baseDir fileContents.filePath
                        |> Task.andThen
                            (\currentContents ->
                                if fileContents.contents /= currentContents.contents then
                                    writeTextFile baseDir fileContents
                                        |> Task.map (always WasDifferent)

                                else
                                    Task.succeed WasSame
                            )

                else
                    writeTextFile baseDir fileContents
                        |> Task.map (always WasNew)
            )



-- renameFile ----------------------------------------------------------------------------------------------------------


renameFile : BaseDir -> { from : FilePath, to : FilePath } -> Task TaskPort.Error ()
renameFile baseDir fromTo =
    TaskPort.call
        { function = "renameFileOptions"
        , valueDecoder = Json.Decode.null ()
        , argsEncoder = encodeBaseDirAndRecord2 baseDir .from .to
        }
        fromTo



-- copyFile ------------------------------------------------------------------------------------------------------------


copyFile : BaseDir -> { from : FilePath, to : FilePath } -> Task TaskPort.Error ()
copyFile baseDir fromTo =
    TaskPort.call
        { function = "copyFileOptions"
        , valueDecoder = Json.Decode.null ()
        , argsEncoder = encodeBaseDirAndRecord2 baseDir .from .to
        }
        fromTo



-- removeFile ----------------------------------------------------------------------------------------------------------


removeFile : BaseDir -> FilePath -> Task TaskPort.Error ()
removeFile baseDir filePath =
    TaskPort.call
        { function = "removeFileOptions"
        , valueDecoder = Json.Decode.null ()
        , argsEncoder = encodeBaseDirAndString baseDir
        }
        filePath



-- ---------------------------------------------------------------------------------------------------------------------
--
--   Directories
--
-- ---------------------------------------------------------------------------------------------------------------------


readDir : BaseDir -> { recursive : Bool } -> FilePath -> Task TaskPort.Error FolderContents
readDir baseDir { recursive } filePath =
    TaskPort.call
        { function = "readDirOptions"
        , valueDecoder = Json.Decode.map FolderContents <| Json.Decode.list Tauri.FS.decodeFileEntry
        , argsEncoder = encodeFsDirOptions baseDir { recursive = recursive }
        }
        filePath



-- createDir -----------------------------------------------------------------------------------------------------------


createDir : BaseDir -> { createParentsIfAbsent : Bool } -> FilePath -> Task TaskPort.Error ()
createDir baseDir { createParentsIfAbsent } nameOfDirectory =
    TaskPort.call
        { function = "createDirOptions"
        , valueDecoder = Json.Decode.null ()
        , argsEncoder = encodeFsDirOptions baseDir { recursive = createParentsIfAbsent }
        }
        nameOfDirectory



-- removeDir -----------------------------------------------------------------------------------------------------------


removeDir : BaseDir -> { removeSubdirectories : Bool } -> FilePath -> Task TaskPort.Error ()
removeDir baseDir { removeSubdirectories } filePath =
    TaskPort.call
        { function = "removeDirOptions"
        , valueDecoder = Json.Decode.null ()
        , argsEncoder = encodeFsDirOptions baseDir { recursive = removeSubdirectories }
        }
        filePath



{-
   ------------------------------------------------------------------------------------------------------------------------



     Encoding / Decoding

     Boring bit where we encode/decode all the arguments.



   ------------------------------------------------------------------------------------------------------------------------
-}


encodeBaseDir : BaseDir -> ( String, Json.Encode.Value )
encodeBaseDir baseDir =
    ( "dir", Tauri.BaseDir.encodeBaseDirectory baseDir )


encodeBaseDirAndString : BaseDir -> String -> Json.Encode.Value
encodeBaseDirAndString baseDir string =
    Json.Encode.list identity
        [ Json.Encode.string string
        , Json.Encode.object [ encodeBaseDir baseDir ]
        ]


encodeBaseDirAndRecord2 : BaseDir -> (r -> String) -> (r -> String) -> r -> Json.Encode.Value
encodeBaseDirAndRecord2 baseDir field1 field2 r =
    Json.Encode.list identity
        [ Json.Encode.string (field1 r)
        , Json.Encode.string (field2 r)
        , Json.Encode.object [ encodeBaseDir baseDir ]
        ]


encodeFsDirOptions : BaseDir -> { recursive : Bool } -> FilePath -> Json.Encode.Value
encodeFsDirOptions baseDir { recursive } filePath =
    Json.Encode.list identity
        [ Json.Encode.string filePath
        , Json.Encode.object
            [ encodeBaseDir baseDir
            , ( "recursive", Json.Encode.bool recursive )
            ]
        ]
