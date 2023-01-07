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
import TaskPort exposing (Error)
import Tauri exposing (FileContents, FileEntry, FilePath, FileWas(..), FolderContents(..))
import Tauri.BaseDir exposing (BaseDir(..))
import Tauri.FS
import Tauri.TaskUtils as TaskUtils exposing (iff)



-- exists --------------------------------------------------------------------------------------------------------------


exists : BaseDir -> FilePath -> { yes : FilePath -> answer, no : FilePath -> answer } -> Task Error answer
exists baseDir filePath yesno =
    TaskPort.call
        { function = "existsOptions"
        , valueDecoder = Json.Decode.bool |> Json.Decode.map (iff (yesno.yes filePath) (yesno.no filePath))
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


writeTextFile : BaseDir -> { filePath : FilePath, contents : String } -> ok -> Task Error ok
writeTextFile baseDir fileContents ok =
    TaskPort.call
        { function = "writeTextFileOptions"
        , valueDecoder = Json.Decode.null ok
        , argsEncoder = encodeBaseDirAndRecord2 baseDir .filePath .contents
        }
        fileContents


writeTextFileIfDifferent : BaseDir -> { filePath : FilePath, contents : String } -> Task Error { filePath : FilePath, fileWas : FileWas }
writeTextFileIfDifferent baseDir fileContents =
    exists baseDir fileContents.filePath { yes = always True, no = always False }
        |> TaskUtils.boolTask
            { true =
                readTextFile baseDir fileContents.filePath
                    |> Task.andThen
                        (\currentContents ->
                            if fileContents.contents /= currentContents.contents then
                                writeTextFile baseDir fileContents { filePath = fileContents.filePath, fileWas = WasDifferent }

                            else
                                Task.succeed { filePath = fileContents.filePath, fileWas = WasSame }
                        )
            , false =
                writeTextFile baseDir fileContents { filePath = fileContents.filePath, fileWas = WasNew }
            }



-- renameFile ----------------------------------------------------------------------------------------------------------


renameFile : BaseDir -> { from : FilePath, to : FilePath } -> ok -> Task Error ok
renameFile baseDir fromTo ok =
    TaskPort.call
        { function = "renameFileOptions"
        , valueDecoder = Json.Decode.null ok
        , argsEncoder = encodeBaseDirAndRecord2 baseDir .from .to
        }
        fromTo



-- copyFile ------------------------------------------------------------------------------------------------------------


copyFile : BaseDir -> { from : FilePath, to : FilePath } -> ok -> Task Error ok
copyFile baseDir fromTo ok =
    TaskPort.call
        { function = "copyFileOptions"
        , valueDecoder = Json.Decode.null ok
        , argsEncoder = encodeBaseDirAndRecord2 baseDir .from .to
        }
        fromTo



-- removeFile ----------------------------------------------------------------------------------------------------------


removeFile : BaseDir -> FilePath -> ok -> Task Error ok
removeFile baseDir filePath ok =
    TaskPort.call
        { function = "removeFileOptions"
        , valueDecoder = Json.Decode.null ok
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


createDir : BaseDir -> { createParentsIfAbsent : Bool } -> FilePath -> ok -> Task Error ok
createDir baseDir { createParentsIfAbsent } nameOfDirectory ok =
    TaskPort.call
        { function = "createDirOptions"
        , valueDecoder = Json.Decode.null ok
        , argsEncoder = encodeFsDirOptions baseDir { recursive = createParentsIfAbsent }
        }
        nameOfDirectory



-- removeDir -----------------------------------------------------------------------------------------------------------


removeDir : BaseDir -> { removeSubdirectories : Bool } -> FilePath -> ok -> Task Error ok
removeDir baseDir { removeSubdirectories } filePath ok =
    TaskPort.call
        { function = "removeDirOptions"
        , valueDecoder = Json.Decode.null ok
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
