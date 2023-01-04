module Tauri.FS2 exposing
    ( copyFile
    , createDir
    , decodeFileEntry
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
import Tauri exposing (FileContents, FileEntry, FilePath, FileWas(..), FolderContents(..), iff)
import Tauri.Constant exposing (..)



-- ---------------------------------------------------------------------------------------------------------------------
--
--   Files
--
-- ---------------------------------------------------------------------------------------------------------------------
-- exists --------------------------------------------------------------------------------------------------------------


exists : FilePath -> { yes : FilePath -> answer, no : FilePath -> answer } -> Task Error answer
exists filePath yesno =
    TaskPort.call
        { function = "exists"
        , valueDecoder = Json.Decode.bool |> Json.Decode.map (iff (yesno.yes filePath) (yesno.no filePath))
        , argsEncoder = Json.Encode.string
        }
        filePath



-- readTextFile --------------------------------------------------------------------------------------------------------


readTextFile : FilePath -> Task Error FileContents
readTextFile filePath =
    TaskPort.call
        { function = "readTextFile"
        , valueDecoder = Json.Decode.string |> Json.Decode.map (\content -> { filePath = filePath, contents = content })
        , argsEncoder = Json.Encode.string
        }
        filePath



-- writeTextFile -------------------------------------------------------------------------------------------------------


writeTextFile : { filePath : FilePath, contents : String } -> ok -> Task Error ok
writeTextFile fileContents ok =
    TaskPort.call
        { function = "writeTextFile"
        , valueDecoder = Json.Decode.null ok
        , argsEncoder = encodeFileContents
        }
        fileContents


writeTextFileIfDifferent : { filePath : FilePath, contents : String } -> Task Error FileWas
writeTextFileIfDifferent fileContents =
    exists fileContents.filePath { yes = always True, no = always False }
        |> Tauri.boolTask
            { true =
                readTextFile fileContents.filePath
                    |> Task.andThen
                        (\currentContents ->
                            if fileContents.contents /= currentContents.contents then
                                writeTextFile fileContents WasDifferent

                            else
                                Task.succeed WasSame
                        )
            , false =
                writeTextFile fileContents WasNew
            }



-- renameFile ----------------------------------------------------------------------------------------------------------


renameFile : { from : FilePath, to : FilePath } -> ok -> Task Error ok
renameFile fromTo ok =
    TaskPort.call
        { function = "renameFile"
        , valueDecoder = Json.Decode.null ok
        , argsEncoder = encodeFromTo
        }
        fromTo



-- copyFile ------------------------------------------------------------------------------------------------------------


copyFile : { from : FilePath, to : FilePath } -> ok -> Task Error ok
copyFile fromTo ok =
    TaskPort.call
        { function = "copyFile"
        , valueDecoder = Json.Decode.null ok
        , argsEncoder = \args -> Json.Encode.list Json.Encode.string [ args.from, args.to ]
        }
        fromTo



-- removeFile ----------------------------------------------------------------------------------------------------------


removeFile : FilePath -> ok -> Task Error ok
removeFile filePath ok =
    TaskPort.call
        { function = "removeFile"
        , valueDecoder = Json.Decode.null ok
        , argsEncoder = Json.Encode.string
        }
        filePath



-- ---------------------------------------------------------------------------------------------------------------------
--
--   Directories
--
-- ---------------------------------------------------------------------------------------------------------------------
-- readDir --------------------------------------------------------------------------------------------------------------


readDir : FilePath -> Task Error FolderContents
readDir filePath =
    TaskPort.call
        { function = "readDir"
        , valueDecoder = Json.Decode.map FolderContents <| Json.Decode.list decodeFileEntry
        , argsEncoder = Json.Encode.string
        }
        filePath



-- createDir -----------------------------------------------------------------------------------------------------------


createDir : FilePath -> ok -> Task Error ok
createDir nameOfDirectory ok =
    TaskPort.call
        { function = "createDir"
        , valueDecoder = Json.Decode.null ok
        , argsEncoder = Json.Encode.string
        }
        nameOfDirectory



-- removeDir -----------------------------------------------------------------------------------------------------------


removeDir : FilePath -> ok -> Task Error ok
removeDir filePath ok =
    TaskPort.call
        { function = "removeDir"
        , valueDecoder = Json.Decode.null ok
        , argsEncoder = Json.Encode.string
        }
        filePath



{-
   ------------------------------------------------------------------------------------------------------------------------



     Encoding / Decoding

     Boring bit where we encode/decode all the arguments.



   ------------------------------------------------------------------------------------------------------------------------
-}


encodeFromTo : { from : FilePath, to : FilePath } -> Json.Encode.Value
encodeFromTo fromTo =
    Json.Encode.list Json.Encode.string [ fromTo.from, fromTo.to ]


encodeFileContents : { filePath : FilePath, contents : String } -> Json.Encode.Value
encodeFileContents fileContents =
    Json.Encode.list Json.Encode.string [ fileContents.filePath, fileContents.contents ]



{-

   type alias FileEntry =
       { children : Maybe Children
       , name : Maybe String
       , path : FilePath
       }


   type Children
       = Children (List FileEntry)



-}


decodeFileEntry : Json.Decode.Decoder FileEntry
decodeFileEntry =
    Json.Decode.map3 (\c n p -> { name = n, path = p, folderContents = c })
        (Json.Decode.maybe <| Json.Decode.field "children" decodeChildren)
        (Json.Decode.maybe <| Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "path" Json.Decode.string)


decodeChildren : Json.Decode.Decoder FolderContents
decodeChildren =
    Json.Decode.map FolderContents <| Json.Decode.list <| Json.Decode.lazy <| \_ -> decodeFileEntry



-- Debugging -----------------------------------------------------------------------------------------------------------
{-
   logValue : Json.Encode.Value -> Json.Encode.Value
   logValue a =
       Debug.log (Result.Extra.unpack identity identity <| Json.Print.prettyValue { indent = 2, columns = 120 } a) a


   debugDecode : Json.Decode.Decoder a -> Json.Decode.Decoder a
   debugDecode decoder =
       Json.Decode.value
           |> Json.Decode.andThen
               (\value ->
                   Debug.log (Debug.toString value)
                       (case Json.Decode.decodeValue decoder value of
                           Ok y ->
                               Json.Decode.succeed y

                           Err n ->
                               Json.Decode.fail <| Json.Decode.errorToString n
                       )
               )

-}
