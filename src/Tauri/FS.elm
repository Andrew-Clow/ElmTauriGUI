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


readTextFile : String -> (TaskPort.Result FileContents -> msg) -> Cmd msg
readTextFile filePath toMsg =
    TaskPort.call
        { function = "readTextFile"
        , valueDecoder = Json.Decode.string |> Json.Decode.map (\content -> { filePath = filePath, contents = content })
        , argsEncoder = Json.Encode.string
        }
        filePath
        |> Task.attempt toMsg
