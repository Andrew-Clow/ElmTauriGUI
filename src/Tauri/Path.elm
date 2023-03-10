module Tauri.Path exposing (..)

import Json.Decode
import Json.Encode
import Task exposing (Task)
import TaskPort
import Tauri exposing (FilePath)
import Tauri.BaseDir exposing (BaseDir(..))


get : BaseDir -> Task TaskPort.Error FilePath
get baseDir =
    TaskPort.call
        { function = Tauri.BaseDir.toFunctionName baseDir
        , valueDecoder = Json.Decode.string
        , argsEncoder = always Json.Encode.null
        }
        ()



-- For your convenience, I have provided a slightly hacky filename from filepath function, so you don't have to call out to Tauri.
-- It replaces the windows \ with the posix / and then splits on that delimiter.
-- In my mind this is justified in that / is an invalid character in a filename in windows,
-- so it ought not to be there anyway so we can't break a filename inappropriately by additionally breaking on / in windows.
--
-- filePathToFileName "home/this\\that/theother/\\filename.txt" == "filename.txt"


toFileName : FilePath -> String
toFileName filePath =
    String.replace "\\" "/" filePath
        |> String.split "/"
        |> List.reverse
        |> List.head
        |> Maybe.withDefault ""



{-

   type BaseDirectory
      = App | AppConfig | AppData | AppLocalData | AppLog | Audio | Cache | Config | Data | Desktop | Document
      | Download | Executable | Home | LocalData | Log | Picture | Public | Resource | Runtime | Temp | Template | Video

-}


appDir : Task TaskPort.Error FilePath
appDir =
    get App


appConfigDir : Task TaskPort.Error FilePath
appConfigDir =
    get AppConfig


appDataDir : Task TaskPort.Error FilePath
appDataDir =
    get AppData


appLocalDataDir : Task TaskPort.Error FilePath
appLocalDataDir =
    get AppLocalData


appLogDir : Task TaskPort.Error FilePath
appLogDir =
    get AppLog


audioDir : Task TaskPort.Error FilePath
audioDir =
    get Audio


cacheDir : Task TaskPort.Error FilePath
cacheDir =
    get Cache


configDir : Task TaskPort.Error FilePath
configDir =
    get Config


dataDir : Task TaskPort.Error FilePath
dataDir =
    get Data


desktopDir : Task TaskPort.Error FilePath
desktopDir =
    get Desktop


documentDir : Task TaskPort.Error FilePath
documentDir =
    get Document


downloadDir : Task TaskPort.Error FilePath
downloadDir =
    get Download


executableDir : Task TaskPort.Error FilePath
executableDir =
    get Executable


homeDir : Task TaskPort.Error FilePath
homeDir =
    get Home


localDataDir : Task TaskPort.Error FilePath
localDataDir =
    get LocalData


logDir : Task TaskPort.Error FilePath
logDir =
    get Log


pictureDir : Task TaskPort.Error FilePath
pictureDir =
    get Picture


publicDir : Task TaskPort.Error FilePath
publicDir =
    get Public


resourceDir : Task TaskPort.Error FilePath
resourceDir =
    get Resource


runtimeDir : Task TaskPort.Error FilePath
runtimeDir =
    get Runtime


tempDir : Task TaskPort.Error FilePath
tempDir =
    get Temp


templateDir : Task TaskPort.Error FilePath
templateDir =
    get Template


videoDir : Task TaskPort.Error FilePath
videoDir =
    get Video
