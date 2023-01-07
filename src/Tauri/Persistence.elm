module Tauri.Persistence exposing
    ( Persist
    , PersistenceError
    , PersistentData
    , default
    , get
    , init
    , initCmd
    , initCmdWithStringError
    , initWithStringError
    , modifyCurrentValue
    , persistenceErrorToString
    , toCmd1
    , toCmd2
    , updateFromCurrentValue
    )

-- Codec couples your encoder and decoder together so it's hard for them not to match up.

import Codec exposing (Codec)
import Json.Decode
import Task exposing (Task)
import TaskPort
import Tauri.BaseDir as BaseDir
import Tauri.FSInBaseDir as FSInBaseDir
import Tauri.TaskUtils as TaskUtils



{-
   Sources of truth in order of preference:
      1. The value of PersistentData pData in your model, which is saved to disk when you change it.
      2. The value stored on disk in AppConfig/persist.filename (eg load this on init)
      3. persist.default, which is then stored to disk
   The idea is that the only time you can supply a pData value directly is in persist.default,
   or via the persist.update function or modify Task.

-}


type alias FilePath =
    String



-- I would advocate importing all this in a Config.elm module or similar containing the Persist record,
-- and exporting customised versions for your Main.elm


type alias Persist pData pMsg =
    { default : pData
    , update : pMsg -> pData -> pData
    , filename : String -- relative, not absolute - will be stored in the AppConfig folder.
    , jsonCodec : Codec pData
    }


type PersistentData pData
    = PersistentData pData -- opaque so you have to use your update to change it.



-- default is to put as the first value in your init.


default : Persist pData pMsg -> PersistentData pData
default persist =
    PersistentData persist.default



-- keep the current PersistentData pData in your model and query it with this.


get : (pData -> a) -> PersistentData pData -> a
get field (PersistentData pData) =
    field pData



-- turn Tasks into Cmds whichever way you prefer:


toCmd1 : (Result String (PersistentData pData) -> msg) -> Task String (PersistentData pData) -> Cmd msg
toCmd1 toMsg task =
    task |> Task.attempt toMsg


toCmd2 : (String -> msg) -> (PersistentData pData -> msg) -> Task String (PersistentData pData) -> Cmd msg
toCmd2 tagError toMsg task =
    task
        |> Task.attempt
            (\result ->
                case result of
                    Ok p ->
                        toMsg p

                    Err e ->
                        tagError e
            )


modifyCurrentValue : Persist pData pMsg -> (pData -> pData) -> PersistentData pData -> Task String (PersistentData pData)
modifyCurrentValue persist change (PersistentData data) =
    write persist <| change data


updateFromCurrentValue : Persist pData pMsg -> pMsg -> PersistentData pData -> Task String (PersistentData pData)
updateFromCurrentValue persist msg (PersistentData data) =
    write persist <| persist.update msg data


initCmd : Persist pData pMsg -> (Result { default : PersistentData pData, error : PersistenceError } (PersistentData pData) -> msg) -> Cmd msg
initCmd persist toMsg =
    Task.attempt toMsg (init persist)


init : Persist pData pMsg -> Task { default : PersistentData pData, error : PersistenceError } (PersistentData pData)
init persist =
    checkExists persist.filename
        |> TaskUtils.boolTask
            { true = readOrDefault persist
            , false = writeDefaultCarefully persist
            }
        |> Task.mapError (\err -> { default = PersistentData persist.default, error = err })


initCmdWithStringError : Persist pData pMsg -> (Result { default : PersistentData pData, error : String } (PersistentData pData) -> msg) -> Cmd msg
initCmdWithStringError persist toMsg =
    Task.attempt toMsg (initWithStringError persist)


initWithStringError : Persist pData pMsg -> Task { default : PersistentData pData, error : String } (PersistentData pData)
initWithStringError persist =
    let
        newError : { default : PersistentData pData, error : PersistenceError } -> { default : PersistentData pData, error : String }
        newError oldError =
            { default = PersistentData persist.default, error = persistenceErrorToString persist oldError.error }
    in
    init persist |> Task.mapError newError


readOrDefault :
    Persist pData pMsg
    -> Task PersistenceError (PersistentData pData)
readOrDefault persist =
    readCarefully persist
        |> Task.map PersistentData


checkExists : FilePath -> Task PersistenceError Bool
checkExists filePath =
    FSInBaseDir.exists appConfig filePath { no = always False, yes = always True }
        |> Task.mapError CouldNotCheckExistence


readCarefully : Persist pData pMsg -> Task PersistenceError pData
readCarefully persist =
    let
        readPersistedFile : Task PersistenceError String
        readPersistedFile =
            FSInBaseDir.readTextFile appConfig persist.filename
                |> Task.mapError CouldNotReadFile
                |> Task.map .contents

        toJson : String -> Result PersistenceError pData
        toJson content =
            Json.Decode.decodeString (Codec.decoder persist.jsonCodec) content
                |> Result.mapError (IncorrectJson content)
    in
    checkExists persist.filename
        |> TaskUtils.boolTask { true = readPersistedFile, false = Task.fail FileDoesNotExist }
        |> Task.map toJson
        |> TaskUtils.combineErrToFail


ensureAppConfigDirExistsCarefully : Task PersistenceError ()
ensureAppConfigDirExistsCarefully =
    checkExists ""
        |> TaskUtils.boolTask
            { true = Task.succeed ()
            , false =
                FSInBaseDir.createDir appConfig { createParentsIfAbsent = True } "" ()
                    |> Task.mapError CouldNotCreateAppConfigFolder
            }


type PersistenceError
    = CouldNotCheckExistence TaskPort.Error
    | CouldNotCreateAppConfigFolder TaskPort.Error
    | FileDoesNotExist
    | CouldNotReadFile TaskPort.Error
    | IncorrectJson String Json.Decode.Error
    | CouldNotSaveDefaultConfig TaskPort.Error


persistenceErrorToString : Persist pData pMsg -> PersistenceError -> String
persistenceErrorToString persist err =
    case err of
        CouldNotCheckExistence error ->
            String.join "\n"
                [ "Sorry, I couldn't check the existence of"
                , persist.filename
                , ""
                , "I got this error message:"
                , TaskPort.errorToString error
                ]

        FileDoesNotExist ->
            String.join "\n"
                [ "Sorry, I needed this file to exist, but it doesn't seem to:"
                , persist.filename
                ]

        CouldNotReadFile error ->
            String.join "\n"
                [ "Sorry, I couldn't find the file"
                , persist.filename
                , ""
                , "I got this error message:"
                , ""
                , TaskPort.errorToString error
                ]

        IncorrectJson string error ->
            String.join "\n"
                [ "Sorry, although I found"
                , persist.filename
                , "the Json in it seemed wrong."
                , ""
                , "I got the error message below"
                , "and I've included the file contents at the bottom"
                , ""
                , Json.Decode.errorToString error
                , ""
                , string
                ]

        CouldNotCreateAppConfigFolder error ->
            String.join "\n"
                [ "Sorry, I couldn't find the folder that "
                , persist.filename
                , "is supposed to go in, so I tried to create it,"
                , "but I got this error message:"
                , ""
                , TaskPort.errorToString error
                ]

        CouldNotSaveDefaultConfig error ->
            String.join "\n"
                [ "Sorry, I couldn't find the file"
                , persist.filename
                , "and so I tried to create one with the default one,"
                , "but I got this error message:"
                , ""
                , TaskPort.errorToString error
                ]



-- Not exported as can break ways of working.
-- Not giving you read - The model is the source of truth, not the disk.


readDeprecated : Persist pData pMsg -> Task String (PersistentData pData)
readDeprecated persist =
    FSInBaseDir.readTextFile appConfig persist.filename
        |> Task.mapError (\e -> "Persistence.init error: " ++ TaskPort.errorToString e)
        |> Task.andThen
            (\fileContents ->
                case Codec.decodeString persist.jsonCodec fileContents.contents of
                    Ok value ->
                        Task.succeed (PersistentData value)

                    Err decodeErr ->
                        Task.fail <| "Persistence.init error: " ++ Json.Decode.errorToString decodeErr
            )



-- Not giving you writeDefault - it's called by init only if you don't have a value.


writeDefault : Persist pData pMsg -> Task String (PersistentData pData)
writeDefault persist =
    write persist persist.default


writeDefaultCarefully : Persist pData pMsg -> Task PersistenceError (PersistentData pData)
writeDefaultCarefully persist =
    FSInBaseDir.writeTextFile appConfig
        { filePath = persist.filename, contents = Codec.encodeToString 2 persist.jsonCodec persist.default }
        (PersistentData persist.default)
        |> Task.mapError CouldNotSaveDefaultConfig



-- Not giving you write - you could generate values outside of your model's PersistentData pData


write : Persist pData pMsg -> pData -> Task String (PersistentData pData)
write persist pData =
    FSInBaseDir.writeTextFile appConfig
        { filePath = persist.filename, contents = Codec.encodeToString 2 persist.jsonCodec pData }
        (PersistentData pData)
        |> Task.mapError (\e -> "Persistence.saveDefault error: " ++ TaskPort.errorToString e)



-- Not giving you ensureAppConfigDirExists - I'm pretty sure it's only needed when creating the file.


ensureAppConfigDirExists : Task String ()
ensureAppConfigDirExists =
    FSInBaseDir.exists appConfig "" { yes = always True, no = always False }
        |> Task.andThen
            (\existence ->
                if existence then
                    Task.succeed ()

                else
                    FSInBaseDir.createDir appConfig { createParentsIfAbsent = True } "" ()
            )
        |> Task.mapError TaskPort.errorToString



-- I think this is always the right decision for where to save application config data.


appConfig : BaseDir.BaseDir
appConfig =
    BaseDir.AppConfig
