module Tauri.Persistence exposing
    ( Persist
    , PersistentData
    , default
    , get
    , init
    , modifyCurrentValue
    , toCmd1
    , toCmd2
    , updateFromCurrentValue
    , updateFromDisk
    )

-- Codec couples your encoder and decoder together so it's hard for them not to match up.

import Codec exposing (Codec)
import Json.Decode
import Task exposing (Task)
import TaskPort
import Tauri.BaseDir as BaseDir
import Tauri.FSInBaseDir as FS



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


init : Persist pData pMsg -> Task String (PersistentData pData)
init persist =
    FS.exists appConfig persist.filename
        |> Task.mapError (\e -> "Persistence.init error: " ++ TaskPort.errorToString e)
        |> Task.andThen
            (\exists ->
                if exists then
                    read persist

                else
                    ensureAppConfigDirExists
                        |> Task.andThen (\_ -> writeDefault persist)
            )


modifyCurrentValue : Persist pData pMsg -> (pData -> pData) -> PersistentData pData -> Task String (PersistentData pData)
modifyCurrentValue persist change (PersistentData data) =
    write persist <| change data


updateFromCurrentValue : Persist pData pMsg -> pMsg -> PersistentData pData -> Task String (PersistentData pData)
updateFromCurrentValue persist msg (PersistentData data) =
    write persist <| persist.update msg data



-- Hack I wrote because I didn't have access to the model in the part of the code I was writing.
-- I shouldn't really do this because the Model should be the source of truth,
-- saved to the disk every time it changes ready for next time.


updateFromDisk : Persist pData pMsg -> pMsg -> Task String (PersistentData pData)
updateFromDisk persist msg =
    read persist |> Task.andThen (updateFromCurrentValue persist msg)



-- Not exported as can break ways of working.
-- Not giving you read - The model is the source of truth, not the disk.


read : Persist pData pMsg -> Task String (PersistentData pData)
read persist =
    FS.readTextFile appConfig persist.filename
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



-- Not giving you write - you could generate values outside of your model's PersistentData pData


write : Persist pData pMsg -> pData -> Task String (PersistentData pData)
write persist pData =
    FS.writeTextFile appConfig { filePath = persist.filename, contents = Codec.encodeToString 2 persist.jsonCodec pData }
        |> Task.mapError (\e -> "Persistence.saveDefault error: " ++ TaskPort.errorToString e)
        |> Task.map
            (\_ -> PersistentData pData)



-- Not giving you ensureAppConfigDirExists - I'm pretty sure it's only needed when creating the file.


ensureAppConfigDirExists : Task String ()
ensureAppConfigDirExists =
    FS.exists appConfig ""
        |> Task.andThen
            (\existence ->
                if existence then
                    Task.succeed ()

                else
                    FS.createDir appConfig { createParentsIfAbsent = True } ""
            )
        |> Task.mapError TaskPort.errorToString



-- I think this is always the right decision for where to save application config data.


appConfig : BaseDir.BaseDir
appConfig =
    BaseDir.AppConfig
