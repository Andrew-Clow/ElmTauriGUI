module Tauri.Modified exposing (..)

import Json.Decode
import Json.Encode
import Task exposing (Task)
import TaskPort
import Tauri.DateTime as DateTime exposing (DateTime)
import Time



{-

   In addition to the TaskPort javascript,
   this requires additions to your src-tauri/main.rs.
   There's a full example of main.rs below.


-}


type alias FilePath =
    String


getFileModifiedPosix : FilePath -> Task TaskPort.Error { filePath : FilePath, modified : Time.Posix }
getFileModifiedPosix filePath =
    Task.map (\time -> { filePath = filePath, modified = time }) <|
        TaskPort.call
            { function = "modifiedTime"
            , argsEncoder = Json.Encode.string
            , valueDecoder = rustSystemTimeDecoder
            }
            filePath


getFileModifiedDateTime : Time.Zone -> FilePath -> Task TaskPort.Error { filePath : FilePath, modified : DateTime }
getFileModifiedDateTime zone filePath =
    let
        answer posixAnswer =
            { filePath = filePath, modified = DateTime.posixToDateTime zone posixAnswer.modified }
    in
    Task.map answer <| getFileModifiedPosix filePath



-- decoder


rustSystemTimeDecoder : Json.Decode.Decoder Time.Posix
rustSystemTimeDecoder =
    let
        time seconds nanoseconds =
            Time.millisToPosix <|
                seconds
                    * 1000
                    + nanoseconds
                    // 1000000
    in
    Json.Decode.map2 time
        (Json.Decode.field "secs_since_epoch" <| Json.Decode.int)
        (Json.Decode.field "nanos_since_epoch" <| Json.Decode.int)



{- src-tauri/main.rs

   #![cfg_attr(
     all(not(debug_assertions), target_os = "windows"),
     windows_subsystem = "windows"
   )]

   use std::fs;                    // add imports for filesystem access
   use std::time::SystemTime;      // and times


   #[tauri::command]
   fn modified_time(file_path: String) -> Result<SystemTime, String> {
       modified_time_of(file_path).map_err(|err| err.to_string())        // separate function to change either error to String
   }

   fn modified_time_of(file_path: String) -> Result<SystemTime, std::io::Error> {
     let meta = fs::metadata(&file_path)?;
     meta.modified()

   }

   fn main() {
     tauri::Builder::default()
       .invoke_handler(tauri::generate_handler![modified_time])        // Add handler for the function. Function names should be snake case.
       .run(tauri::generate_context!())
       .expect("error while running tauri application");
   }



-}
