module Tauri.BaseDir exposing (..)

import Json.Encode



-- see Path.get


type BaseDir
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


home : BaseDir
home =
    Home


appConfig : BaseDir
appConfig =
    AppConfig


encodeBaseDirectory : BaseDir -> Json.Encode.Value
encodeBaseDirectory b =
    Json.Encode.string <| toString b



{-

   type BaseDirectory
      = App | AppConfig | AppData | AppLocalData | AppLog | Audio | Cache | Config | Data | Desktop | Document
      | Download | Executable | Home | LocalData | Log | Picture | Public | Resource | Runtime | Temp | Template | Video

-}


toString : BaseDir -> String
toString b =
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


toFunctionName : BaseDir -> String
toFunctionName b =
    case toString b |> String.uncons of
        Just ( c, cs ) ->
            String.cons (Char.toLower c) (cs ++ "Dir")

        Nothing ->
            -- shouldn't happen because there aren't any empty strings but elm is careful anyway!
            ""
