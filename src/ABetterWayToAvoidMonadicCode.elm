module ABetterWayToAvoidMonadicCode exposing (..)

-- This is ideas in code form rather than working code, written for a post in elm slack, but preserved here for my future reference.

import Task exposing (Task)


type alias Model =
    { saveAsFilePath : Maybe String, content : Int }


type alias FilePath =
    String



-- ManyMsgs: program sequencing logic is spread across messages and update.
-- Good: very elm architecture.
-- Bad: very repetetive, ugly message names. This code many times over for different purposes.
-- Balance: repetition brings editability, understandability, maintainability
-- Compromise: maybe you can factor out some of those commonailities


type ManyMsgs
    = SaveAsClicked
    | SaveAsChoseFilePath FilePath
    | SaveAsItPreExists FilePath Bool
    | SaveAsConfirmClobber FilePath Bool
    | FSErrorArrived Tauri.FS.Error
    | ExportClicked
    | ExportChoseFilePath FilePath
    | ExportItPreExists FilePath Bool
    | ExportConfirmClobber FilePath
    | SaveClicked
    | OpenClicked
    | OpenChoseFilePath FilePath
    | OpenGotFile String
    | OpenBadFormat Content.File.FormatError


updateManyMsgs : ManyMsgs -> Model -> ( Model, Cmd ManyMsgs )
updateManyMsgs msg model =
    case msg of
        SaveAsClicked ->
            ( model, TauriCmd.Dialog.saveAs SaveAsChoseFilePath )

        SaveAsChoseFilePath filePath ->
            ( model, TauriCmd.FS.exists filePath (SaveAsItPreExists filePath) )

        SaveAsItPreExists filePath exists ->
            if exists then
                ( model, TauriCmd.Dialog.confirm Tauri.Dialog.Warning (filePath ++ " already exists. Save anyway?") (SaveAsConfirmClobber filePath) )

            else
                ( model, TauriCmd.save filePath (Content.File.output <| model.content) FSErrorArrived )

        SaveAsConfirmClobber filePath confirmed ->
            if confirmed then
                ( model, TauriCmd.save filePath (Content.File.output <| model.content) FSErrorArrived )

            else
                ( model, Cmd.none )

        _ ->
            -- Actually, seven more very similar cases, long, repetative, spread out, but easy
            -- and a bit of error handling
            ( model, Cmd.none )



-- MonadicTaskMsgs: program sequencing logic is in a Task.andThen block.
-- Good: fewer messages, sequencing logic is all in one place, even if it's messy. Consise.
-- Bad: Not very functional, feels imperative, ugly ugly indentation
-- Bad: I can't read that. I just can't read it.
-- Bad: I end up copy-pasting large chunks of ugly formatted monadic style code and honestly,
--      if I have to write monadic code, I'd rather do it in Haskell where the syntax supports it,
--      but it's all monadic over there and I don't enjoy it.


type MonadicTaskMsgs
    = SaveAs
    | Export
    | Save
    | Open
    | Cancelled
    | TaskResult (Result Tauri.FS.Error ())


updateMonadicTaskMsgs : MonadicTaskMsgs -> Model -> ( Model, Cmd MonadicTaskMsgs )
updateMonadicTaskMsgs msg model =
    case msg of
        SaveAs ->
            ( model
            , (TauriTask.Dialog.saveAs
                |> TauriTask.andThen
                    (\filePath ->
                        TauriTask.FS.exists filePath
                            |> TauriTask.andThen
                                (\exists ->
                                    if exists then
                                        TauriTask.Dialog.confirm Tauri.Dialog.Warning (filePath ++ " already exists. Save anyway?")
                                            |> TauriTask.andThen
                                                (\confirmed ->
                                                    if confirmed then
                                                        TauriTask.FS.save filePath (Content.toString <| model.content) FSErrorArrived

                                                    else
                                                        Task.succeed ()
                                                )

                                    else
                                        TauriTask.FS.save filePath (Content.toString <| model.content) FSErrorArrived
                                )
                    )
              )
                |> Task.attempt TaskResult
            )

        _ ->
            -- Actually, three more horrible, complex, difficult to understand cases
            -- and a little error handling
            ( model, Cmd.none )


updateMonadicTaskMsgsWithLetBindings : MonadicTaskMsgs -> Model -> ( Model, Cmd MonadicTaskMsgs )
updateMonadicTaskMsgsWithLetBindings msg model =
    let
        checkExists : FilePath -> Task Tauri.FSError { filePath : FilePath, exists : Bool }
        checkExists filePath =
            TauriTask.FS.exists |> Task.map (\exists -> { filePath = filePath, exists = exists })

        ifExistsThenConfirm : { filePath : FilePath, exists : Bool } -> Task Tauri.FSError ()
        ifExistsThenConfirm { filePath, exists } =
            if exists then
                TauriTask.Dialog.confirm Tauri.Dialog.Warning (filePath ++ " already exists. Save anyway?")
                    |> TauriTask.andThen (ifConfirmedThenSave filePath)

            else
                actuallySave filePath

        ifConfirmedThenSave : FilePath -> Bool -> Task Tauri.FSError ()
        ifConfirmedThenSave filePath confirmed =
            if confirmed then
                actuallySave filePath

            else
                Task.succeed ()

        actuallySave : FilePath -> Task Tauri.FSError ()
        actuallySave filePath =
            TauriTask.FS.save filePath (Content.toString <| model.content) FSErrorArrived
    in
    case msg of
        SaveAs ->
            ( model
            , TauriTask.Dialog.saveAs
                |> TauriTask.andThen checkExists
                |> TauriTask.andThen ifExistsThenConfirm
                |> TauriTask.andThen ifConfirmedThenSave
                |> Task.attempt TaskResult
            )

        _ ->
            -- Actually, three more lengthy, multiple let-bindings type things
            -- and a little error handling
            ( model, Cmd.none )



-- MsgTree: program sequencing logic is in the messages.
-- Good: fewer messages, less repetition because code becomes multipurpose
-- Bad: Messages can be hard to read.
-- Balance: the elm architecture, more consisely, but with maybe uglier messages


type MsgTree
    = ClickedSaveAs
    | ClickedSave
    | ClickedOpen
    | ClickedExport
    | CheckExists FilePath { fileExists : MsgTree, fileDoesNotExist : MsgTree }
    | Confirm String { saidYes : MsgTree, saidNo : MsgTree }
    | Clobber FilePath String
    | CancelledSomething
    | GotFSError Tauri.FS.Error


updateMsgTree : MsgTree -> Model -> ( Model, Cmd MsgTree )
updateMsgTree msg model =
    case msg of
        ClickedSaveAs ->
            ( model
            , TauriCmd.Dialog.saveAs <|
                \filePath ->
                    CheckExists filePath
                        { fileExists =
                            Confirm (filePath ++ " already exists. Save anyway?")
                                { saidYes = Clobber filePath (Content.File.output <| model.content)
                                , saidNo = CancelledSomething
                                }
                        , fileDoesNotExist = Clobber filePath (Content.File.output <| model.content)
                        }
            )

        CheckExists filePath { fileExists, fileDoesNotExist } ->
            ( model
            , TauriCmd.FS.exists filePath <|
                \existence ->
                    if existence then
                        fileExists

                    else
                        fileDoesNotExist
            )

        Confirm question { saidYes, saidNo } ->
            -- I'm tempted to do ifThenElse : a -> a -> Bool to make these one-liners!
            ( model
            , TauriCmd.Dialog.confirm Tauri.Dialog.Warning question <|
                \answer ->
                    if answer then
                        saidYes

                    else
                        saidNo
            )

        Clobber filePath contents ->
            ( model, TauriCmd.save filePath contents GotFSError )

        _ ->
            -- Actually, three more cases, each as clearly self-documenting as the ClickedSaveAs case,
            -- and a little error-handling, as usual.
            ( model, Cmd.none )
