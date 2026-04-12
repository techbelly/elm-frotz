port module Main exposing (main)

import Bytes exposing (Bytes)
import Bytes.Encode
import Platform
import ZMachine
import ZMachine.Types exposing (InputRequest(..), OutputEvent(..), StatusLineMode(..), StepResult(..), ZMachineError(..))


port storyLoaded : (List Int -> msg) -> Sub msg


port output : String -> Cmd msg


port requestInput : String -> Cmd msg


port inputProvided : (String -> msg) -> Sub msg


port errorOccurred : String -> Cmd msg


port continueRunning : () -> Cmd msg


port resume : (() -> msg) -> Sub msg


type alias Model =
    { machine : Maybe ZMachine
    , waitingForInput : Maybe InputRequest
    }


type alias ZMachine =
    ZMachine.ZMachine


type Msg
    = StoryLoaded (List Int)
    | InputReceived String
    | Resume


main : Program () Model Msg
main =
    Platform.worker
        { init = \_ -> ( { machine = Nothing, waitingForInput = Nothing }, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ storyLoaded StoryLoaded
        , inputProvided InputReceived
        , resume (\_ -> Resume)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StoryLoaded byteList ->
            let
                bytes =
                    byteList
                        |> List.map Bytes.Encode.unsignedInt8
                        |> Bytes.Encode.sequence
                        |> Bytes.Encode.encode
            in
            case ZMachine.load bytes of
                Err err ->
                    ( model, errorOccurred ("Failed to load: " ++ err) )

                Ok machine ->
                    runMachine machine

        InputReceived text ->
            case ( model.machine, model.waitingForInput ) of
                ( Just machine, Just req ) ->
                    handleResult (ZMachine.provideInput text req machine) model

                _ ->
                    -- The harness does not yet implement file-backed save/restore;
                    -- when a story executes the save/restore opcode the machine
                    -- will surface NeedSave / NeedRestore through `handleResult`,
                    -- which reports the failure branch via provideSaveResult False /
                    -- provideRestoreResult Nothing.
                    ( model, errorOccurred "Input received but not waiting for input" )

        Resume ->
            case model.machine of
                Just machine ->
                    runMachine machine

                Nothing ->
                    ( model, Cmd.none )


runMachine : ZMachine -> ( Model, Cmd Msg )
runMachine machine =
    let
        result =
            ZMachine.runSteps 100000 machine
    in
    handleResult result { machine = Just machine, waitingForInput = Nothing }


handleResult : StepResult -> Model -> ( Model, Cmd Msg )
handleResult result model =
    case result of
        Continue events m ->
            -- Step budget exhausted — yield to JS event loop, then resume
            ( { model | machine = Just m }
            , Cmd.batch [ output (formatOutput events), continueRunning () ]
            )

        NeedInput req events m ->
            ( { model | machine = Just m, waitingForInput = Just req }
            , Cmd.batch [ output (formatOutput events), requestInput "" ]
            )

        NeedSave _ events m ->
            -- Harness has no file I/O wired up yet; report save as failed
            -- so the story's save branch is taken as false.
            let
                ( next, cmd ) =
                    handleResult (ZMachine.provideSaveResult False m) model
            in
            ( next, Cmd.batch [ output (formatOutput events), cmd ] )

        NeedRestore events m ->
            let
                ( next, cmd ) =
                    handleResult (ZMachine.provideRestoreResult Nothing m) model
            in
            ( next, Cmd.batch [ output (formatOutput events), cmd ] )

        Halted events m ->
            ( { model | machine = Just m }
            , Cmd.batch [ output (formatOutput events ++ "\n[Machine halted]"), requestInput "HALT" ]
            )

        Error err events m ->
            ( { model | machine = Just m }
            , errorOccurred (formatOutput events ++ "\n[Error: " ++ errorToString err ++ "]")
            )


formatOutput : List OutputEvent -> String
formatOutput events =
    events
        |> List.filterMap
            (\event ->
                case event of
                    PrintText s ->
                        Just s

                    NewLine ->
                        Just "\n"

                    ShowStatusLine status ->
                        let
                            right =
                                case status.mode of
                                    ScoreAndTurns score turns ->
                                        "Score: " ++ String.fromInt score ++ " Turns: " ++ String.fromInt turns

                                    TimeOfDay hours minutes ->
                                        "Time: " ++ String.fromInt hours ++ ":" ++ String.padLeft 2 '0' (String.fromInt minutes)
                        in
                        Just ("[Status: " ++ status.locationName ++ " | " ++ right ++ "]\n")

                    _ ->
                        Nothing
            )
        |> String.concat


errorToString : ZMachineError -> String
errorToString err =
    case err of
        DivisionByZero ->
            "Division by zero"

        StackUnderflow ->
            "Stack underflow"

        InvalidOpcode n ->
            "Invalid opcode: " ++ String.fromInt n

        InvalidVariable n ->
            "Invalid variable: " ++ String.fromInt n
