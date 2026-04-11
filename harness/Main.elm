port module Main exposing (main)

import Bytes exposing (Bytes)
import Bytes.Encode
import Platform
import ZMachine
import ZMachine.Types exposing (InputRequest(..), OutputEvent(..), StepResult(..), ZMachineError(..))


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
                    case ZMachine.provideInput text req machine of
                        Continue next ->
                            runMachine next

                        other ->
                            handleResult other model

                _ ->
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
        Continue m ->
            -- Step budget exhausted — yield to JS event loop, then resume
            let
                text =
                    formatOutput (ZMachine.getOutput m)

                cleaned =
                    ZMachine.clearOutput m
            in
            ( { model | machine = Just cleaned }
            , Cmd.batch [ output text, continueRunning () ]
            )

        NeedInput req m ->
            let
                text =
                    formatOutput (ZMachine.getOutput m)
            in
            ( { model | machine = Just (ZMachine.clearOutput m), waitingForInput = Just req }
            , Cmd.batch [ output text, requestInput "" ]
            )

        Halted m ->
            let
                text =
                    formatOutput (ZMachine.getOutput m)
            in
            ( { model | machine = Just m }
            , Cmd.batch [ output (text ++ "\n[Machine halted]"), requestInput "HALT" ]
            )

        Error err m ->
            let
                text =
                    formatOutput (ZMachine.getOutput m)
            in
            ( { model | machine = Just m }
            , errorOccurred (text ++ "\n[Error: " ++ errorToString err ++ "]")
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
                        Just ("[Status: " ++ status.locationName ++ " | Score: " ++ String.fromInt status.score ++ " Turns: " ++ String.fromInt status.turns ++ "]\n")

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

        IllegalMemoryAccess n ->
            "Illegal memory access: " ++ String.fromInt n
