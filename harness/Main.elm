port module Main exposing (main)

import Bytes exposing (Bytes)
import Bytes.Encode
import Platform
import ZMachine
import ZMachine.Types exposing (LineInputInfo, OutputEvent(..), StatusLineMode(..), StepResult(..), ZMachineError(..))


port storyLoaded : (List Int -> msg) -> Sub msg


port output : String -> Cmd msg


port requestInput : String -> Cmd msg


port inputProvided : (String -> msg) -> Sub msg


port errorOccurred : String -> Cmd msg


port continueRunning : () -> Cmd msg


port resume : (() -> msg) -> Sub msg


type alias Model =
    { machine : Maybe ZMachine
    , waitingFor : Maybe WaitingFor
    , eventsMode : Bool
    }


type WaitingFor
    = WaitingForLine LineInputInfo
    | WaitingForChar


type alias ZMachine =
    ZMachine.ZMachine


type Msg
    = StoryLoaded (List Int)
    | InputReceived String
    | Resume


{-| Flag: True for NDJSON event output, False for rendered text.
-}
type alias Flags =
    Bool


main : Program Flags Model Msg
main =
    Platform.worker
        { init =
            \eventsMode ->
                ( { machine = Nothing, waitingFor = Nothing, eventsMode = eventsMode }
                , Cmd.none
                )
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
                    runMachine machine model

        InputReceived text ->
            case ( model.machine, model.waitingFor ) of
                ( Just machine, Just (WaitingForLine info) ) ->
                    handleResult (ZMachine.provideInput text info machine) model

                ( Just machine, Just WaitingForChar ) ->
                    handleResult (ZMachine.provideChar (String.left 1 text) machine) model

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
                    runMachine machine model

                Nothing ->
                    ( model, Cmd.none )


runMachine : ZMachine -> Model -> ( Model, Cmd Msg )
runMachine machine model =
    let
        result =
            ZMachine.runSteps 100000 machine
    in
    handleResult result { model | machine = Just machine, waitingFor = Nothing }


handleResult : StepResult -> Model -> ( Model, Cmd Msg )
handleResult result model =
    let
        em =
            model.eventsMode
    in
    case result of
        Continue events m ->
            -- Step budget exhausted — yield to JS event loop, then resume
            ( { model | machine = Just m }
            , Cmd.batch [ output (formatOutput em events), continueRunning () ]
            )

        NeedInput info events m ->
            ( { model | machine = Just m, waitingFor = Just (WaitingForLine info) }
            , Cmd.batch
                [ output (formatOutput em events ++ marker em (needInputJson info))
                , requestInput ""
                ]
            )

        NeedChar events m ->
            ( { model | machine = Just m, waitingFor = Just WaitingForChar }
            , Cmd.batch
                [ output (formatOutput em events ++ marker em needCharJson)
                , requestInput "CHAR"
                ]
            )

        NeedSave _ events m ->
            -- Harness has no file I/O wired up yet; report save as failed
            -- so the story's save branch is taken as false.
            let
                ( next, cmd ) =
                    handleResult (ZMachine.provideSaveResult False m) model
            in
            ( next, Cmd.batch [ output (formatOutput em events), cmd ] )

        NeedRestore events m ->
            let
                ( next, cmd ) =
                    handleResult (ZMachine.provideRestoreResult Nothing m) model
            in
            ( next, Cmd.batch [ output (formatOutput em events), cmd ] )

        Halted events m ->
            ( { model | machine = Just m }
            , Cmd.batch
                [ output
                    (formatOutput em events
                        ++ (if em then
                                jsonLine haltedJson

                            else
                                "\n[Machine halted]"
                           )
                    )
                , requestInput "HALT"
                ]
            )

        Error err events m ->
            let
                msg =
                    errorToString err
            in
            if em then
                ( { model | machine = Just m }
                , Cmd.batch
                    [ output (formatOutput em events ++ jsonLine (errorJson msg))
                    , errorOccurred ""
                    ]
                )

            else
                ( { model | machine = Just m }
                , errorOccurred (formatOutput em events ++ "\n[Error: " ++ msg ++ "]")
                )


formatOutput : Bool -> List OutputEvent -> String
formatOutput eventsMode events =
    if eventsMode then
        events |> List.map (eventToJson >> jsonLine) |> String.concat

    else
        events
            |> List.filterMap
                (\event ->
                    case event of
                        PrintText s ->
                            Just s

                        PrintObject s ->
                            Just s

                        ShowStatusLine status ->
                            Just ("[Status: " ++ formatStatusLine status ++ "]\n")

                        _ ->
                            Nothing
                )
            |> String.concat


formatStatusLine : { a | locationId : Int, locationName : String, mode : StatusLineMode } -> String
formatStatusLine status =
    let
        loc =
            "loc=" ++ String.fromInt status.locationId ++ " name=\"" ++ status.locationName ++ "\""
    in
    case status.mode of
        ScoreAndTurns score turns ->
            loc ++ " | Score: " ++ String.fromInt score ++ " Turns: " ++ String.fromInt turns

        TimeOfDay hours minutes ->
            loc ++ " | Time: " ++ String.fromInt hours ++ ":" ++ String.padLeft 2 '0' (String.fromInt minutes)

        ScreenRows rows ->
            loc ++ " | " ++ String.join " | " rows


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



-- NDJSON EVENT SERIALISATION -------------------------------------------------


marker : Bool -> String -> String
marker eventsMode json =
    if eventsMode then
        jsonLine json

    else
        ""


jsonLine : String -> String
jsonLine s =
    s ++ "\n"


eventToJson : OutputEvent -> String
eventToJson event =
    case event of
        PrintText s ->
            jsonObj [ ( "event", jsonStr "print_text" ), ( "text", jsonStr s ) ]

        PrintObject s ->
            jsonObj [ ( "event", jsonStr "print_object" ), ( "text", jsonStr s ) ]

        ShowStatusLine status ->
            jsonObj
                [ ( "event", jsonStr "status_line" )
                , ( "locationId", jsonInt status.locationId )
                , ( "locationName", jsonStr status.locationName )
                , ( "mode", modeToJson status.mode )
                ]

        SetBufferMode b ->
            jsonObj [ ( "event", jsonStr "set_buffer_mode" ), ( "enabled", jsonBool b ) ]

        SetTextStyle n ->
            jsonObj [ ( "event", jsonStr "set_text_style" ), ( "style", jsonInt n ) ]

        SetColour fg bg ->
            jsonObj [ ( "event", jsonStr "set_colour" ), ( "fg", jsonInt fg ), ( "bg", jsonInt bg ) ]

        PlaySound n ->
            jsonObj [ ( "event", jsonStr "play_sound" ), ( "id", jsonInt n ) ]


modeToJson : StatusLineMode -> String
modeToJson mode =
    case mode of
        ScoreAndTurns score turns ->
            jsonObj
                [ ( "mode", jsonStr "score_and_turns" )
                , ( "score", jsonInt score )
                , ( "turns", jsonInt turns )
                ]

        TimeOfDay hours minutes ->
            jsonObj
                [ ( "mode", jsonStr "time_of_day" )
                , ( "hours", jsonInt hours )
                , ( "minutes", jsonInt minutes )
                ]

        ScreenRows rows ->
            jsonObj
                [ ( "mode", jsonStr "screen_rows" )
                , ( "rows", jsonArray (List.map jsonStr rows) )
                ]


needInputJson : LineInputInfo -> String
needInputJson info =
    jsonObj
        [ ( "event", jsonStr "need_input" )
        , ( "maxLength", jsonInt info.maxLength )
        ]


needCharJson : String
needCharJson =
    jsonObj [ ( "event", jsonStr "need_char" ) ]


haltedJson : String
haltedJson =
    jsonObj [ ( "event", jsonStr "halted" ) ]


errorJson : String -> String
errorJson msg =
    jsonObj [ ( "event", jsonStr "error" ), ( "message", jsonStr msg ) ]



-- MINIMAL JSON PRIMITIVES ----------------------------------------------------


jsonStr : String -> String
jsonStr s =
    "\"" ++ String.foldl (\c acc -> acc ++ escapeChar c) "" s ++ "\""


escapeChar : Char -> String
escapeChar c =
    case c of
        '"' ->
            "\\\""

        '\\' ->
            "\\\\"

        '\n' ->
            "\\n"

        '\u{000D}' ->
            "\\r"

        '\t' ->
            "\\t"

        _ ->
            let
                code =
                    Char.toCode c
            in
            if code < 0x20 then
                "\\u" ++ String.padLeft 4 '0' (toHex4 code)

            else
                String.fromChar c


toHex4 : Int -> String
toHex4 n =
    hexDigit ((n // 4096) |> modBy 16)
        ++ hexDigit ((n // 256) |> modBy 16)
        ++ hexDigit ((n // 16) |> modBy 16)
        ++ hexDigit (modBy 16 n)


hexDigit : Int -> String
hexDigit d =
    if d < 10 then
        String.fromChar (Char.fromCode (Char.toCode '0' + d))

    else
        String.fromChar (Char.fromCode (Char.toCode 'a' + (d - 10)))


jsonObj : List ( String, String ) -> String
jsonObj pairs =
    "{" ++ String.join "," (List.map (\( k, v ) -> jsonStr k ++ ":" ++ v) pairs) ++ "}"


jsonInt : Int -> String
jsonInt =
    String.fromInt


jsonBool : Bool -> String
jsonBool b =
    if b then
        "true"

    else
        "false"


jsonArray : List String -> String
jsonArray items =
    "[" ++ String.join "," items ++ "]"
