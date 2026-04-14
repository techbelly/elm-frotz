port module Dump exposing (main)

import Bytes.Encode
import Platform
import ZMachine
import ZMachine.Memory as Memory
import ZMachine.ObjectTable as ObjectTable
import ZMachine.Types exposing (StepResult(..))


port startDump : ({ story : List Int, commands : List String } -> msg) -> Sub msg


port output : String -> Cmd msg


type alias Model =
    { machine : Maybe ZMachine.ZMachine
    , commands : List String
    }


type Msg
    = Start { story : List Int, commands : List String }


main : Program () Model Msg
main =
    Platform.worker
        { init = \_ -> ( { machine = Nothing, commands = [] }, Cmd.none )
        , update = update
        , subscriptions = \_ -> startDump Start
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update (Start { story, commands }) _ =
    let
        bytes =
            story
                |> List.map Bytes.Encode.unsignedInt8
                |> Bytes.Encode.sequence
                |> Bytes.Encode.encode
    in
    case ZMachine.load bytes of
        Err err ->
            ( { machine = Nothing, commands = [] }, output ("ERROR: " ++ err) )

        Ok machine ->
            runUntilDone (ZMachine.runSteps stepBudget machine) commands


stepBudget : Int
stepBudget =
    1000000


runUntilDone : StepResult -> List String -> ( Model, Cmd Msg )
runUntilDone result commands =
    case result of
        Continue _ m ->
            runUntilDone (ZMachine.runSteps stepBudget m) commands

        NeedInput info _ m ->
            case commands of
                cmd :: rest ->
                    runUntilDone (ZMachine.provideInput cmd info m) rest

                [] ->
                    finish m

        NeedChar _ m ->
            case commands of
                cmd :: rest ->
                    runUntilDone (ZMachine.provideChar (String.left 1 cmd) m) rest

                [] ->
                    finish m

        NeedSave _ _ m ->
            runUntilDone (ZMachine.provideSaveResult False m) commands

        NeedRestore _ m ->
            runUntilDone (ZMachine.provideRestoreResult Nothing m) commands

        Halted _ m ->
            finish m

        Error err _ m ->
            let
                _ =
                    err
            in
            finish m


finish : ZMachine.ZMachine -> ( Model, Cmd Msg )
finish machine =
    let
        header =
            "obj#\tparent#\tchildren\tsibs\tattrs\tparent_name\tname"

        lines =
            collectObjects 1 machine.memory []
    in
    ( { machine = Just machine, commands = [] }
    , output (String.join "\n" (header :: lines) ++ "\n")
    )


collectObjects : Int -> Memory.Memory -> List String -> List String
collectObjects n mem acc =
    if n > 500 then
        List.reverse acc

    else
        case dumpObject n mem of
            Nothing ->
                List.reverse acc

            Just line ->
                collectObjects (n + 1) mem (line :: acc)


dumpObject : Int -> Memory.Memory -> Maybe String
dumpObject n mem =
    let
        name =
            ObjectTable.shortName n mem

        parentNum =
            ObjectTable.parent n mem

        childNum =
            ObjectTable.child n mem
    in
    if parentNum > 500 || childNum > 500 then
        Nothing

    else
        let
            parentName =
                ObjectTable.shortName parentNum mem

            childCount =
                countChain childNum mem 0 0

            siblingCount =
                countChain (ObjectTable.child parentNum mem) mem 0 0

            attrs =
                List.range 0 47
                    |> List.filter (\a -> ObjectTable.testAttribute n a mem)
                    |> List.map String.fromInt
                    |> String.join ","
        in
        Just
            (String.join "\t"
                [ String.fromInt n
                , String.fromInt parentNum
                , String.fromInt childCount
                , String.fromInt siblingCount
                , attrs
                , parentName
                , name
                ]
            )


countChain : Int -> Memory.Memory -> Int -> Int -> Int
countChain objNum mem depth acc =
    if objNum == 0 || depth > 1000 then
        acc

    else
        countChain (ObjectTable.sibling objNum mem) mem (depth + 1) (acc + 1)
