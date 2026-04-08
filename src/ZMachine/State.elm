module ZMachine.State exposing
    ( ZMachine
    , StepResult(..)
    , ZMachineError(..)
    , InputRequest(..)
    , OutputEvent(..)
    , StatusLine
    , Window(..)
    , init
    , readVariable
    , writeVariable
    , pushStack
    , popStack
    , appendOutput
    )

{-| Z-Machine state and core operations.

@docs ZMachine, StepResult, ZMachineError, InputRequest, OutputEvent, StatusLine, Window
@docs init, readVariable, writeVariable, pushStack, popStack, appendOutput

-}

import Array
import Bitwise
import Bytes exposing (Bytes)
import ZMachine.Instruction exposing (VariableRef(..))
import ZMachine.Memory as Memory exposing (Memory)
import ZMachine.Memory.Header as Header
import ZMachine.Stack exposing (CallFrame)


{-| The complete state of a running Z-Machine.
-}
type alias ZMachine =
    { memory : Memory
    , originalMemory : Memory
    , pc : Int
    , stack : List Int
    , callStack : List CallFrame
    , output : List OutputEvent
    , outputStreams : { stream1 : Bool, stream2 : Bool, stream3 : List Int }
    , randomState : { seed : Int, count : Int }
    }


{-| Result of executing one or more instructions.
-}
type StepResult
    = Continue ZMachine
    | NeedInput InputRequest ZMachine
    | Halted ZMachine
    | Error ZMachineError ZMachine


{-| Errors that can halt the machine.
-}
type ZMachineError
    = DivisionByZero
    | StackUnderflow
    | InvalidOpcode Int
    | InvalidVariable Int
    | IllegalMemoryAccess Int


{-| What kind of input the machine needs.
-}
type InputRequest
    = LineInput
        { maxLength : Int
        , textBufferAddr : Int
        , parseBufferAddr : Int
        }


{-| Structured output events for the host to render.
-}
type OutputEvent
    = PrintText String
    | NewLine
    | ShowStatusLine StatusLine
    | SplitWindow Int
    | SetWindow Window
    | EraseWindow Int
    | SetCursor Int Int
    | SetBufferMode Bool
    | PlaySound Int


{-| Status line data.
-}
type alias StatusLine =
    { locationName : String
    , score : Int
    , turns : Int
    , isTimeGame : Bool
    }


{-| Screen window identifier.
-}
type Window
    = Upper
    | Lower


{-| Initialize a Z-Machine from a loaded story file.
-}
init : Memory -> ZMachine
init mem =
    let
        pc =
            Header.initialPC mem

        -- Set interpreter metadata in header
        configuredMem =
            mem
                |> Header.setInterpreterInfo 6 (Char.toCode 'A')
                |> Header.setScreenSize 25 80
                |> Header.setStandardRevision 1 1
    in
    { memory = configuredMem
    , originalMemory = mem
    , pc = pc
    , stack = []
    , callStack = []
    , output = []
    , outputStreams = { stream1 = True, stream2 = False, stream3 = [] }
    , randomState = { seed = 12345, count = 0 }
    }


{-| Read a variable value. Variable 0 = pop stack, 1-15 = locals, 16-255 = globals.
Returns the value and updated machine (stack may be modified).
-}
readVariable : VariableRef -> ZMachine -> ( Int, ZMachine )
readVariable ref machine =
    case ref of
        Stack ->
            popStack machine

        Local n ->
            case machine.callStack of
                frame :: _ ->
                    ( ZMachine.Stack.getLocal n frame, machine )

                [] ->
                    ( 0, machine )

        Global n ->
            let
                globalsAddr =
                    Header.globalVariablesAddress machine.memory

                addr =
                    globalsAddr + (n - 0x10) * 2
            in
            ( Memory.readWord addr machine.memory, machine )


{-| Write a value to a variable. Variable 0 = push stack, 1-15 = locals, 16-255 = globals.
-}
writeVariable : VariableRef -> Int -> ZMachine -> ZMachine
writeVariable ref value machine =
    let
        val =
            toUnsigned16 value
    in
    case ref of
        Stack ->
            pushStack val machine

        Local n ->
            case machine.callStack of
                frame :: rest ->
                    { machine | callStack = ZMachine.Stack.setLocal n val frame :: rest }

                [] ->
                    machine

        Global n ->
            let
                globalsAddr =
                    Header.globalVariablesAddress machine.memory

                addr =
                    globalsAddr + (n - 0x10) * 2
            in
            { machine | memory = Memory.writeWord addr val machine.memory }


{-| Push a value onto the evaluation stack.
-}
pushStack : Int -> ZMachine -> ZMachine
pushStack value machine =
    { machine | stack = toUnsigned16 value :: machine.stack }


{-| Pop a value from the evaluation stack.
Returns (value, updatedMachine). Returns (0, machine) on underflow.
-}
popStack : ZMachine -> ( Int, ZMachine )
popStack machine =
    case machine.stack of
        top :: rest ->
            ( top, { machine | stack = rest } )

        [] ->
            ( 0, machine )


{-| Append an output event.
-}
appendOutput : OutputEvent -> ZMachine -> ZMachine
appendOutput event machine =
    { machine | output = machine.output ++ [ event ] }



-- INTERNAL


toUnsigned16 : Int -> Int
toUnsigned16 n =
    Bitwise.and n 0xFFFF
