module ZMachine.State exposing
    ( appendOutput
    , init
    , peekStack
    , pokeStack
    , popStack
    , pushStack
    , readVariable
    , writeVariable
    )

{-| Internal Z-Machine state operations.

This module provides the state manipulation functions used by the
execution engine. Library consumers should use `ZMachine` and
`ZMachine.Types` instead.

-}

import Bitwise
import ZMachine.Instruction exposing (VariableRef(..))
import ZMachine.Memory as Memory exposing (Memory)
import ZMachine.Header as Header
import ZMachine.Stack
import ZMachine.Types exposing (OutputEvent, ZMachine)


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
                    globalsAddr + (n - 0x10) * Memory.wordLength
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
                    globalsAddr + (n - 0x10) * Memory.wordLength
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


{-| Read the top of the evaluation stack without popping it.
Returns 0 on underflow. Used for indirect variable references to
the stack pointer, per Z-machine Standard §6.3.4.
-}
peekStack : ZMachine -> Int
peekStack machine =
    case machine.stack of
        top :: _ ->
            top

        [] ->
            0


{-| Replace the top of the evaluation stack in place. No-op on
underflow. Used for indirect variable writes to the stack pointer,
per Z-machine Standard §6.3.4.
-}
pokeStack : Int -> ZMachine -> ZMachine
pokeStack value machine =
    case machine.stack of
        _ :: rest ->
            { machine | stack = toUnsigned16 value :: rest }

        [] ->
            machine


{-| Append an output event.
-}
appendOutput : OutputEvent -> ZMachine -> ZMachine
appendOutput event machine =
    { machine | output = machine.output ++ [ event ] }



-- INTERNAL


toUnsigned16 : Int -> Int
toUnsigned16 n =
    Bitwise.and n 0xFFFF
