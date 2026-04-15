module ZMachine.State exposing
    ( appendOutput
    , eraseLine
    , eraseWindow
    , init
    , outputText
    , outputObjectName
    , outputNewLine
    , peekStack
    , pokeStack
    , popStack
    , pushStack
    , readVariable
    , setCursor
    , getCursor
    , setWindow
    , splitWindow
    , flushUpperWindow
    , pushStream3
    , popStream3
    , writeVariable
    )

{-| Internal Z-Machine state operations.

This module provides the state manipulation functions used by the
execution engine. Library consumers should use `ZMachine` and
`ZMachine.Types` instead.

-}

import Library.IntExtra exposing (toUnsignedInt16)
import ZMachine.Opcode exposing (VariableRef(..))
import ZMachine.Memory as Memory exposing (Memory)
import ZMachine.Header as Header
import ZMachine.ObjectTable as ObjectTable
import ZMachine.Player as Player
import ZMachine.Stack
import ZMachine.Types exposing (OutputEvent(..), StatusLineMode(..), Window(..), ZMachine)
import ZMachine.Window as Window


{-| Initialize a Z-Machine from a loaded story file.
-}
init : Memory -> ZMachine
init mem =
    let
        pc =
            Header.initialPC mem

        -- Set interpreter metadata in header
        baseMem =
            mem
                |> Header.setInterpreterInfo 3 (Char.toCode 'A')
                |> Header.setScreenSize 25 80
                |> Header.setStandardRevision 1 1

        configuredMem =
            case (Memory.profile mem).version of
                Memory.V5 ->
                    baseMem
                        |> Header.setScreenSizeUnits
                            { widthUnits = 80, heightUnits = 25, fontWidth = 1, fontHeight = 1 }
                        |> Header.setDefaultColours 9 2

                Memory.V3 ->
                    baseMem
    in
    { memory = configuredMem
    , originalMemory = mem
    , pc = pc
    , stack = []
    , callStack = []
    , output = []
    , outputStreams = { stream1 = True, stream2 = False }
    , stream3Stack = []
    , randomState = { seed = 12345, count = 0 }
    , currentWindow = Lower
    , upperWindow = Window.empty 80
    , playerTracking = Player.empty
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
            ( Memory.readWord (globalAddress n machine.memory) machine.memory
            , { machine | playerTracking = Player.noteGlobalRead n machine.playerTracking }
            )


{-| Write a value to a variable. Variable 0 = push stack, 1-15 = locals, 16-255 = globals.

Normalization into the 0..65535 range is handled by the underlying
primitives (`pushStack`, `Stack.setLocal`, `Memory.writeWord`), so
callers may pass signed or oversized values directly.

-}
writeVariable : VariableRef -> Int -> ZMachine -> ZMachine
writeVariable ref value machine =
    case ref of
        Stack ->
            pushStack value machine

        Local n ->
            case machine.callStack of
                frame :: rest ->
                    { machine | callStack = ZMachine.Stack.setLocal n value frame :: rest }

                [] ->
                    machine

        Global n ->
            { machine | memory = Memory.writeWord (globalAddress n machine.memory) value machine.memory }


{-| Byte address of global variable `n`. Globals are numbered from `0x10`
onward in the Z-machine variable space, so the stored word index is
`n - 0x10`.
-}
globalAddress : Int -> Memory -> Int
globalAddress n mem =
    Header.globalVariablesAddress mem + (n - firstGlobalVariable) * Memory.wordLength


{-| The variable number at which the global region begins (globals are
numbered 0x10..0xFF in the Z-machine variable space).
-}
firstGlobalVariable : Int
firstGlobalVariable =
    0x10


{-| Push a value onto the evaluation stack.
-}
pushStack : Int -> ZMachine -> ZMachine
pushStack value machine =
    { machine | stack = toUnsignedInt16 value :: machine.stack }


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
            { machine | stack = toUnsignedInt16 value :: rest }

        [] ->
            machine


{-| Append an output event.

Events are stored newest-first so that appending is O(1); call
[`ZMachine.Run.getOutput`](ZMachine-Run#getOutput) to retrieve them in
chronological order.

Consecutive `PrintText` events are coalesced into a single event — a
rendered paragraph typically reaches the host as one string rather
than dozens of per-opcode fragments. `PrintObject` is never merged
with `PrintText`, so clients keep the split between narrative text
and object short names.

-}
appendOutput : OutputEvent -> ZMachine -> ZMachine
appendOutput event machine =
    case ( event, machine.output ) of
        ( PrintText new, (PrintText prev) :: rest ) ->
            { machine | output = PrintText (prev ++ new) :: rest }

        _ ->
            { machine | output = event :: machine.output }



-- STREAM 3 (MEMORY TABLE)


{-| Start redirecting output to a memory table. The table format is:
word 0 = character count (written on pop), then ZSCII bytes follow.
-}
pushStream3 : Int -> ZMachine -> ZMachine
pushStream3 tableAddr machine =
    { machine
        | stream3Stack = { tableAddr = tableAddr, count = 0 } :: machine.stream3Stack
    }


{-| Stop redirecting output to memory. Writes the character count
to the first word of the table.
-}
popStream3 : ZMachine -> ZMachine
popStream3 machine =
    case machine.stream3Stack of
        entry :: rest ->
            { machine
                | memory = Memory.writeWord entry.tableAddr entry.count machine.memory
                , stream3Stack = rest
            }

        [] ->
            machine



-- UPPER WINDOW / SCREEN


{-| Output text, routing to stream 3 (memory table), the upper window
grid, or the lower window output list.
-}
outputText : String -> ZMachine -> ZMachine
outputText str machine =
    case machine.stream3Stack of
        entry :: rest ->
            -- Stream 3 active: write characters to memory table
            let
                chars =
                    String.toList str

                ( mem, newCount ) =
                    List.foldl
                        (\ch ( m, n ) ->
                            case charToZscii ch of
                                Just code ->
                                    ( Memory.writeByte (entry.tableAddr + 2 + n) code m
                                    , n + 1
                                    )

                                Nothing ->
                                    ( m, n )
                        )
                        ( machine.memory, entry.count )
                        chars
            in
            { machine
                | memory = mem
                , stream3Stack = { entry | count = newCount } :: rest
            }

        [] ->
            case machine.currentWindow of
                Lower ->
                    appendOutput (PrintText str) machine

                Upper ->
                    { machine | upperWindow = Window.printText str machine.upperWindow }


{-| Output a string that the game identified as an object's short name
(via `print_obj` or `print_addr`). Routes identically to
[`outputText`](#outputText) — into the stream-3 memory table or the
upper-window grid when those are active — except that, when the text
reaches the lower window, it is emitted as a
[`PrintObject`](ZMachine-Types#OutputEvent) event so clients can
distinguish object names from ordinary text output.
-}
outputObjectName : String -> ZMachine -> ZMachine
outputObjectName str machine =
    case ( machine.stream3Stack, machine.currentWindow ) of
        ( [], Lower ) ->
            appendOutput (PrintObject str) machine

        _ ->
            outputText str machine


{-| Output a newline, routing to stream 3, upper window, or lower window.
-}
outputNewLine : ZMachine -> ZMachine
outputNewLine machine =
    case machine.stream3Stack of
        entry :: rest ->
            -- Stream 3: write ZSCII 13 (newline) to memory table
            let
                mem =
                    Memory.writeByte (entry.tableAddr + 2 + entry.count) 13 machine.memory
            in
            { machine
                | memory = mem
                , stream3Stack = { entry | count = entry.count + 1 } :: rest
            }

        [] ->
            case machine.currentWindow of
                Lower ->
                    appendOutput (PrintText "\n") machine

                Upper ->
                    { machine | upperWindow = Window.newLine machine.upperWindow }


charToZscii : Char -> Maybe Int
charToZscii ch =
    if ch == '\n' then
        Just 13

    else
        let
            code =
                Char.toCode ch
        in
        if code >= 32 && code <= 126 then
            Just code

        else
            Nothing


{-| Split the screen, allocating lines for the upper window.
In V5+, this resets the upper window cursor to (1,1).
-}
splitWindow : Int -> ZMachine -> ZMachine
splitWindow height machine =
    { machine | upperWindow = Window.split height machine.upperWindow }


{-| Switch to the given window. Entering the upper window starts a new
status-bar draw, so the captured "first print_obj" marker is reset.
-}
setWindow : Window -> ZMachine -> ZMachine
setWindow win machine =
    case win of
        Upper ->
            let
                uw =
                    machine.upperWindow
            in
            { machine
                | currentWindow = Upper
                , upperWindow = { uw | firstPrintedObj = 0 }
            }

        Lower ->
            { machine | currentWindow = Lower }


{-| Set the cursor position in the upper window (1-based row and column).
-}
setCursor : Int -> Int -> ZMachine -> ZMachine
setCursor row col machine =
    { machine | upperWindow = Window.setCursor row col machine.upperWindow }


{-| Get the current cursor position as (row, col), 1-based.
-}
getCursor : ZMachine -> ( Int, Int )
getCursor machine =
    Window.getCursor machine.upperWindow


{-| Erase a window. For the upper window (1, -1, -2), clears the grid.
-}
eraseWindow : Int -> ZMachine -> ZMachine
eraseWindow win machine =
    { machine | upperWindow = Window.eraseWindow win machine.upperWindow }


{-| Erase from the cursor to the end of the current line in the upper
window (when value is 1). Fills with spaces.
-}
eraseLine : Int -> ZMachine -> ZMachine
eraseLine value machine =
    { machine | upperWindow = Window.eraseLine value machine.upperWindow }


{-| Render the upper window grid into a ShowStatusLine event and
append it to the output list. The grid content is preserved (a real
terminal keeps upper window content visible).
-}
flushUpperWindow : ZMachine -> ZMachine
flushUpperWindow machine =
    let
        rows =
            Window.render machine.upperWindow

        ( locId, locName ) =
            case Player.currentLocation machine.memory machine.playerTracking of
                ( 0, _ ) ->
                    -- Inform fallback: the first `print_obj` during an
                    -- upper-window draw identifies the current location.
                    let
                        obj =
                            machine.upperWindow.firstPrintedObj
                    in
                    if obj == 0 then
                        ( 0, "" )

                    else
                        ( obj, ObjectTable.shortName obj machine.memory )

                result ->
                    result
    in
    case rows of
        [] ->
            machine

        _ ->
            appendOutput
                (ShowStatusLine
                    { locationId = locId
                    , locationName = locName
                    , mode = ScreenRows rows
                    }
                )
                machine

