module ZMachine.Types exposing
    ( ZMachine
    , Memory
    , CallFrame
    , Snapshot
    , StepResult(..)
    , OutputEvent(..)
    , InputRequest(..)
    , ZMachineError(..)
    , StatusLine
    , StatusLineMode(..)
    , Window(..)
    )

{-| Types for the Z-Machine interpreter.

Import this module to pattern match on result types:

    import ZMachine.Types exposing (StepResult(..), OutputEvent(..), InputRequest(..))


# Machine State

@docs ZMachine, Memory, CallFrame


# Step Results

@docs StepResult, ZMachineError, InputRequest


# Output

@docs OutputEvent, StatusLine, Window

-}

import ZMachine.Memory
import ZMachine.Snapshot
import ZMachine.Stack


{-| Opaque Z-Machine memory image, re-exported so consumers can annotate
[`ZMachine`](#ZMachine) fields without a second import.
-}
type alias Memory =
    ZMachine.Memory.Memory


{-| A single call stack frame, re-exported so consumers can annotate
[`ZMachine`](#ZMachine) fields without a second import.
-}
type alias CallFrame =
    ZMachine.Stack.CallFrame


{-| Opaque snapshot of Z-Machine state, produced by the save path and
consumed by the restore path. See [`ZMachine.Snapshot`](ZMachine-Snapshot)
for operations.
-}
type alias Snapshot =
    ZMachine.Snapshot.Snapshot


{-| The complete state of a running Z-Machine. You will receive this
inside each [`StepResult`](#StepResult) variant.
-}
type alias ZMachine =
    { memory : Memory
    , originalMemory : Memory
    , pc : Int
    , stack : List Int
    , callStack : List CallFrame
    , output : List OutputEvent
    , outputStreams : { stream1 : Bool, stream2 : Bool }
    , randomState : { seed : Int, count : Int }
    }


{-| Result of executing one or more instructions.

Every variant carries a `List OutputEvent` containing the output
accumulated during the call that produced this result, followed by the
current `ZMachine`. The returned machine's output buffer is always
empty — the run-loop drains it into the variant, so callers never need
to manage output state themselves.

  - `Continue` — step budget exhausted, call
    [`ZMachine.runSteps`](ZMachine#runSteps) again to keep going.
  - `NeedInput` — the machine is waiting for a line of player input.
    Call [`ZMachine.provideInput`](ZMachine#provideInput) to resume.
  - `NeedSave` — the story executed a `save` opcode. The attached
    snapshot captures the state to be persisted; call
    [`ZMachine.provideSaveResult`](ZMachine#provideSaveResult) to resume.
  - `NeedRestore` — the story executed a `restore` opcode. Call
    [`ZMachine.provideRestoreResult`](ZMachine#provideRestoreResult)
    with a snapshot (or `Nothing` on failure) to resume.
  - `Halted` — the story called `quit`.
  - `Error` — an unrecoverable error occurred.

-}
type StepResult
    = Continue (List OutputEvent) ZMachine
    | NeedInput InputRequest (List OutputEvent) ZMachine
    | NeedSave Snapshot (List OutputEvent) ZMachine
    | NeedRestore (List OutputEvent) ZMachine
    | Halted (List OutputEvent) ZMachine
    | Error ZMachineError (List OutputEvent) ZMachine


{-| Errors that can halt the machine.

  - `DivisionByZero` — a `div` or `mod` instruction divided by zero.
  - `StackUnderflow` — a pop from an empty evaluation stack.
  - `InvalidOpcode` — an unrecognised opcode number.
  - `InvalidVariable` — a variable reference outside 0–255.

-}
type ZMachineError
    = DivisionByZero
    | StackUnderflow
    | InvalidOpcode Int
    | InvalidVariable Int


{-| What kind of input the machine needs. Currently only line input is
supported (the `sread` opcode in Z-Machine v3).

    case result of
        NeedInput (LineInput info) machine ->
            -- prompt the player, then call ZMachine.provideInput
            ...

-}
type InputRequest
    = LineInput
        { maxLength : Int
        , textBufferAddr : Int
        , parseBufferAddr : Int
        }


{-| Structured output events for the host to render.

  - `PrintText` — display a string.
  - `NewLine` — insert a line break.
  - `ShowStatusLine` — update the status bar with a [`StatusLine`](#StatusLine).
  - `SplitWindow` — split the screen (number of lines for the upper window).
  - `SetWindow` — switch to a [`Window`](#Window).
  - `EraseWindow` — erase a window (−1 = unsplit and clear, −2 = clear all).
  - `SetCursor` — move the cursor (row, column) in the upper window.
  - `SetBufferMode` — enable or disable word-wrap buffering.
  - `PlaySound` — play a sound effect by number.

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


{-| Whether the game tracks score/turns or a clock.

  - `ScoreAndTurns score turns` — a score game.
  - `TimeOfDay hours minutes` — a time game (hours 0–23, minutes 0–59).

-}
type StatusLineMode
    = ScoreAndTurns Int Int
    | TimeOfDay Int Int


{-| Status line data shown at the top of the screen.

  - `locationId` — the object number of the current location.
  - `locationName` — the short name of the current location.
  - `mode` — score/turns or time, depending on the game.

-}
type alias StatusLine =
    { locationId : Int
    , locationName : String
    , mode : StatusLineMode
    }


{-| Screen window identifier. Z-Machine v3 has two windows:

  - `Upper` — the fixed-size status area at the top.
  - `Lower` — the scrolling main text area.

-}
type Window
    = Upper
    | Lower
