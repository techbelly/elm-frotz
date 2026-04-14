module ZMachine.Types exposing
    ( ZMachine
    , UpperWindow
    , Memory
    , CallFrame
    , Snapshot
    , StepResult(..)
    , OutputEvent(..)
    , LineInputInfo
    , ZMachineError(..)
    , StatusLine
    , StatusLineMode(..)
    , Window(..)
    )

{-| Types for the Z-Machine interpreter.

Import this module to pattern match on result types:

    import ZMachine.Types exposing (StepResult(..), OutputEvent(..))


# Machine State

@docs ZMachine, Memory, CallFrame


# Step Results

@docs StepResult, ZMachineError, LineInputInfo


# Output

@docs OutputEvent, StatusLine, Window

-}

import Array exposing (Array)
import Dict exposing (Dict)
import ZMachine.Memory
import ZMachine.Snapshot
import ZMachine.Stack


{-| Tracking state for identifying which global variable holds the
current player object. See `ZMachine.Player` for the algorithm.
-}
type alias PlayerTracking =
    { playerObject : Int
    , globalCandidates : Dict Int Int
    }


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
    , stream3Stack : List { tableAddr : Int, count : Int }
    , randomState : { seed : Int, count : Int }
    , currentWindow : Window
    , upperWindow : UpperWindow
    , playerTracking : PlayerTracking
    }


{-| Virtual upper window state. V5 games draw the status line by
writing directly to the upper window with cursor positioning. The
interpreter tracks this internally and emits a `ShowStatusLine` event
when the game switches back to the lower window.
-}
type alias UpperWindow =
    { height : Int
    , rows : Dict Int (Array Char)
    , cursorRow : Int
    , cursorCol : Int
    , width : Int
    , firstPrintedObj : Int
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
  - `NeedChar` — the machine is waiting for a single keypress.
    Call [`ZMachine.provideChar`](ZMachine#provideChar) to resume.
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
    | NeedInput LineInputInfo (List OutputEvent) ZMachine
    | NeedChar (List OutputEvent) ZMachine
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


{-| Metadata about a line-input request from `sread`/`aread`.
Carried by the `NeedInput` variant of `StepResult`.
-}
type alias LineInputInfo =
    { maxLength : Int
    , textBufferAddr : Int
    , parseBufferAddr : Int
    }


{-| Structured output events for the host to render.

  - `PrintText` — display a string.
  - `NewLine` — insert a line break.
  - `ShowStatusLine` — update the status bar with a [`StatusLine`](#StatusLine).
  - `SetBufferMode` — enable or disable word-wrap buffering.
  - `SetTextStyle` — change text style (bold, italic, etc.).
  - `SetColour` — change foreground/background colours.
  - `PlaySound` — play a sound effect by number.

-}
type OutputEvent
    = PrintText String
    | NewLine
    | ShowStatusLine StatusLine
    | SetBufferMode Bool
    | SetTextStyle Int
    | SetColour Int Int
    | PlaySound Int


{-| How the status line content is provided.

  - `ScoreAndTurns score turns` — V3 score game.
  - `TimeOfDay hours minutes` — V3 time game (hours 0–23, minutes 0–59).
  - `ScreenRows rows` — V5+ game that draws its own status area.
    Each string is one rendered row of the upper window, trimmed.

-}
type StatusLineMode
    = ScoreAndTurns Int Int
    | TimeOfDay Int Int
    | ScreenRows (List String)


{-| Status line data shown at the top of the screen.

  - `locationId` — the object number of the current location.
  - `locationName` — the short name of the current location.
  - `mode` — score/turns, time, or raw screen rows depending on version.

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
