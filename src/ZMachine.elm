module ZMachine exposing
    ( ZMachine
    , StepResult
    , OutputEvent
    , LineInputInfo
    , ZMachineError
    , StatusLine
    , Window
    , Snapshot
    , load
    , step
    , runSteps
    , provideInput
    , provideChar
    , provideSaveResult
    , provideRestoreResult
    , snapshot
    , restoreSnapshot
    , storyRelease
    , storySerial
    , storyChecksum
    )

{-| A pure Elm Z-Machine interpreter for interactive fiction.
Supports version 3 story files fully and version 5 partially — see
the package README for the current v5 coverage.

This module is the main entry point for the library. All functions needed
to load and run a `.z3` or `.z5` story file are here. For pattern
matching on result types, also import the constructors from
[`ZMachine.Types`](ZMachine-Types):

    import ZMachine exposing (load, runSteps, provideInput)
    import ZMachine.Types exposing (StepResult(..), OutputEvent(..))

Each [`StepResult`](ZMachine-Types#StepResult) variant carries the
output events accumulated during the call that produced it, so the
host never needs to inspect the machine's output buffer directly.


# Types

@docs ZMachine, StepResult, OutputEvent, LineInputInfo, ZMachineError, StatusLine, Window, Snapshot


# Loading

@docs load


# Running

@docs step, runSteps, provideInput, provideChar


# Save and restore

See [`ZMachine.Snapshot`](ZMachine-Snapshot) for building custom save
schemes. A standard portable Quetzal codec is planned but not yet
shipped in this release.

@docs snapshot, restoreSnapshot, provideSaveResult, provideRestoreResult


# Story identity

@docs storyRelease, storySerial, storyChecksum

-}

import Bytes exposing (Bytes)
import ZMachine.Header as Header
import ZMachine.Memory as Memory
import ZMachine.Run as Run
import ZMachine.Snapshot as Snapshot
import ZMachine.State as State
import ZMachine.Types


{-| The complete state of a running Z-Machine.
-}
type alias ZMachine =
    ZMachine.Types.ZMachine


{-| Result of executing one or more instructions. See
[`ZMachine.Types.StepResult`](ZMachine-Types#StepResult) for the
constructors you will pattern match on.
-}
type alias StepResult =
    ZMachine.Types.StepResult


{-| Structured output events for the host to render. See
[`ZMachine.Types.OutputEvent`](ZMachine-Types#OutputEvent) for the full
list of constructors.
-}
type alias OutputEvent =
    ZMachine.Types.OutputEvent


{-| Metadata about a line-input request. Carried by the `NeedInput`
variant of `StepResult`.
-}
type alias LineInputInfo =
    ZMachine.Types.LineInputInfo


{-| Errors that can halt the machine.
-}
type alias ZMachineError =
    ZMachine.Types.ZMachineError


{-| Status line data including location name, score, and turns.
-}
type alias StatusLine =
    ZMachine.Types.StatusLine



{-| Screen window identifier (`Upper` or `Lower`).
-}
type alias Window =
    ZMachine.Types.Window


{-| Opaque snapshot of machine state. See
[`ZMachine.Snapshot`](ZMachine-Snapshot) for operations.
-}
type alias Snapshot =
    ZMachine.Types.Snapshot


{-| Load a `.z3` story file from raw bytes and initialize the machine.

    case ZMachine.load storyBytes of
        Ok machine ->
            -- ready to run
            ...

        Err err ->
            -- invalid story file
            ...

-}
load : Bytes -> Result String ZMachine
load bytes =
    Memory.fromBytes bytes
        |> Result.map State.init


{-| Execute a single instruction. Returns a `StepResult` indicating
whether the machine can continue, needs input, halted, or hit an error.

    case ZMachine.step machine of
        Continue nextMachine ->
            ...

        NeedInput request machineWithOutput ->
            ...

-}
step : ZMachine -> StepResult
step =
    Run.step


{-| Execute up to `n` instructions, stopping early if input is needed,
the machine halts, or an error occurs.

    case ZMachine.runSteps 10000 machine of
        Continue nextMachine ->
            -- budget exhausted, call runSteps again to keep going
            ...

        NeedInput request machineWithOutput ->
            -- prompt the player for input
            ...

-}
runSteps : Int -> ZMachine -> StepResult
runSteps =
    Run.runSteps


{-| Provide a line of input to a machine that returned `NeedInput`.

Writes the text into the story's text buffer, tokenizes it, and
resumes execution. In V5, also stores the terminating character (13
for newline).

    case ZMachine.runSteps 10000 machine of
        NeedInput info _ machineWithOutput ->
            ZMachine.provideInput "open mailbox" info machineWithOutput

        _ ->
            ...

-}
provideInput : String -> LineInputInfo -> ZMachine -> StepResult
provideInput =
    Run.provideInput


{-| Provide a character to a machine that returned `NeedChar`.

Pass a single-character string — only the first character is used.
The ZSCII code is stored as the result of the `read_char` opcode.

    case ZMachine.runSteps 10000 machine of
        NeedChar _ machineWithOutput ->
            ZMachine.provideChar "y" machineWithOutput

        _ ->
            ...

-}
provideChar : String -> ZMachine -> StepResult
provideChar =
    Run.provideChar


{-| Capture a snapshot of the current machine state. Safe to call any
time the host is holding a `ZMachine` between steps — useful for
autosaves or quick-save UIs. The resulting snapshot is tagged
`ResumeAt`, meaning on restore execution picks up at the next
instruction.

To persist a snapshot, serialize it with
[`ZMachine.Snapshot.encode`](ZMachine-Snapshot#encode). The built-in
codec is a native format that round-trips cleanly but isn't
cross-interpreter portable; a Quetzal codec for interop is planned.

-}
snapshot : ZMachine -> Snapshot
snapshot machine =
    Snapshot.capture
        { memory = machine.memory
        , pc = machine.pc
        , stack = machine.stack
        , callStack = machine.callStack
        , resumeKind = Snapshot.ResumeAt
        }


{-| Rehydrate a running machine from a snapshot. Used to load an
autosave at startup (or any time the host wants to swap state outside
of a `restore` opcode).

Fails with a [`RestoreError`](ZMachine-Snapshot#RestoreError) if the
snapshot is for a different story or its dynamic-memory image is
corrupt.

-}
restoreSnapshot : Snapshot -> ZMachine -> Result Snapshot.RestoreError ZMachine
restoreSnapshot snap machine =
    Snapshot.restore snap machine.originalMemory
        |> Result.map
            (\parts ->
                { machine
                    | memory = parts.memory
                    , pc = parts.pc
                    , stack = parts.stack
                    , callStack = parts.callStack
                }
            )


{-| Resume a machine that returned `NeedSave`. Pass `True` if the host
successfully persisted the snapshot, `False` otherwise. The `save`
opcode's branch is evaluated against the result.

    case ZMachine.runSteps 10000 machine of
        NeedSave snap m ->
            writeToFile (ZMachine.Snapshot.encode snap)
                |> Task.map (\_ -> ZMachine.provideSaveResult True m)

-}
provideSaveResult : Bool -> ZMachine -> StepResult
provideSaveResult =
    Run.provideSaveResult


{-| Resume a machine that returned `NeedRestore`. Pass `Just snapshot`
to rehydrate from the snapshot (the original `save` opcode's branch is
taken as true); pass `Nothing` to report failure (the `restore` opcode
branches false and execution continues normally). Mismatched-story
snapshots are treated as failures.
-}
provideRestoreResult : Maybe Snapshot -> ZMachine -> StepResult
provideRestoreResult =
    Run.provideRestoreResult


{-| The story file's release number (header word 0x02).

Combined with [`storySerial`](#storySerial) and [`storyChecksum`](#storyChecksum)
this uniquely identifies the story file edition — useful for tagging
snapshots so a host can reject loading a save against the wrong story.

-}
storyRelease : ZMachine -> Int
storyRelease machine =
    Header.releaseNumber machine.memory


{-| The story file's serial number — six ASCII bytes at header 0x12.
Infocom convention is a `YYMMDD` build date.
-}
storySerial : ZMachine -> String
storySerial machine =
    Header.serialNumber machine.memory


{-| The story file's checksum (header word 0x1C). Sum of all bytes
from 0x40 onward, mod 65536. Returns 0 for early V3 files that predate
the checksum field.
-}
storyChecksum : ZMachine -> Int
storyChecksum machine =
    Header.checksum machine.memory
