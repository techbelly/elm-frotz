module ZMachine exposing
    ( ZMachine
    , StepResult
    , OutputEvent
    , InputRequest
    , ZMachineError
    , StatusLine
    , Window
    , load
    , step
    , runSteps
    , provideInput
    , getOutput
    , clearOutput
    )

{-| A pure Elm Z-Machine version 3 interpreter for interactive fiction.

This is the main entry point for the library. All functions needed to
load and run a `.z3` story file are here.

For pattern matching on result types, also import the constructors:

    import ZMachine exposing (load, runSteps, provideInput, clearOutput, getOutput)
    import ZMachine.State
        exposing
            ( StepResult(..)
            , OutputEvent(..)
            , InputRequest(..)
            , ZMachineError(..)
            )


## Loading

@docs load


## Running

@docs step, runSteps, provideInput


## Output

@docs getOutput, clearOutput


## Types

@docs ZMachine, StepResult, OutputEvent, InputRequest, ZMachineError, StatusLine, Window

-}

import Bytes exposing (Bytes)
import ZMachine.Memory as Memory
import ZMachine.Run as Run
import ZMachine.State as State
import ZMachine.Types


{-| The complete state of a running Z-Machine.
-}
type alias ZMachine =
    ZMachine.Types.ZMachine


{-| Result of executing one or more instructions.

  - `Continue` — step budget exhausted; call `runSteps` again to keep going.
  - `NeedInput` — the machine is waiting for a line of input from the player.
  - `Halted` — the story called `quit`.
  - `Error` — an unrecoverable error occurred.

-}
type alias StepResult =
    ZMachine.Types.StepResult


{-| Structured output events for the host to render.
-}
type alias OutputEvent =
    ZMachine.Types.OutputEvent


{-| What kind of input the machine needs.
-}
type alias InputRequest =
    ZMachine.Types.InputRequest


{-| Errors that can halt the machine.
-}
type alias ZMachineError =
    ZMachine.Types.ZMachineError


{-| Status line data (location, score, turns).
-}
type alias StatusLine =
    ZMachine.Types.StatusLine


{-| Screen window identifier.
-}
type alias Window =
    ZMachine.Types.Window


{-| Load a `.z3` story file from raw bytes and initialize the machine.

    case ZMachine.load storyBytes of
        Ok machine ->
            -- ready to run

        Err err ->
            -- invalid story file

-}
load : Bytes -> Result String ZMachine
load bytes =
    Memory.fromBytes bytes
        |> Result.map State.init


{-| Execute a single instruction.
-}
step : ZMachine -> StepResult
step =
    Run.step


{-| Execute up to `n` instructions, stopping early if input is needed,
the machine halts, or an error occurs.
-}
runSteps : Int -> ZMachine -> StepResult
runSteps =
    Run.runSteps


{-| Provide a line of input to a machine waiting for input.
-}
provideInput : String -> InputRequest -> ZMachine -> StepResult
provideInput =
    Run.provideInput


{-| Get all pending output events.
-}
getOutput : ZMachine -> List OutputEvent
getOutput =
    Run.getOutput


{-| Clear pending output events.
-}
clearOutput : ZMachine -> ZMachine
clearOutput =
    Run.clearOutput
