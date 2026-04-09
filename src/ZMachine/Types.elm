module ZMachine.Types exposing
    ( ZMachine
    , StepResult(..)
    , OutputEvent(..)
    , InputRequest(..)
    , ZMachineError(..)
    , StatusLine
    , Window(..)
    )

{-| Public types for the Z-Machine interpreter.

Import this module for pattern matching on result types:

    import ZMachine.Types exposing (StepResult(..), OutputEvent(..), InputRequest(..), ZMachineError(..))

@docs ZMachine, StepResult, OutputEvent, InputRequest, ZMachineError, StatusLine, Window

-}

import ZMachine.Memory exposing (Memory)
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

  - `Continue` — step budget exhausted; call `runSteps` again to keep going.
  - `NeedInput` — the machine is waiting for a line of input from the player.
  - `Halted` — the story called `quit`.
  - `Error` — an unrecoverable error occurred.

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
