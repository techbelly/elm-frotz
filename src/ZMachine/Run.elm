module ZMachine.Run exposing (step, runSteps, provideInput, getOutput, clearOutput)

{-| High-level run loop for the Z-Machine.

@docs step, runSteps, provideInput, getOutput, clearOutput

-}

import ZMachine.Dictionary as Dictionary
import ZMachine.Execute as Execute
import ZMachine.Types
    exposing
        ( InputRequest(..)
        , StepResult(..)
        , ZMachine
        )


{-| Execute a single instruction.
-}
step : ZMachine -> StepResult
step =
    Execute.step


{-| Execute up to `n` instructions, stopping early if input is needed,
the machine halts, or an error occurs.
-}
runSteps : Int -> ZMachine -> StepResult
runSteps n machine =
    if n <= 0 then
        Continue machine

    else
        case Execute.step machine of
            Continue next ->
                runSteps (n - 1) next

            other ->
                other


{-| Provide a line of input to a machine waiting for input.

Writes the input to the text buffer, tokenizes it, writes parse results,
and resumes execution.

-}
provideInput : String -> InputRequest -> ZMachine -> StepResult
provideInput input request machine =
    case request of
        LineInput info ->
            let
                -- Truncate to max length
                truncated =
                    String.left info.maxLength input

                -- Write to text buffer and tokenize
                mem =
                    Dictionary.tokenize truncated info.textBufferAddr info.parseBufferAddr machine.memory
            in
            Continue { machine | memory = mem }


{-| Get all pending output events, in chronological order.

Events are stored newest-first internally (so `appendOutput` is O(1));
this reverses them on read. Consumers that touch `machine.output`
directly will see the reversed order — use this accessor instead.

-}
getOutput : ZMachine -> List ZMachine.Types.OutputEvent
getOutput machine =
    List.reverse machine.output


{-| Clear pending output events.
-}
clearOutput : ZMachine -> ZMachine
clearOutput machine =
    { machine | output = [] }
