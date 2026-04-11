module ZMachine.Run exposing
    ( step
    , runSteps
    , provideInput
    , provideSaveResult
    , provideRestoreResult
    , getOutput
    , clearOutput
    )

{-| High-level run loop for the Z-Machine.

@docs step, runSteps, provideInput, provideSaveResult, provideRestoreResult, getOutput, clearOutput

-}

import ZMachine.Dictionary as Dictionary
import ZMachine.Execute as Execute
import ZMachine.Snapshot as Snapshot exposing (Snapshot)
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


{-| Resume a machine that returned `NeedSave`. Pass `True` if the host
successfully persisted the snapshot, `False` otherwise — the `save`
instruction's branch is then evaluated against that result, matching
the Z-Machine v3 convention that `save` branches on success.

    case ZMachine.runSteps 10000 machine of
        NeedSave snapshot m ->
            -- write snapshot to storage, then:
            ZMachine.provideSaveResult True m

-}
provideSaveResult : Bool -> ZMachine -> StepResult
provideSaveResult =
    Execute.resumeWithBranch


{-| Resume a machine that returned `NeedRestore`. Pass `Just snapshot`
to rehydrate state from the snapshot (the `save` instruction that
created it will have its branch taken as true); pass `Nothing` to
report failure, in which case the `restore` instruction branches false
and execution continues normally.

A snapshot that belongs to a different story (mismatched release /
serial / checksum) is treated as a failure — the restore branches false
rather than raising an error, matching how other interpreters behave
when given a bad save file.

-}
provideRestoreResult : Maybe Snapshot -> ZMachine -> StepResult
provideRestoreResult maybeSnap machine =
    case maybeSnap of
        Nothing ->
            Execute.resumeWithBranch False machine

        Just snap ->
            case Snapshot.restore snap machine.originalMemory of
                Err _ ->
                    Execute.resumeWithBranch False machine

                Ok parts ->
                    let
                        restored =
                            { machine
                                | memory = parts.memory
                                , pc = parts.pc
                                , stack = parts.stack
                                , callStack = parts.callStack
                            }
                    in
                    case Snapshot.resumeKind snap of
                        Snapshot.ResumeAt ->
                            Continue restored

                        Snapshot.ResumeByBranchTrue ->
                            Execute.resumeWithBranch True restored


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
