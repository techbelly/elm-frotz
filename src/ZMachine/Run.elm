module ZMachine.Run exposing
    ( step
    , runSteps
    , provideInput
    , provideSaveResult
    , provideRestoreResult
    )

{-| High-level run loop for the Z-Machine.

@docs step, runSteps, provideInput, provideSaveResult, provideRestoreResult

-}

import ZMachine.Decode as Decode
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
step machine =
    drain (Execute.step machine)


{-| Execute up to `n` instructions, stopping early if input is needed,
the machine halts, or an error occurs. Output accumulated across all
iterations is drained into the returned variant.
-}
runSteps : Int -> ZMachine -> StepResult
runSteps n machine =
    if n <= 0 then
        drain (Execute.Continue machine)

    else
        case Execute.step machine of
            Execute.Continue next ->
                runSteps (n - 1) next

            other ->
                drain other


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

                -- Advance PC past the sread instruction (PC still points at it)
                instr =
                    Decode.decode machine.pc machine.memory

                nextPC =
                    machine.pc + instr.length
            in
            Continue [] { machine | memory = mem, pc = nextPC }


{-| Resume a machine that returned `NeedSave`. Pass `True` if the host
successfully persisted the snapshot, `False` otherwise — the `save`
instruction's branch is then evaluated against that result, matching
the Z-Machine v3 convention that `save` branches on success.

    case ZMachine.runSteps 10000 machine of
        NeedSave snapshot _ m ->
            -- write snapshot to storage, then:
            ZMachine.provideSaveResult True m

-}
provideSaveResult : Bool -> ZMachine -> StepResult
provideSaveResult success machine =
    drain (Execute.resumeWithBranch success machine)


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
            drain (Execute.resumeWithBranch False machine)

        Just snap ->
            case Snapshot.restore snap machine.originalMemory of
                Err _ ->
                    drain (Execute.resumeWithBranch False machine)

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
                            Continue [] restored

                        Snapshot.ResumeByBranchTrue ->
                            drain (Execute.resumeWithBranch True restored)



-- OUTCOME → STEPRESULT TRANSLATION


{-| Convert an interpreter `Outcome` into the public `StepResult`,
draining the machine's output buffer into the variant. `machine.output`
accumulates newest-first (so `State.appendOutput` is O(1)); we reverse
it on the way out so the host sees events in chronological order, and
blank the buffer on the returned machine so nothing is delivered twice.
-}
drain : Execute.Outcome -> StepResult
drain outcome =
    case outcome of
        Execute.Continue m ->
            Continue (List.reverse m.output) (clearOutput m)

        Execute.NeedInput req m ->
            NeedInput req (List.reverse m.output) (clearOutput m)

        Execute.NeedSave snap m ->
            NeedSave snap (List.reverse m.output) (clearOutput m)

        Execute.NeedRestore m ->
            NeedRestore (List.reverse m.output) (clearOutput m)

        Execute.Halted m ->
            Halted (List.reverse m.output) (clearOutput m)

        Execute.Error err m ->
            Error err (List.reverse m.output) (clearOutput m)


clearOutput : ZMachine -> ZMachine
clearOutput machine =
    { machine | output = [] }
