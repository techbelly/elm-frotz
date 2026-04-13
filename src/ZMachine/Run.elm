module ZMachine.Run exposing
    ( step
    , runSteps
    , provideInput
    , provideChar
    , provideSaveResult
    , provideRestoreResult
    )

{-| High-level run loop for the Z-Machine.

@docs step, runSteps, provideInput, provideChar, provideSaveResult, provideRestoreResult

-}

import ZMachine.Decode as Decode
import ZMachine.Dictionary as Dictionary
import ZMachine.Execute as Execute
import ZMachine.Memory as Memory
import ZMachine.Snapshot as Snapshot exposing (Snapshot)
import ZMachine.State as State
import ZMachine.Types
    exposing
        ( LineInputInfo
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


{-| Provide a line of input to a machine that returned `NeedInput`.

Writes the input to the text buffer, tokenizes it, writes parse results,
and resumes execution.

-}
provideInput : String -> LineInputInfo -> ZMachine -> StepResult
provideInput input info machine =
    let
        truncated =
            String.left info.maxLength input

        mem =
            Dictionary.tokenize truncated info.textBufferAddr info.parseBufferAddr machine.memory

        instr =
            Decode.decode machine.pc machine.memory

        nextPC =
            machine.pc + instr.length

        version =
            (Memory.profile machine.memory).version

        advanced =
            { machine | memory = mem, pc = nextPC }
    in
    case version of
        Memory.V5 ->
            -- V5 aread stores the terminating character (13 = newline)
            drain (Execute.storeInstr instr 13 advanced)

        Memory.V3 ->
            Continue [] advanced


{-| Provide a character to a machine that returned `NeedChar`.

Pass a single-character string. The ZSCII code is stored into the
result variable of the `read_char` instruction.

-}
provideChar : String -> ZMachine -> StepResult
provideChar input machine =
    let
        charCode =
            String.uncons input
                |> Maybe.map (Tuple.first >> Char.toCode)
                |> Maybe.withDefault 13

        instr =
            Decode.decode machine.pc machine.memory

        nextPC =
            machine.pc + instr.length
    in
    drain (Execute.storeInstr instr charCode { machine | pc = nextPC })


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
    let
        version =
            (Memory.profile machine.memory).version
    in
    case version of
        Memory.V5 ->
            drain (Execute.resumeWithStore (if success then 1 else 0) machine)

        Memory.V3 ->
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

                        Snapshot.ResumeByStoreResult ->
                            drain (Execute.resumeWithStore 2 restored)



-- OUTCOME → STEPRESULT TRANSLATION


{-| Convert an interpreter `Outcome` into the public `StepResult`,
draining the machine's output buffer into the variant. `machine.output`
accumulates newest-first (so `State.appendOutput` is O(1)); we reverse
it on the way out so the host sees events in chronological order, and
blank the buffer on the returned machine so nothing is delivered twice.
-}
drain : Execute.Outcome -> StepResult
drain outcome =
    let
        drainMachine m =
            let
                flushed =
                    State.flushUpperWindow m
            in
            ( List.reverse flushed.output, { flushed | output = [] } )
    in
    case outcome of
        Execute.Continue m ->
            let ( events, cleaned ) = drainMachine m in
            Continue events cleaned

        Execute.NeedInput info m ->
            let ( events, cleaned ) = drainMachine m in
            NeedInput info events cleaned

        Execute.NeedChar m ->
            let ( events, cleaned ) = drainMachine m in
            NeedChar events cleaned

        Execute.NeedSave snap m ->
            let ( events, cleaned ) = drainMachine m in
            NeedSave snap events cleaned

        Execute.NeedRestore m ->
            let ( events, cleaned ) = drainMachine m in
            NeedRestore events cleaned

        Execute.Halted m ->
            let ( events, cleaned ) = drainMachine m in
            Halted events cleaned

        Execute.Error err m ->
            let ( events, cleaned ) = drainMachine m in
            Error err events cleaned
