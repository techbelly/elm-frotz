module ZMachine.Execute exposing (Outcome(..), step, resumeWithBranch, resumeWithStore, storeInstr)

{-| Z-Machine instruction execution.

Decodes and executes a single instruction, returning the resulting state.

@docs Outcome, step, resumeWithBranch

-}

import Array
import Bitwise
import Library.IntExtra
    exposing
        ( addInt16
        , divInt16
        , modInt16
        , mulInt16
        , subInt16
        , toSignedInt16
        , xorshift
        )
import Library.ListExtra exposing (getAt)
import ZMachine.Decode as Decode
import ZMachine.Dictionary as Dictionary
import ZMachine.Memory as Memory
import ZMachine.ObjectTable as ObjectTable
import ZMachine.Opcode as Opcode
    exposing
        ( BranchTarget(..)
        , Instruction
        , Op0(..)
        , Op1(..)
        , Op2(..)
        , OpExt(..)
        , OpVar(..)
        , Opcode(..)
        , Operand(..)
        , variableRefFromByte
        )
import ZMachine.Player as Player
import ZMachine.Snapshot as Snapshot
import ZMachine.State as State
import ZMachine.StatusLine as StatusLine
import ZMachine.Window as Window
import ZMachine.Text as Text
import ZMachine.Types as Types
    exposing
        ( LineInputInfo
        , ZMachine
        , ZMachineError(..)
        )


{-| Internal result of executing one instruction. The public
[`Outcome`](ZMachine-Types#Outcome) is assembled in
[`ZMachine.Run`](ZMachine-Run), which drains `machine.output` into the
variant so callers never see the internal output buffer. Keeping this
type private to the interpreter lets the 47 construction sites below
stay output-agnostic.
-}
type Outcome
    = Continue ZMachine
    | NeedInput LineInputInfo ZMachine
    | NeedChar ZMachine
    | NeedSave Snapshot.Snapshot ZMachine
    | NeedRestore ZMachine
    | Halted ZMachine
    | Error ZMachineError ZMachine


{-| Execute a single instruction at the current PC.
-}
step : ZMachine -> Outcome
step machine =
    let
        instr =
            Decode.decode machine.pc machine.memory

        nextPC =
            machine.pc + instr.length

        ( operandValues, machineAfterOperands ) =
            resolveOperands instr.operands machine
    in
    execute instr nextPC operandValues machineAfterOperands


{-| Re-decode the instruction at `machine.pc` and apply a branch with
the given boolean result, advancing past the instruction. Used by the
save/restore resumption paths in [`ZMachine.Run`](ZMachine-Run): the
machine was suspended mid-`save`/`restore`, and the host now reports
whether the operation succeeded.
-}
resumeWithBranch : Bool -> ZMachine -> Outcome
resumeWithBranch success machine =
    let
        instr =
            Decode.decode machine.pc machine.memory

        nextPC =
            machine.pc + instr.length

        m =
            { machine | pc = nextPC }
    in
    executeBranch instr success m


{-| Re-decode the instruction at `machine.pc` and store the given value
into its store target, advancing past the instruction. Used by the V5
ext\_save/ext\_restore resumption paths.
-}
resumeWithStore : Int -> ZMachine -> Outcome
resumeWithStore value machine =
    let
        instr =
            Decode.decode machine.pc machine.memory

        nextPC =
            machine.pc + instr.length

        m =
            { machine | pc = nextPC }
    in
    storeResult instr value m


{-| Store a value into the store target of an already-decoded instruction.
The machine's PC should already be advanced past the instruction. Used by
Run.elm for V5 aread (store terminating char) and read\_char.
-}
storeInstr : Opcode.Instruction -> Int -> ZMachine -> Outcome
storeInstr instr value machine =
    storeResult instr value machine



-- OPERAND RESOLUTION


resolveOperands : List Operand -> ZMachine -> ( List Int, ZMachine )
resolveOperands operands machine =
    let
        ( reversed, finalMachine ) =
            List.foldl
                (\op ( acc, m ) ->
                    let
                        ( val, m2 ) =
                            resolveOperand op m
                    in
                    ( val :: acc, m2 )
                )
                ( [], machine )
                operands
    in
    ( List.reverse reversed, finalMachine )


resolveOperand : Operand -> ZMachine -> ( Int, ZMachine )
resolveOperand operand machine =
    case operand of
        LargeConstant n ->
            ( n, machine )

        SmallConstant n ->
            ( n, machine )

        Variable ref ->
            State.readVariable ref machine



-- DISPATCH


execute : Instruction -> Int -> List Int -> ZMachine -> Outcome
execute instr nextPC ops machine =
    let
        m =
            { machine | pc = nextPC }

        arg0 =
            operandAt 0 ops

        arg1 =
            operandAt 1 ops

        arg2 =
            operandAt 2 ops
    in
    case instr.opcode of
        -- 2OP
        Op2 Je ->
            executeJe instr ops m

        Op2 Jl ->
            executeBranch instr (toSignedInt16 arg0 < toSignedInt16 arg1) m

        Op2 Jg ->
            executeBranch instr (toSignedInt16 arg0 > toSignedInt16 arg1) m

        Op2 DecChk ->
            executeIncDecCheck -1 (<) instr ops m

        Op2 IncChk ->
            executeIncDecCheck 1 (>) instr ops m

        Op2 Jin ->
            executeJin instr ops m

        Op2 Test ->
            executeBranch instr (Bitwise.and arg0 arg1 == arg1) m

        Op2 Or ->
            storeResult instr (Bitwise.or arg0 arg1) m

        Op2 And ->
            storeResult instr (Bitwise.and arg0 arg1) m

        Op2 TestAttr ->
            executeTestAttr instr ops m

        Op2 SetAttr ->
            executeAttrUpdate ObjectTable.setAttribute ops m

        Op2 ClearAttr ->
            executeAttrUpdate ObjectTable.clearAttribute ops m

        Op2 Store ->
            Continue (writeIndirect arg0 arg1 m)

        Op2 InsertObj ->
            executeInsertObj ops m

        Op2 Loadw ->
            storeResult instr (Memory.readWord (arg0 + Memory.wordLength * arg1) m.memory) m

        Op2 Loadb ->
            storeResult instr (Memory.readByte (arg0 + arg1) m.memory) m

        Op2 GetProp ->
            executeGetProp instr ops m

        Op2 GetPropAddr ->
            executeGetPropAddr instr ops m

        Op2 GetNextProp ->
            executeGetNextProp instr ops m

        Op2 Add ->
            storeResult instr (addInt16 arg0 arg1) m

        Op2 Sub ->
            storeResult instr (subInt16 arg0 arg1) m

        Op2 Mul ->
            storeResult instr (mulInt16 arg0 arg1) m

        Op2 Div ->
            if arg1 == 0 then
                Error DivisionByZero m

            else
                storeResult instr (divInt16 arg0 arg1) m

        Op2 Mod ->
            if arg1 == 0 then
                Error DivisionByZero m

            else
                storeResult instr (modInt16 arg0 arg1) m

        Op2 CallS2 ->
            executeCall instr arg0 [ arg1 ] m

        Op2 CallN2 ->
            executeCall instr arg0 [ arg1 ] m

        Op2 SetColour ->
            Continue (State.appendOutput (Types.SetColour arg0 arg1) m)

        Op2 Throw ->
            -- arg0 = return value, arg1 = frame cookie (stack depth from catch)
            let
                framesToDrop =
                    List.length m.callStack - arg1
            in
            executeReturn arg0 { m | callStack = List.drop framesToDrop m.callStack }

        Op2 (Opcode.Unknown2Op n) ->
            Error (InvalidOpcode n) m

        -- 1OP
        Op1 Jz ->
            executeBranch instr (arg0 == 0) m

        Op1 GetSibling ->
            executeGetTreeLink ObjectTable.sibling instr ops m

        Op1 GetChild ->
            executeGetTreeLink ObjectTable.child instr ops m

        Op1 GetParent ->
            executeGetParent instr ops m

        Op1 GetPropLen ->
            executeGetPropLen instr ops m

        Op1 Inc ->
            executeIncDec 1 ops m

        Op1 Dec ->
            executeIncDec -1 ops m

        Op1 PrintAddr ->
            let
                ( str, _ ) =
                    Text.decodeZString arg0 m.memory
            in
            Continue (State.outputObjectName str m)

        Op1 CallS1 ->
            executeCall instr arg0 [] m

        Op1 RemoveObj ->
            executeRemoveObj ops m

        Op1 PrintObj ->
            executePrintObj ops m

        Op1 Ret ->
            executeReturn arg0 m

        Op1 Jump ->
            Continue { m | pc = m.pc + toSignedInt16 arg0 - 2 }

        Op1 PrintPaddr ->
            let
                ( str, _ ) =
                    Text.decodeZString (Memory.unpackAddress arg0 m.memory) m.memory
            in
            Continue (State.outputText str m)

        Op1 Load ->
            let
                ( val, m2 ) =
                    readIndirect arg0 m
            in
            storeResult instr val m2

        Op1 Not ->
            storeResult instr (Bitwise.and (Bitwise.complement arg0) 0xFFFF) m

        Op1 CallN1 ->
            executeCall instr arg0 [] m

        Op1 (Opcode.Unknown1Op n) ->
            Error (InvalidOpcode n) m

        -- 0OP
        Op0 Rtrue ->
            executeReturn 1 m

        Op0 Rfalse ->
            executeReturn 0 m

        Op0 Print ->
            case instr.textLiteral of
                Just words ->
                    let
                        str =
                            Text.decodeZStringWords words m.memory
                    in
                    Continue (State.outputText str m)

                Nothing ->
                    Continue m

        Op0 PrintRet ->
            case instr.textLiteral of
                Just words ->
                    let
                        str =
                            Text.decodeZStringWords words m.memory

                        m2 =
                            m
                                |> State.outputText str
                                |> State.outputNewLine
                    in
                    executeReturn 1 m2

                Nothing ->
                    executeReturn 1 m

        Op0 Nop ->
            Continue m

        Op0 Save ->
            let
                resumeKind =
                    case (Memory.profile machine.memory).version of
                        Memory.V3 ->
                            Snapshot.ResumeByBranchTrue

                        Memory.V5 ->
                            Snapshot.ResumeByStoreResult

                snap =
                    Snapshot.capture
                        { memory = machine.memory
                        , pc = machine.pc
                        , stack = machine.stack
                        , callStack = machine.callStack
                        , resumeKind = resumeKind
                        }
            in
            NeedSave snap machine

        Op0 Restore ->
            -- Suspend execution; host supplies a snapshot via
            -- `provideRestoreResult`. We return `machine` (pc still at
            -- the restore opcode) so resumption can re-decode the
            -- instruction when the host reports failure (branch False).
            NeedRestore machine

        Op0 Restart ->
            let
                newMachine =
                    State.init m.originalMemory
            in
            Continue newMachine

        Op0 RetPopped ->
            let
                ( val, m2 ) =
                    State.popStack m
            in
            executeReturn val m2

        Op0 Pop ->
            case (Memory.profile machine.memory).version of
                Memory.V3 ->
                    let
                        ( _, m2 ) =
                            State.popStack m
                    in
                    Continue m2

                Memory.V5 ->
                    -- V5: this is "catch" — store call stack depth
                    storeResult instr (List.length m.callStack) m

        Op0 Quit ->
            Halted m

        Op0 NewLine ->
            Continue (State.outputNewLine m)

        Op0 ShowStatus ->
            executeShowStatus m

        Op0 Verify ->
            -- For now, always pass verification
            executeBranch instr True m

        Op0 Piracy ->
            -- Always branch (no piracy check)
            executeBranch instr True m

        Op0 (Opcode.Unknown0Op n) ->
            Error (InvalidOpcode n) m

        -- VAR
        OpVar Call ->
            executeCall instr arg0 (List.drop 1 ops) m

        OpVar Storew ->
            Continue { m | memory = Memory.writeWord (arg0 + Memory.wordLength * arg1) arg2 m.memory }

        OpVar Storeb ->
            Continue { m | memory = Memory.writeByte (arg0 + arg1) arg2 m.memory }

        OpVar PutProp ->
            executePutProp ops m

        OpVar Sread ->
            executeSread ops { m | pc = machine.pc }

        OpVar PrintChar ->
            Continue (State.outputText (String.fromChar (Text.zsciiToChar arg0)) m)

        OpVar PrintNum ->
            Continue (State.outputText (String.fromInt (toSignedInt16 arg0)) m)

        OpVar Random ->
            executeRandom instr ops m

        OpVar Push ->
            Continue (State.pushStack arg0 m)

        OpVar Pull ->
            let
                ( val, m2 ) =
                    State.popStack m
            in
            Continue (writeIndirect arg0 val m2)

        OpVar SplitWindow ->
            Continue (State.splitWindow arg0 m)

        OpVar SetWindow ->
            let
                win =
                    if arg0 == 0 then
                        Types.Lower

                    else
                        Types.Upper
            in
            Continue (State.setWindow win m)

        OpVar OutputStream ->
            let
                streamNum =
                    toSignedInt16 arg0
            in
            if streamNum == 3 then
                Continue (State.pushStream3 arg1 m)

            else if streamNum == -3 then
                Continue (State.popStream3 m)

            else if streamNum == 2 then
                Continue (setStream2 True m)

            else if streamNum == -2 then
                Continue (setStream2 False m)

            else
                Continue m

        OpVar InputStream ->
            -- Only stream 0 (keyboard) supported; ignore
            Continue m

        OpVar SoundEffect ->
            Continue (State.appendOutput (Types.PlaySound arg0) m)

        OpVar CallVs2 ->
            executeCall instr arg0 (List.drop 1 ops) m

        OpVar EraseWindow ->
            Continue (State.eraseWindow (toSignedInt16 arg0) m)

        OpVar EraseLine ->
            Continue (State.eraseLine arg0 m)

        OpVar SetCursor ->
            Continue (State.setCursor arg0 arg1 m)

        OpVar GetCursor ->
            let
                ( row, col ) =
                    State.getCursor m

                mem =
                    m.memory
                        |> Memory.writeWord arg0 row
                        |> Memory.writeWord (arg0 + Memory.wordLength) col
            in
            Continue { m | memory = mem }

        OpVar SetTextStyle ->
            Continue (State.appendOutput (Types.SetTextStyle arg0) m)

        OpVar BufferMode ->
            Continue (State.appendOutput (Types.SetBufferMode (arg0 /= 0)) m)

        OpVar ReadChar ->
            NeedChar { m | pc = machine.pc }

        OpVar ScanTable ->
            executeScanTable instr ops m

        OpVar NotV5 ->
            storeResult instr (Bitwise.and (Bitwise.complement arg0) 0xFFFF) m

        OpVar CallVn ->
            executeCall instr arg0 (List.drop 1 ops) m

        OpVar CallVn2 ->
            executeCall instr arg0 (List.drop 1 ops) m

        OpVar Tokenise ->
            executeTokenise ops m

        OpVar EncodeText ->
            -- Rarely used; store zeros as a safe no-op
            Continue m

        OpVar CopyTable ->
            executeCopyTable ops m

        OpVar PrintTable ->
            -- Minimal implementation: print as single-column text
            executePrintTable ops m

        OpVar CheckArgCount ->
            executeCheckArgCount instr ops m

        OpVar (Opcode.UnknownVar n) ->
            Error (InvalidOpcode n) m

        -- EXT
        OpExt ExtSave ->
            executeSave instr { m | pc = machine.pc }

        OpExt ExtRestore ->
            executeRestore instr { m | pc = machine.pc }

        OpExt LogShift ->
            executeLogShift instr ops m

        OpExt ArtShift ->
            executeArtShift instr ops m

        OpExt SetFont ->
            -- Font 1 (normal) always available; return previous font (1)
            storeResult instr 1 m

        OpExt SaveUndo ->
            -- No undo support: store -1 (failure)
            storeResult instr 0xFFFF m

        OpExt RestoreUndo ->
            -- No undo support: store 0 (failure)
            storeResult instr 0 m

        OpExt (Opcode.UnknownExt n) ->
            Error (InvalidOpcode n) m



-- BRANCH EXECUTION


executeBranch : Instruction -> Bool -> ZMachine -> Outcome
executeBranch instr conditionResult machine =
    case instr.branch of
        Just branch ->
            if branch.condition == conditionResult then
                case branch.target of
                    ReturnFalse ->
                        executeReturn 0 machine

                    ReturnTrue ->
                        executeReturn 1 machine

                    Offset offset ->
                        Continue { machine | pc = machine.pc + offset - 2 }

            else
                Continue machine

        Nothing ->
            Continue machine



-- STORE RESULT


storeResult : Instruction -> Int -> ZMachine -> Outcome
storeResult instr value machine =
    case instr.store of
        Just varRef ->
            Continue (State.writeVariable varRef value machine)

        Nothing ->
            Continue machine



-- CALL / RETURN


executeCall : Instruction -> Int -> List Int -> ZMachine -> Outcome
executeCall instr packedAddr args machine =
    if packedAddr == 0 then
        -- Calling address 0 returns false
        storeResult instr 0 machine

    else
        let
            p =
                Memory.profile machine.memory

            routineAddr =
                Memory.unpackAddress packedAddr machine.memory

            numLocals =
                Memory.readByte routineAddr machine.memory

            initialValues =
                if p.routineHasInitialValues then
                    -- V3: initial local values follow the count byte
                    List.range 1 numLocals
                        |> List.map
                            (\i ->
                                Memory.readWord (routineAddr + 1 + (i - 1) * Memory.wordLength) machine.memory
                            )

                else
                    -- V5+: locals are zero-initialised
                    List.repeat numLocals 0

            -- Override leading defaults with provided arguments
            localsWithArgs =
                mergeArgsWithDefaults args initialValues

            firstInstrAddr =
                if p.routineHasInitialValues then
                    routineAddr + 1 + numLocals * Memory.wordLength

                else
                    routineAddr + 1

            frame =
                { returnPC = machine.pc
                , returnStore = instr.store
                , locals = Array.fromList localsWithArgs
                , evalStack = machine.stack
                , argCount = List.length args
                }
        in
        Continue
            { machine
                | pc = firstInstrAddr
                , callStack = frame :: machine.callStack
                , stack = []
            }


executeReturn : Int -> ZMachine -> Outcome
executeReturn value machine =
    case machine.callStack of
        frame :: rest ->
            let
                m =
                    { machine
                        | pc = frame.returnPC
                        , callStack = rest
                        , stack = frame.evalStack
                    }
            in
            case frame.returnStore of
                Just varRef ->
                    Continue (State.writeVariable varRef value m)

                Nothing ->
                    Continue m

        [] ->
            -- Returning from top level — halt
            Halted machine



-- JE: branch if first operand equals any subsequent


executeJe : Instruction -> List Int -> ZMachine -> Outcome
executeJe instr ops machine =
    let
        a =
            operandAt 0 ops

        rest =
            List.drop 1 ops

        matches =
            List.member a rest
    in
    executeBranch instr matches machine



-- INC/DEC AND INC_CHK/DEC_CHK


{-| Adjust an indirect variable by `delta` and return the new signed value.
Shared by `inc`, `dec`, `inc_chk`, `dec_chk`.
-}
adjustIndirect : Int -> Int -> ZMachine -> ( Int, ZMachine )
adjustIndirect varNum delta machine =
    let
        ( current, m ) =
            readIndirect varNum machine

        newValue =
            toSignedInt16 current + delta
    in
    ( newValue, writeIndirect varNum newValue m )


executeIncDecCheck : Int -> (Int -> Int -> Bool) -> Instruction -> List Int -> ZMachine -> Outcome
executeIncDecCheck delta cmp instr ops machine =
    let
        checkValue =
            toSignedInt16 (operandAt 1 ops)

        ( newValue, m ) =
            adjustIndirect (operandAt 0 ops) delta machine
    in
    executeBranch instr (cmp newValue checkValue) m


executeIncDec : Int -> List Int -> ZMachine -> Outcome
executeIncDec delta ops machine =
    let
        ( _, m ) =
            adjustIndirect (operandAt 0 ops) delta machine
    in
    Continue m



-- OBJECT OPERATIONS


executeJin : Instruction -> List Int -> ZMachine -> Outcome
executeJin instr ops machine =
    let
        child =
            operandAt 0 ops

        parent =
            operandAt 1 ops

        childParent =
            ObjectTable.parent child machine.memory
    in
    executeBranch instr (childParent == parent) machine


executeTestAttr : Instruction -> List Int -> ZMachine -> Outcome
executeTestAttr instr ops machine =
    let
        obj =
            operandAt 0 ops

        attr =
            operandAt 1 ops

        hasAttr =
            ObjectTable.testAttribute obj attr machine.memory
    in
    executeBranch instr hasAttr machine


executeAttrUpdate : (Int -> Int -> Memory.Memory -> Memory.Memory) -> List Int -> ZMachine -> Outcome
executeAttrUpdate update ops machine =
    Continue
        { machine
            | memory = update (operandAt 0 ops) (operandAt 1 ops) machine.memory
        }


executeInsertObj : List Int -> ZMachine -> Outcome
executeInsertObj ops machine =
    let
        obj =
            operandAt 0 ops

        dest =
            operandAt 1 ops

        mem =
            machine.memory
                |> ObjectTable.removeFromParent obj
                |> (\m ->
                        let
                            destChild =
                                ObjectTable.child dest m
                        in
                        m
                            |> ObjectTable.setParent obj dest
                            |> ObjectTable.setSibling obj destChild
                            |> ObjectTable.setChild dest obj
                   )
    in
    Continue
        { machine
            | memory = mem
            , playerTracking = Player.noteInsert obj dest mem machine.playerTracking
        }


executeRemoveObj : List Int -> ZMachine -> Outcome
executeRemoveObj ops machine =
    let
        obj =
            operandAt 0 ops

        mem =
            ObjectTable.removeFromParent obj machine.memory
                |> ObjectTable.setParent obj 0
                |> ObjectTable.setSibling obj 0
    in
    Continue { machine | memory = mem }


{-| Shared implementation of `get_sibling` and `get_child`: store the
resolved object number and branch if it is non-zero.
-}
executeGetTreeLink : (Int -> Memory.Memory -> Int) -> Instruction -> List Int -> ZMachine -> Outcome
executeGetTreeLink accessor instr ops machine =
    let
        target =
            accessor (operandAt 0 ops) machine.memory

        m =
            case instr.store of
                Just varRef ->
                    State.writeVariable varRef target machine

                Nothing ->
                    machine
    in
    executeBranch instr (target /= 0) m


executeGetParent : Instruction -> List Int -> ZMachine -> Outcome
executeGetParent instr ops machine =
    storeResult instr (ObjectTable.parent (operandAt 0 ops) machine.memory) machine


executeGetPropLen : Instruction -> List Int -> ZMachine -> Outcome
executeGetPropLen instr ops machine =
    storeResult instr (ObjectTable.propertyLength (operandAt 0 ops) machine.memory) machine


executeGetProp : Instruction -> List Int -> ZMachine -> Outcome
executeGetProp instr ops machine =
    let
        obj =
            operandAt 0 ops

        propNum =
            operandAt 1 ops
    in
    case ObjectTable.findProperty obj propNum machine.memory of
        Just ( dataAddr, dataLen ) ->
            let
                val =
                    if dataLen == 1 then
                        Memory.readByte dataAddr machine.memory

                    else
                        Memory.readWord dataAddr machine.memory
            in
            storeResult instr val machine

        Nothing ->
            storeResult instr (ObjectTable.propertyDefault propNum machine.memory) machine


executeGetPropAddr : Instruction -> List Int -> ZMachine -> Outcome
executeGetPropAddr instr ops machine =
    let
        obj =
            operandAt 0 ops

        propNum =
            operandAt 1 ops
    in
    case ObjectTable.findProperty obj propNum machine.memory of
        Just ( dataAddr, _ ) ->
            storeResult instr dataAddr machine

        Nothing ->
            storeResult instr 0 machine


executeGetNextProp : Instruction -> List Int -> ZMachine -> Outcome
executeGetNextProp instr ops machine =
    storeResult instr
        (ObjectTable.nextPropertyNumber (operandAt 0 ops) (operandAt 1 ops) machine.memory)
        machine


executePutProp : List Int -> ZMachine -> Outcome
executePutProp ops machine =
    let
        obj =
            operandAt 0 ops

        propNum =
            operandAt 1 ops
    in
    case ObjectTable.findProperty obj propNum machine.memory of
        Just ( dataAddr, dataLen ) ->
            let
                value =
                    operandAt 2 ops

                mem =
                    if dataLen == 1 then
                        Memory.writeByte dataAddr value machine.memory

                    else
                        Memory.writeWord dataAddr value machine.memory
            in
            Continue { machine | memory = mem }

        Nothing ->
            -- Writing to nonexistent property — spec says this is illegal
            Continue machine


executePrintObj : List Int -> ZMachine -> Outcome
executePrintObj ops machine =
    let
        objNum =
            operandAt 0 ops

        name =
            ObjectTable.shortName objNum machine.memory

        noted =
            case machine.currentWindow of
                Types.Upper ->
                    { machine | upperWindow = Window.notePrintObj objNum machine.upperWindow }

                Types.Lower ->
                    machine
    in
    Continue (State.outputObjectName name noted)



-- SHOW STATUS


executeShowStatus : ZMachine -> Outcome
executeShowStatus machine =
    if (Memory.profile machine.memory).hasStatusLine then
        Continue (State.appendOutput (Types.ShowStatusLine (StatusLine.build machine)) machine)

    else
        Continue machine



-- SREAD (input)


executeSread : List Int -> ZMachine -> Outcome
executeSread ops machine =
    let
        textBufAddr =
            operandAt 0 ops

        parseBufAddr =
            operandAt 1 ops

        maxLen =
            Memory.readByte textBufAddr machine.memory
    in
    NeedInput
        { maxLength = maxLen
        , textBufferAddr = textBufAddr
        , parseBufferAddr = parseBufAddr
        }
        (if (Memory.profile machine.memory).hasStatusLine then
            State.appendOutput (Types.ShowStatusLine (StatusLine.build machine)) machine

         else
            machine
        )



-- RANDOM


executeRandom : Instruction -> List Int -> ZMachine -> Outcome
executeRandom instr ops machine =
    let
        range =
            toSignedInt16 (operandAt 0 ops)
    in
    if range <= 0 then
        -- Seed the RNG
        let
            newSeed =
                if range == 0 then
                    -- Re-seed randomly (use a simple counter)
                    machine.randomState.count + 12345

                else
                    -- Seed with absolute value
                    abs range
        in
        storeResult instr 0 { machine | randomState = { seed = newSeed, count = machine.randomState.count + 1 } }

    else
        -- Generate random number 1..range
        let
            rs =
                machine.randomState

            ( value, newSeed ) =
                xorshift rs.seed

            result =
                modBy range value + 1
        in
        storeResult instr result { machine | randomState = { seed = newSeed, count = rs.count + 1 } }



-- GENERAL HELPERS


operandAt : Int -> List Int -> Int
operandAt index ops =
    getAt index ops |> Maybe.withDefault 0


{-| Toggle the transcript (stream 2) flag.
-}
setStream2 : Bool -> ZMachine -> ZMachine
setStream2 enabled machine =
    let
        streams =
            machine.outputStreams
    in
    { machine | outputStreams = { streams | stream2 = enabled } }


{-| Walk both lists once to overwrite the leading `defaults` with `args`.
`List.length defaults` caps the result so routines never receive more
locals than they declared. This is a linear replacement for the earlier
`List.indexedMap`/`getAt` pairing, which was O(n²) in the number of
locals — measurable on routines with many locals called in a hot loop.
-}
mergeArgsWithDefaults : List Int -> List Int -> List Int
mergeArgsWithDefaults args defaults =
    case ( args, defaults ) of
        ( _, [] ) ->
            []

        ( [], _ ) ->
            defaults

        ( arg :: restArgs, _ :: restDefaults ) ->
            arg :: mergeArgsWithDefaults restArgs restDefaults


{-| Read a variable whose number was supplied as an operand to an
"indirect" opcode (inc, dec, inc\_chk, dec\_chk, load, store, pull).
Per Z-machine Standard §6.3.4, when the variable number is 0 the
stack is read _in place_ (peek), not popped.
-}
readIndirect : Int -> ZMachine -> ( Int, ZMachine )
readIndirect varNum machine =
    if varNum == 0 then
        ( State.peekStack machine, machine )

    else
        State.readVariable (variableRefFromByte varNum) machine


{-| Write a variable whose number was supplied as an operand to an
"indirect" opcode. Per Z-machine Standard §6.3.4, when the variable
number is 0 the stack is written _in place_ (poke), not pushed.
-}
writeIndirect : Int -> Int -> ZMachine -> ZMachine
writeIndirect varNum value machine =
    if varNum == 0 then
        State.pokeStack value machine

    else
        State.writeVariable (variableRefFromByte varNum) value machine



-- SAVE / RESTORE (V5 ext versions reuse NeedSave/NeedRestore)


executeSave : Instruction -> ZMachine -> Outcome
executeSave _ machine =
    let
        snap =
            Snapshot.capture
                { memory = machine.memory
                , pc = machine.pc
                , stack = machine.stack
                , callStack = machine.callStack
                , resumeKind = Snapshot.ResumeByStoreResult
                }
    in
    NeedSave snap machine


executeRestore : Instruction -> ZMachine -> Outcome
executeRestore _ machine =
    NeedRestore machine



-- SCAN TABLE


executeScanTable : Instruction -> List Int -> ZMachine -> Outcome
executeScanTable instr ops machine =
    let
        x =
            operandAt 0 ops

        table =
            operandAt 1 ops

        len =
            operandAt 2 ops

        form =
            if List.length ops > 3 then
                operandAt 3 ops

            else
                0x82

        fieldLen =
            Bitwise.and form 0x7F

        isWord =
            Bitwise.and form 0x80 /= 0
    in
    scanTableLoop x table len fieldLen isWord 0 instr machine


scanTableLoop : Int -> Int -> Int -> Int -> Bool -> Int -> Instruction -> ZMachine -> Outcome
scanTableLoop x table len fieldLen isWord i instr machine =
    if i >= len then
        let
            m1 =
                storeResultRaw instr 0 machine
        in
        executeBranch instr False m1

    else
        let
            addr =
                table + i * fieldLen

            val =
                if isWord then
                    Memory.readWord addr machine.memory

                else
                    Memory.readByte addr machine.memory
        in
        if val == x then
            let
                m1 =
                    storeResultRaw instr addr machine
            in
            executeBranch instr True m1

        else
            scanTableLoop x table len fieldLen isWord (i + 1) instr machine


storeResultRaw : Instruction -> Int -> ZMachine -> ZMachine
storeResultRaw instr value machine =
    case instr.store of
        Just varRef ->
            State.writeVariable varRef value machine

        Nothing ->
            machine



-- TOKENISE (VAR opcode)


executeTokenise : List Int -> ZMachine -> Outcome
executeTokenise ops machine =
    let
        textBufAddr =
            operandAt 0 ops

        parseBufAddr =
            operandAt 1 ops

        p =
            Memory.profile machine.memory

        -- Read the text from the text buffer
        textStart =
            textBufAddr + p.textBufferOffset

        input =
            readZsciiString textStart machine.memory
    in
    Continue { machine | memory = Dictionary.tokenize input textBufAddr parseBufAddr machine.memory }


readZsciiString : Int -> Memory.Memory -> String
readZsciiString addr mem =
    readZsciiStringHelp addr mem []


readZsciiStringHelp : Int -> Memory.Memory -> List Char -> String
readZsciiStringHelp addr mem acc =
    let
        byte =
            Memory.readByte addr mem
    in
    if byte == 0 then
        String.fromList (List.reverse acc)

    else
        readZsciiStringHelp (addr + 1) mem (Char.fromCode byte :: acc)



-- COPY TABLE


executeCopyTable : List Int -> ZMachine -> Outcome
executeCopyTable ops machine =
    let
        first =
            operandAt 0 ops

        second =
            operandAt 1 ops

        size =
            toSignedInt16 (operandAt 2 ops)
    in
    if second == 0 then
        -- Zero out first table
        Continue { machine | memory = zeroMemory first (abs size) machine.memory }

    else if size >= 0 then
        -- Copy forward (safe when src > dest)
        Continue { machine | memory = copyMemForward first second size machine.memory }

    else
        -- Copy backward (safe when dest > src, size is negative = abs)
        Continue { machine | memory = copyMemBackward first second (abs size) machine.memory }


zeroMemory : Int -> Int -> Memory.Memory -> Memory.Memory
zeroMemory addr len mem =
    if len <= 0 then
        mem

    else
        zeroMemory (addr + 1) (len - 1) (Memory.writeByte addr 0 mem)


copyMemForward : Int -> Int -> Int -> Memory.Memory -> Memory.Memory
copyMemForward src dest len mem =
    copyMemForwardHelp src dest 0 len mem


copyMemForwardHelp : Int -> Int -> Int -> Int -> Memory.Memory -> Memory.Memory
copyMemForwardHelp src dest i len mem =
    if i >= len then
        mem

    else
        let
            byte =
                Memory.readByte (src + i) mem
        in
        copyMemForwardHelp src dest (i + 1) len (Memory.writeByte (dest + i) byte mem)


copyMemBackward : Int -> Int -> Int -> Memory.Memory -> Memory.Memory
copyMemBackward src dest len mem =
    copyMemBackwardHelp src dest (len - 1) mem


copyMemBackwardHelp : Int -> Int -> Int -> Memory.Memory -> Memory.Memory
copyMemBackwardHelp src dest i mem =
    if i < 0 then
        mem

    else
        let
            byte =
                Memory.readByte (src + i) mem
        in
        copyMemBackwardHelp src dest (i - 1) (Memory.writeByte (dest + i) byte mem)



-- PRINT TABLE


executePrintTable : List Int -> ZMachine -> Outcome
executePrintTable ops machine =
    let
        addr =
            operandAt 0 ops

        width =
            operandAt 1 ops

        height =
            if List.length ops > 2 then
                operandAt 2 ops

            else
                1

        skip =
            if List.length ops > 3 then
                operandAt 3 ops

            else
                0
    in
    printTableRows addr width height skip 0 machine


printTableRows : Int -> Int -> Int -> Int -> Int -> ZMachine -> Outcome
printTableRows addr width height skip row machine =
    if row >= height then
        Continue machine

    else
        let
            rowAddr =
                addr + row * (width + skip)

            text =
                List.range 0 (width - 1)
                    |> List.map (\col -> Char.fromCode (Memory.readByte (rowAddr + col) machine.memory))
                    |> String.fromList

            m1 =
                State.outputText text machine

            m2 =
                if row < height - 1 then
                    State.outputNewLine m1

                else
                    m1
        in
        printTableRows addr width height skip (row + 1) m2



-- CHECK ARG COUNT


executeCheckArgCount : Instruction -> List Int -> ZMachine -> Outcome
executeCheckArgCount instr ops machine =
    let
        argNum =
            operandAt 0 ops

        currentArgCount =
            case machine.callStack of
                frame :: _ ->
                    frame.argCount

                [] ->
                    0
    in
    executeBranch instr (argNum <= currentArgCount) machine



-- LOG SHIFT / ART SHIFT


executeLogShift : Instruction -> List Int -> ZMachine -> Outcome
executeLogShift instr ops machine =
    let
        number =
            operandAt 0 ops

        places =
            toSignedInt16 (operandAt 1 ops)

        result =
            if places >= 0 then
                Bitwise.and (Bitwise.shiftLeftBy places number) 0xFFFF

            else
                Bitwise.shiftRightZfBy (abs places) number
    in
    storeResult instr result machine


executeArtShift : Instruction -> List Int -> ZMachine -> Outcome
executeArtShift instr ops machine =
    let
        number =
            toSignedInt16 (operandAt 0 ops)

        places =
            toSignedInt16 (operandAt 1 ops)

        result =
            if places >= 0 then
                Bitwise.and (Bitwise.shiftLeftBy places number) 0xFFFF

            else
                -- Arithmetic right shift preserves sign
                Bitwise.and (Bitwise.shiftRightBy (abs places) number) 0xFFFF
    in
    storeResult instr result machine
