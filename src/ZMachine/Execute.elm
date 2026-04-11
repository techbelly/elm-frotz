module ZMachine.Execute exposing (step)

{-| Z-Machine instruction execution.

Decodes and executes a single instruction, returning the resulting state.

@docs step

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
import ZMachine.Opcode as Opcode
    exposing
        ( BranchTarget(..)
        , Instruction
        , Op0(..)
        , Op1(..)
        , Op2(..)
        , OpVar(..)
        , Opcode(..)
        , Operand(..)
        , variableRefFromByte
        )
import ZMachine.Memory as Memory
import ZMachine.ObjectTable as ObjectTable
import ZMachine.State as State
import ZMachine.StatusLine as StatusLine
import ZMachine.Text as Text
import ZMachine.Types as Types
    exposing
        ( InputRequest(..)
        , StepResult(..)
        , ZMachine
        , ZMachineError(..)
        )


{-| Execute a single instruction at the current PC.
-}
step : ZMachine -> StepResult
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


execute : Instruction -> Int -> List Int -> ZMachine -> StepResult
execute instr nextPC ops machine =
    let
        m =
            { machine | pc = nextPC }
    in
    case instr.opcode of
        -- 2OP
        Op2 Je ->
            executeJe instr ops m

        Op2 Jl ->
            executeBranch instr (toSignedInt16 (operandAt 0 ops) < toSignedInt16 (operandAt 1 ops)) m

        Op2 Jg ->
            executeBranch instr (toSignedInt16 (operandAt 0 ops) > toSignedInt16 (operandAt 1 ops)) m

        Op2 DecChk ->
            executeCheckedStep -1 (<) instr ops m

        Op2 IncChk ->
            executeCheckedStep 1 (>) instr ops m

        Op2 Jin ->
            executeJin instr ops m

        Op2 Test ->
            let
                bitmap =
                    operandAt 0 ops

                flags =
                    operandAt 1 ops
            in
            executeBranch instr (Bitwise.and bitmap flags == flags) m

        Op2 Or ->
            storeResult instr (Bitwise.or (operandAt 0 ops) (operandAt 1 ops)) m

        Op2 And ->
            storeResult instr (Bitwise.and (operandAt 0 ops) (operandAt 1 ops)) m

        Op2 TestAttr ->
            executeTestAttr instr ops m

        Op2 SetAttr ->
            executeAttrUpdate ObjectTable.setAttribute ops m

        Op2 ClearAttr ->
            executeAttrUpdate ObjectTable.clearAttribute ops m

        Op2 Store ->
            Continue (writeIndirect (operandAt 0 ops) (operandAt 1 ops) m)

        Op2 InsertObj ->
            executeInsertObj ops m

        Op2 Loadw ->
            let
                addr =
                    operandAt 0 ops + Memory.wordLength * operandAt 1 ops
            in
            storeResult instr (Memory.readWord addr m.memory) m

        Op2 Loadb ->
            let
                addr =
                    operandAt 0 ops + operandAt 1 ops
            in
            storeResult instr (Memory.readByte addr m.memory) m

        Op2 GetProp ->
            executeGetProp instr ops m

        Op2 GetPropAddr ->
            executeGetPropAddr instr ops m

        Op2 GetNextProp ->
            executeGetNextProp instr ops m

        Op2 Add ->
            storeResult instr (addInt16 (operandAt 0 ops) (operandAt 1 ops)) m

        Op2 Sub ->
            storeResult instr (subInt16 (operandAt 0 ops) (operandAt 1 ops)) m

        Op2 Mul ->
            storeResult instr (mulInt16 (operandAt 0 ops) (operandAt 1 ops)) m

        Op2 Div ->
            if operandAt 1 ops == 0 then
                Error DivisionByZero m

            else
                storeResult instr (divInt16 (operandAt 0 ops) (operandAt 1 ops)) m

        Op2 Mod ->
            if operandAt 1 ops == 0 then
                Error DivisionByZero m

            else
                storeResult instr (modInt16 (operandAt 0 ops) (operandAt 1 ops)) m

        Op2 (Opcode.Unknown2Op n) ->
            Error (InvalidOpcode n) m

        -- 1OP
        Op1 Jz ->
            executeBranch instr (operandAt 0 ops == 0) m

        Op1 GetSibling ->
            executeGetTreeLink ObjectTable.sibling instr ops m

        Op1 GetChild ->
            executeGetTreeLink ObjectTable.child instr ops m

        Op1 GetParent ->
            executeGetParent instr ops m

        Op1 GetPropLen ->
            executeGetPropLen instr ops m

        Op1 Inc ->
            executeStep 1 ops m

        Op1 Dec ->
            executeStep -1 ops m

        Op1 PrintAddr ->
            let
                ( str, _ ) =
                    Text.decodeZString (operandAt 0 ops) m.memory
            in
            Continue (State.appendOutput (Types.PrintText str) m)

        Op1 CallS1 ->
            executeCall instr [ operandAt 0 ops ] [] m

        Op1 RemoveObj ->
            executeRemoveObj ops m

        Op1 PrintObj ->
            executePrintObj ops m

        Op1 Ret ->
            executeReturn (operandAt 0 ops) m

        Op1 Jump ->
            let
                offset =
                    toSignedInt16 (operandAt 0 ops)
            in
            Continue { m | pc = m.pc + offset - 2 }

        Op1 PrintPaddr ->
            let
                addr =
                    Memory.unpackAddress (operandAt 0 ops)

                ( str, _ ) =
                    Text.decodeZString addr m.memory
            in
            Continue (State.appendOutput (Types.PrintText str) m)

        Op1 Load ->
            let
                ( val, m2 ) =
                    readIndirect (operandAt 0 ops) m
            in
            storeResult instr val m2

        Op1 Not ->
            storeResult instr (Bitwise.and (Bitwise.complement (operandAt 0 ops)) 0xFFFF) m

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
                    Continue (State.appendOutput (Types.PrintText str) m)

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
                                |> State.appendOutput (Types.PrintText str)
                                |> State.appendOutput Types.NewLine
                    in
                    executeReturn 1 m2

                Nothing ->
                    executeReturn 1 m

        Op0 Nop ->
            Continue m

        Op0 Save ->
            -- For now, save always fails (branches on failure)
            executeBranch instr False m

        Op0 Restore ->
            -- For now, restore always fails
            executeBranch instr False m

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
            let
                ( _, m2 ) =
                    State.popStack m
            in
            Continue m2

        Op0 Quit ->
            Halted m

        Op0 NewLine ->
            Continue (State.appendOutput Types.NewLine m)

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
            let
                routineAddr =
                    operandAt 0 ops

                args =
                    List.drop 1 ops
            in
            executeCall instr [ routineAddr ] args m

        OpVar Storew ->
            let
                addr =
                    operandAt 0 ops + Memory.wordLength * operandAt 1 ops

                val =
                    operandAt 2 ops
            in
            Continue { m | memory = Memory.writeWord addr val m.memory }

        OpVar Storeb ->
            let
                addr =
                    operandAt 0 ops + operandAt 1 ops

                val =
                    operandAt 2 ops
            in
            Continue { m | memory = Memory.writeByte addr val m.memory }

        OpVar PutProp ->
            executePutProp ops m

        OpVar Sread ->
            executeSread ops m

        OpVar PrintChar ->
            let
                ch =
                    Text.zsciiToChar (operandAt 0 ops)
            in
            Continue (State.appendOutput (Types.PrintText (String.fromChar ch)) m)

        OpVar PrintNum ->
            let
                num =
                    toSignedInt16 (operandAt 0 ops)
            in
            Continue (State.appendOutput (Types.PrintText (String.fromInt num)) m)

        OpVar Random ->
            executeRandom instr ops m

        OpVar Push ->
            Continue (State.pushStack (operandAt 0 ops) m)

        OpVar Pull ->
            let
                varNum =
                    operandAt 0 ops

                ( val, m2 ) =
                    State.popStack m
            in
            Continue (writeIndirect varNum val m2)

        OpVar SplitWindow ->
            Continue (State.appendOutput (Types.SplitWindow (operandAt 0 ops)) m)

        OpVar SetWindow ->
            let
                win =
                    if operandAt 0 ops == 0 then
                        Types.Lower

                    else
                        Types.Upper
            in
            Continue (State.appendOutput (Types.SetWindow win) m)

        OpVar OutputStream ->
            -- Minimal: just track stream 2 (transcript) toggle
            let
                streamNum =
                    toSignedInt16 (operandAt 0 ops)
            in
            if streamNum == 2 then
                let
                    streams =
                        m.outputStreams
                in
                Continue { m | outputStreams = { streams | stream2 = True } }

            else if streamNum == -2 then
                let
                    streams =
                        m.outputStreams
                in
                Continue { m | outputStreams = { streams | stream2 = False } }

            else
                Continue m

        OpVar InputStream ->
            -- Only stream 0 (keyboard) supported; ignore
            Continue m

        OpVar SoundEffect ->
            Continue (State.appendOutput (Types.PlaySound (operandAt 0 ops)) m)

        OpVar (Opcode.UnknownVar n) ->
            Error (InvalidOpcode n) m



-- BRANCH EXECUTION


executeBranch : Instruction -> Bool -> ZMachine -> StepResult
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


storeResult : Instruction -> Int -> ZMachine -> StepResult
storeResult instr value machine =
    case instr.store of
        Just varRef ->
            Continue (State.writeVariable varRef value machine)

        Nothing ->
            Continue machine



-- CALL / RETURN


executeCall : Instruction -> List Int -> List Int -> ZMachine -> StepResult
executeCall instr addrOps args machine =
    let
        packedAddr =
            List.head addrOps |> Maybe.withDefault 0
    in
    if packedAddr == 0 then
        -- Calling address 0 returns false
        storeResult instr 0 machine

    else
        let
            routineAddr =
                Memory.unpackAddress packedAddr

            numLocals =
                Memory.readByte routineAddr machine.memory

            -- V3: initial local values follow the count byte
            initialValues =
                List.range 1 numLocals
                    |> List.map
                        (\i ->
                            Memory.readWord (routineAddr + 1 + (i - 1) * Memory.wordLength) machine.memory
                        )

            -- Override leading defaults with provided arguments
            localsWithArgs =
                mergeArgsWithDefaults args initialValues

            firstInstrAddr =
                routineAddr + 1 + numLocals * Memory.wordLength

            frame =
                { returnPC = machine.pc
                , returnStore = instr.store
                , locals = Array.fromList localsWithArgs
                , evalStack = machine.stack
                }
        in
        Continue
            { machine
                | pc = firstInstrAddr
                , callStack = frame :: machine.callStack
                , stack = []
            }


executeReturn : Int -> ZMachine -> StepResult
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


executeJe : Instruction -> List Int -> ZMachine -> StepResult
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


executeCheckedStep : Int -> (Int -> Int -> Bool) -> Instruction -> List Int -> ZMachine -> StepResult
executeCheckedStep delta cmp instr ops machine =
    let
        checkValue =
            toSignedInt16 (operandAt 1 ops)

        ( newValue, m ) =
            adjustIndirect (operandAt 0 ops) delta machine
    in
    executeBranch instr (cmp newValue checkValue) m


executeStep : Int -> List Int -> ZMachine -> StepResult
executeStep delta ops machine =
    let
        ( _, m ) =
            adjustIndirect (operandAt 0 ops) delta machine
    in
    Continue m



-- OBJECT OPERATIONS


executeJin : Instruction -> List Int -> ZMachine -> StepResult
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


executeTestAttr : Instruction -> List Int -> ZMachine -> StepResult
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


executeAttrUpdate : (Int -> Int -> Memory.Memory -> Memory.Memory) -> List Int -> ZMachine -> StepResult
executeAttrUpdate update ops machine =
    Continue
        { machine
            | memory = update (operandAt 0 ops) (operandAt 1 ops) machine.memory
        }


executeInsertObj : List Int -> ZMachine -> StepResult
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
    Continue { machine | memory = mem }


executeRemoveObj : List Int -> ZMachine -> StepResult
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
executeGetTreeLink : (Int -> Memory.Memory -> Int) -> Instruction -> List Int -> ZMachine -> StepResult
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


executeGetParent : Instruction -> List Int -> ZMachine -> StepResult
executeGetParent instr ops machine =
    storeResult instr (ObjectTable.parent (operandAt 0 ops) machine.memory) machine


executeGetPropLen : Instruction -> List Int -> ZMachine -> StepResult
executeGetPropLen instr ops machine =
    storeResult instr (ObjectTable.propertyLength (operandAt 0 ops) machine.memory) machine


executeGetProp : Instruction -> List Int -> ZMachine -> StepResult
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


executeGetPropAddr : Instruction -> List Int -> ZMachine -> StepResult
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


executeGetNextProp : Instruction -> List Int -> ZMachine -> StepResult
executeGetNextProp instr ops machine =
    storeResult instr
        (ObjectTable.nextPropertyNumber (operandAt 0 ops) (operandAt 1 ops) machine.memory)
        machine


executePutProp : List Int -> ZMachine -> StepResult
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


executePrintObj : List Int -> ZMachine -> StepResult
executePrintObj ops machine =
    let
        name =
            ObjectTable.shortName (operandAt 0 ops) machine.memory
    in
    Continue (State.appendOutput (Types.PrintText name) machine)



-- SHOW STATUS


executeShowStatus : ZMachine -> StepResult
executeShowStatus machine =
    Continue (State.appendOutput (Types.ShowStatusLine (StatusLine.build machine)) machine)



-- SREAD (input)


executeSread : List Int -> ZMachine -> StepResult
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
        (LineInput
            { maxLength = maxLen
            , textBufferAddr = textBufAddr
            , parseBufferAddr = parseBufAddr
            }
        )
        (State.appendOutput (Types.ShowStatusLine (StatusLine.build machine)) machine)



-- RANDOM


executeRandom : Instruction -> List Int -> ZMachine -> StepResult
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
