module ZMachine.Execute exposing (step)

{-| Z-Machine instruction execution.

Decodes and executes a single instruction, returning the resulting state.

@docs step

-}

import Bitwise
import ZMachine.Instruction as Inst
    exposing
        ( BranchTarget(..)
        , Instruction
        , Op0(..)
        , Op1(..)
        , Op2(..)
        , OpVar(..)
        , Opcode(..)
        , Operand(..)
        , VariableRef(..)
        )
import ZMachine.Memory as Memory exposing (Memory)
import ZMachine.Memory.Header as Header
import ZMachine.Stack as Stack
import ZMachine.State as State
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
            Inst.decode machine.pc machine.memory

        nextPC =
            machine.pc + instr.length

        ( operandValues, machineAfterOperands ) =
            resolveOperands instr.operands machine
    in
    execute instr nextPC operandValues machineAfterOperands



-- OPERAND RESOLUTION


resolveOperands : List Operand -> ZMachine -> ( List Int, ZMachine )
resolveOperands operands machine =
    List.foldl
        (\op ( acc, m ) ->
            let
                ( val, m2 ) =
                    resolveOperand op m
            in
            ( acc ++ [ val ], m2 )
        )
        ( [], machine )
        operands


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
            executeBranch instr (toSigned (getOp 0 ops) < toSigned (getOp 1 ops)) m

        Op2 Jg ->
            executeBranch instr (toSigned (getOp 0 ops) > toSigned (getOp 1 ops)) m

        Op2 DecChk ->
            executeDecChk instr ops m

        Op2 IncChk ->
            executeIncChk instr ops m

        Op2 Jin ->
            executeJin instr ops m

        Op2 Test ->
            let
                bitmap =
                    getOp 0 ops

                flags =
                    getOp 1 ops
            in
            executeBranch instr (Bitwise.and bitmap flags == flags) m

        Op2 Or ->
            storeResult instr (Bitwise.or (getOp 0 ops) (getOp 1 ops)) m

        Op2 And ->
            storeResult instr (Bitwise.and (getOp 0 ops) (getOp 1 ops)) m

        Op2 TestAttr ->
            executeTestAttr instr ops m

        Op2 SetAttr ->
            executeSetAttr ops m

        Op2 ClearAttr ->
            executeClearAttr ops m

        Op2 Store ->
            Continue (writeIndirect (getOp 0 ops) (getOp 1 ops) m)

        Op2 InsertObj ->
            executeInsertObj ops m

        Op2 Loadw ->
            let
                addr =
                    getOp 0 ops + 2 * getOp 1 ops
            in
            storeResult instr (Memory.readWord addr m.memory) m

        Op2 Loadb ->
            let
                addr =
                    getOp 0 ops + getOp 1 ops
            in
            storeResult instr (Memory.readByte addr m.memory) m

        Op2 GetProp ->
            executeGetProp instr ops m

        Op2 GetPropAddr ->
            executeGetPropAddr instr ops m

        Op2 GetNextProp ->
            executeGetNextProp instr ops m

        Op2 Add ->
            storeResult instr (toUnsigned (toSigned (getOp 0 ops) + toSigned (getOp 1 ops))) m

        Op2 Sub ->
            storeResult instr (toUnsigned (toSigned (getOp 0 ops) - toSigned (getOp 1 ops))) m

        Op2 Mul ->
            storeResult instr (toUnsigned (toSigned (getOp 0 ops) * toSigned (getOp 1 ops))) m

        Op2 Div ->
            if getOp 1 ops == 0 then
                Error DivisionByZero m

            else
                storeResult instr (toUnsigned (truncDiv (toSigned (getOp 0 ops)) (toSigned (getOp 1 ops)))) m

        Op2 Mod ->
            if getOp 1 ops == 0 then
                Error DivisionByZero m

            else
                storeResult instr (toUnsigned (truncMod (toSigned (getOp 0 ops)) (toSigned (getOp 1 ops)))) m

        Op2 (Inst.Unknown2Op n) ->
            Error (InvalidOpcode n) m

        -- 1OP
        Op1 Jz ->
            executeBranch instr (getOp 0 ops == 0) m

        Op1 GetSibling ->
            executeGetSibling instr ops m

        Op1 GetChild ->
            executeGetChild instr ops m

        Op1 GetParent ->
            executeGetParent instr ops m

        Op1 GetPropLen ->
            executeGetPropLen instr ops m

        Op1 Inc ->
            let
                byte =
                    getOp 0 ops

                ( current, m2 ) =
                    readIndirect byte m
            in
            Continue (writeIndirect byte (toUnsigned (toSigned current + 1)) m2)

        Op1 Dec ->
            let
                byte =
                    getOp 0 ops

                ( current, m2 ) =
                    readIndirect byte m
            in
            Continue (writeIndirect byte (toUnsigned (toSigned current - 1)) m2)

        Op1 PrintAddr ->
            let
                ( str, _ ) =
                    Text.decodeZString (getOp 0 ops) m.memory
            in
            Continue (State.appendOutput (Types.PrintText str) m)

        Op1 CallS1 ->
            executeCall instr [ getOp 0 ops ] [] m

        Op1 RemoveObj ->
            executeRemoveObj ops m

        Op1 PrintObj ->
            executePrintObj ops m

        Op1 Ret ->
            executeReturn (getOp 0 ops) m

        Op1 Jump ->
            let
                offset =
                    toSigned (getOp 0 ops)
            in
            Continue { m | pc = m.pc + offset - 2 }

        Op1 PrintPaddr ->
            let
                addr =
                    Memory.unpackAddress (getOp 0 ops)

                ( str, _ ) =
                    Text.decodeZString addr m.memory
            in
            Continue (State.appendOutput (Types.PrintText str) m)

        Op1 Load ->
            let
                ( val, m2 ) =
                    readIndirect (getOp 0 ops) m
            in
            storeResult instr val m2

        Op1 Not ->
            storeResult instr (Bitwise.and (Bitwise.complement (getOp 0 ops)) 0xFFFF) m

        Op1 (Inst.Unknown1Op n) ->
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

        Op0 (Inst.Unknown0Op n) ->
            Error (InvalidOpcode n) m

        -- VAR
        OpVar Call ->
            let
                routineAddr =
                    getOp 0 ops

                args =
                    List.drop 1 ops
            in
            executeCall instr [ routineAddr ] args m

        OpVar Storew ->
            let
                addr =
                    getOp 0 ops + 2 * getOp 1 ops

                val =
                    getOp 2 ops
            in
            Continue { m | memory = Memory.writeWord addr val m.memory }

        OpVar Storeb ->
            let
                addr =
                    getOp 0 ops + getOp 1 ops

                val =
                    getOp 2 ops
            in
            Continue { m | memory = Memory.writeByte addr val m.memory }

        OpVar PutProp ->
            executePutProp ops m

        OpVar Sread ->
            executeSread ops m

        OpVar PrintChar ->
            let
                ch =
                    Text.zsciiToChar (getOp 0 ops)
            in
            Continue (State.appendOutput (Types.PrintText (String.fromChar ch)) m)

        OpVar PrintNum ->
            let
                num =
                    toSigned (getOp 0 ops)
            in
            Continue (State.appendOutput (Types.PrintText (String.fromInt num)) m)

        OpVar Random ->
            executeRandom instr ops m

        OpVar Push ->
            Continue (State.pushStack (getOp 0 ops) m)

        OpVar Pull ->
            let
                byte =
                    getOp 0 ops

                ( val, m2 ) =
                    State.popStack m
            in
            Continue (writeIndirect byte val m2)

        OpVar SplitWindow ->
            Continue (State.appendOutput (Types.SplitWindow (getOp 0 ops)) m)

        OpVar SetWindow ->
            let
                win =
                    if getOp 0 ops == 0 then
                        Types.Lower

                    else
                        Types.Upper
            in
            Continue (State.appendOutput (Types.SetWindow win) m)

        OpVar OutputStream ->
            -- Minimal: just track stream 2 (transcript) toggle
            let
                streamNum =
                    toSigned (getOp 0 ops)
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
            Continue (State.appendOutput (Types.PlaySound (getOp 0 ops)) m)

        OpVar (Inst.UnknownVar n) ->
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
            Continue (State.writeVariable varRef (toUnsigned value) machine)

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
                            Memory.readWord (routineAddr + 1 + (i - 1) * 2) machine.memory
                        )

            -- Override with provided arguments
            localsWithArgs =
                List.indexedMap
                    (\i default ->
                        case listGet i args of
                            Just arg ->
                                arg

                            Nothing ->
                                default
                    )
                    initialValues

            firstInstrAddr =
                routineAddr + 1 + numLocals * 2

            frame =
                Stack.newFrame numLocals localsWithArgs machine.pc instr.store machine.stack
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
                    Continue (State.writeVariable varRef (toUnsigned value) m)

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
            getOp 0 ops

        rest =
            List.drop 1 ops

        matches =
            List.member a rest
    in
    executeBranch instr matches machine



-- DEC_CHK / INC_CHK


executeDecChk : Instruction -> List Int -> ZMachine -> StepResult
executeDecChk instr ops machine =
    let
        byte =
            getOp 0 ops

        checkValue =
            toSigned (getOp 1 ops)

        ( current, m ) =
            readIndirect byte machine

        newValue =
            toSigned current - 1

        m2 =
            writeIndirect byte (toUnsigned newValue) m
    in
    executeBranch instr (newValue < checkValue) m2


executeIncChk : Instruction -> List Int -> ZMachine -> StepResult
executeIncChk instr ops machine =
    let
        byte =
            getOp 0 ops

        checkValue =
            toSigned (getOp 1 ops)

        ( current, m ) =
            readIndirect byte machine

        newValue =
            toSigned current + 1

        m2 =
            writeIndirect byte (toUnsigned newValue) m
    in
    executeBranch instr (newValue > checkValue) m2



-- OBJECT OPERATIONS (stubs for now — full implementation in Phase 3)


executeJin : Instruction -> List Int -> ZMachine -> StepResult
executeJin instr ops machine =
    let
        child =
            getOp 0 ops

        parent =
            getOp 1 ops

        childParent =
            readObjectParent child machine.memory
    in
    executeBranch instr (childParent == parent) machine


executeTestAttr : Instruction -> List Int -> ZMachine -> StepResult
executeTestAttr instr ops machine =
    let
        obj =
            getOp 0 ops

        attr =
            getOp 1 ops

        hasAttr =
            testObjectAttribute obj attr machine.memory
    in
    executeBranch instr hasAttr machine


executeSetAttr : List Int -> ZMachine -> StepResult
executeSetAttr ops machine =
    let
        obj =
            getOp 0 ops

        attr =
            getOp 1 ops

        addr =
            objectAddress obj machine.memory + (attr // 8)

        byte =
            Memory.readByte addr machine.memory

        newByte =
            Bitwise.or byte (Bitwise.shiftLeftBy (7 - modBy 8 attr) 1)
    in
    Continue { machine | memory = Memory.writeByte addr newByte machine.memory }


executeClearAttr : List Int -> ZMachine -> StepResult
executeClearAttr ops machine =
    let
        obj =
            getOp 0 ops

        attr =
            getOp 1 ops

        addr =
            objectAddress obj machine.memory + (attr // 8)

        byte =
            Memory.readByte addr machine.memory

        newByte =
            Bitwise.and byte (Bitwise.complement (Bitwise.shiftLeftBy (7 - modBy 8 attr) 1))
    in
    Continue { machine | memory = Memory.writeByte addr newByte machine.memory }


executeInsertObj : List Int -> ZMachine -> StepResult
executeInsertObj ops machine =
    let
        obj =
            getOp 0 ops

        dest =
            getOp 1 ops

        mem =
            machine.memory
                |> removeObjectFromParent obj
                |> (\m ->
                        let
                            destChild =
                                readObjectChild dest m
                        in
                        m
                            |> writeObjectParent obj dest
                            |> writeObjectSibling obj destChild
                            |> writeObjectChild dest obj
                   )
    in
    Continue { machine | memory = mem }


executeRemoveObj : List Int -> ZMachine -> StepResult
executeRemoveObj ops machine =
    let
        obj =
            getOp 0 ops

        mem =
            removeObjectFromParent obj machine.memory
                |> writeObjectParent obj 0
                |> writeObjectSibling obj 0
    in
    Continue { machine | memory = mem }


executeGetSibling : Instruction -> List Int -> ZMachine -> StepResult
executeGetSibling instr ops machine =
    let
        sibling =
            readObjectSibling (getOp 0 ops) machine.memory

        m =
            case instr.store of
                Just varRef ->
                    State.writeVariable varRef sibling machine

                Nothing ->
                    machine
    in
    executeBranch instr (sibling /= 0) m


executeGetChild : Instruction -> List Int -> ZMachine -> StepResult
executeGetChild instr ops machine =
    let
        child =
            readObjectChild (getOp 0 ops) machine.memory

        m =
            case instr.store of
                Just varRef ->
                    State.writeVariable varRef child machine

                Nothing ->
                    machine
    in
    executeBranch instr (child /= 0) m


executeGetParent : Instruction -> List Int -> ZMachine -> StepResult
executeGetParent instr ops machine =
    let
        parent =
            readObjectParent (getOp 0 ops) machine.memory
    in
    storeResult instr parent machine


executeGetPropLen : Instruction -> List Int -> ZMachine -> StepResult
executeGetPropLen instr ops machine =
    let
        propDataAddr =
            getOp 0 ops

        len =
            if propDataAddr == 0 then
                0

            else
                -- The size byte is the byte *before* the property data
                let
                    sizeByte =
                        Memory.readByte (propDataAddr - 1) machine.memory
                in
                Bitwise.shiftRightZfBy 5 sizeByte + 1
    in
    storeResult instr len machine


executeGetProp : Instruction -> List Int -> ZMachine -> StepResult
executeGetProp instr ops machine =
    let
        obj =
            getOp 0 ops

        propNum =
            getOp 1 ops

        result =
            findProperty obj propNum machine.memory
    in
    case result of
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
            -- Return default value
            let
                tableBase =
                    Header.objectTableAddress machine.memory

                defaultVal =
                    Memory.readWord (tableBase + (propNum - 1) * 2) machine.memory
            in
            storeResult instr defaultVal machine


executeGetPropAddr : Instruction -> List Int -> ZMachine -> StepResult
executeGetPropAddr instr ops machine =
    let
        obj =
            getOp 0 ops

        propNum =
            getOp 1 ops

        result =
            findProperty obj propNum machine.memory
    in
    case result of
        Just ( dataAddr, _ ) ->
            storeResult instr dataAddr machine

        Nothing ->
            storeResult instr 0 machine


executeGetNextProp : Instruction -> List Int -> ZMachine -> StepResult
executeGetNextProp instr ops machine =
    let
        obj =
            getOp 0 ops

        propNum =
            getOp 1 ops

        propTableAddr =
            readObjectPropTableAddr obj machine.memory

        -- Skip the short name header
        nameLen =
            Memory.readByte propTableAddr machine.memory

        firstPropAddr =
            propTableAddr + 1 + nameLen * 2
    in
    if propNum == 0 then
        -- Return number of first property
        let
            sizeByte =
                Memory.readByte firstPropAddr machine.memory
        in
        storeResult instr (Bitwise.and sizeByte 0x1F) machine

    else
        -- Find propNum, then return the number of the one after it
        let
            nextNum =
                findNextPropertyNumber firstPropAddr propNum machine.memory
        in
        storeResult instr nextNum machine


executePutProp : List Int -> ZMachine -> StepResult
executePutProp ops machine =
    let
        obj =
            getOp 0 ops

        propNum =
            getOp 1 ops

        result =
            findProperty obj propNum machine.memory
    in
    case result of
        Just ( dataAddr, dataLen ) ->
            let
                value =
                    getOp 2 ops

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
        obj =
            getOp 0 ops

        propTableAddr =
            readObjectPropTableAddr obj machine.memory

        nameLen =
            Memory.readByte propTableAddr machine.memory

        name =
            if nameLen == 0 then
                ""

            else
                let
                    ( str, _ ) =
                        Text.decodeZString (propTableAddr + 1) machine.memory
                in
                str
    in
    Continue (State.appendOutput (Types.PrintText name) machine)



-- SHOW STATUS


executeShowStatus : ZMachine -> StepResult
executeShowStatus machine =
    let
        globalsAddr =
            Header.globalVariablesAddress machine.memory

        -- Global 0 (var 0x10) = location object number
        locationObj =
            Memory.readWord globalsAddr machine.memory

        -- Global 1 (var 0x11) = score or hours
        scoreOrHours =
            toSigned (Memory.readWord (globalsAddr + 2) machine.memory)

        -- Global 2 (var 0x12) = turns or minutes
        turnsOrMinutes =
            Memory.readWord (globalsAddr + 4) machine.memory

        isTimeGame =
            Header.testFlag1 Header.StatusLineType machine.memory

        locationName =
            if locationObj == 0 then
                ""

            else
                let
                    propTableAddr =
                        readObjectPropTableAddr locationObj machine.memory

                    nameLen =
                        Memory.readByte propTableAddr machine.memory
                in
                if nameLen == 0 then
                    ""

                else
                    let
                        ( str, _ ) =
                            Text.decodeZString (propTableAddr + 1) machine.memory
                    in
                    str

        status =
            { locationName = locationName
            , score = scoreOrHours
            , turns = turnsOrMinutes
            , isTimeGame = isTimeGame
            }
    in
    Continue (State.appendOutput (Types.ShowStatusLine status) machine)



-- SREAD (input)


executeSread : List Int -> ZMachine -> StepResult
executeSread ops machine =
    let
        textBufAddr =
            getOp 0 ops

        parseBufAddr =
            getOp 1 ops

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
        (State.appendOutput (Types.ShowStatusLine (buildStatusLine machine)) machine)


buildStatusLine : ZMachine -> Types.StatusLine
buildStatusLine machine =
    let
        globalsAddr =
            Header.globalVariablesAddress machine.memory

        locationObj =
            Memory.readWord globalsAddr machine.memory

        locationName =
            if locationObj == 0 then
                ""

            else
                let
                    propTableAddr =
                        readObjectPropTableAddr locationObj machine.memory

                    nameLen =
                        Memory.readByte propTableAddr machine.memory
                in
                if nameLen == 0 then
                    ""

                else
                    Tuple.first (Text.decodeZString (propTableAddr + 1) machine.memory)
    in
    { locationName = locationName
    , score = toSigned (Memory.readWord (globalsAddr + 2) machine.memory)
    , turns = Memory.readWord (globalsAddr + 4) machine.memory
    , isTimeGame = Header.testFlag1 Header.StatusLineType machine.memory
    }



-- RANDOM


executeRandom : Instruction -> List Int -> ZMachine -> StepResult
executeRandom instr ops machine =
    let
        range =
            toSigned (getOp 0 ops)
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

            -- Simple xorshift
            s1 =
                Bitwise.xor rs.seed (Bitwise.shiftLeftBy 13 rs.seed)

            s2 =
                Bitwise.xor s1 (Bitwise.shiftRightZfBy 17 s1)

            s3 =
                Bitwise.xor s2 (Bitwise.shiftLeftBy 5 s2)

            newSeed =
                Bitwise.and s3 0x7FFFFFFF

            result =
                modBy range (abs newSeed) + 1
        in
        storeResult instr result { machine | randomState = { seed = newSeed, count = rs.count + 1 } }



-- OBJECT TABLE HELPERS


objectAddress : Int -> Memory -> Int
objectAddress objNum mem =
    let
        tableBase =
            Header.objectTableAddress mem

        defaultsSize =
            31 * 2
    in
    tableBase + defaultsSize + (objNum - 1) * 9


readObjectParent : Int -> Memory -> Int
readObjectParent objNum mem =
    if objNum == 0 then
        0

    else
        Memory.readByte (objectAddress objNum mem + 4) mem


readObjectSibling : Int -> Memory -> Int
readObjectSibling objNum mem =
    if objNum == 0 then
        0

    else
        Memory.readByte (objectAddress objNum mem + 5) mem


readObjectChild : Int -> Memory -> Int
readObjectChild objNum mem =
    if objNum == 0 then
        0

    else
        Memory.readByte (objectAddress objNum mem + 6) mem


writeObjectParent : Int -> Int -> Memory -> Memory
writeObjectParent objNum value mem =
    if objNum == 0 then
        mem

    else
        Memory.writeByte (objectAddress objNum mem + 4) value mem


writeObjectSibling : Int -> Int -> Memory -> Memory
writeObjectSibling objNum value mem =
    if objNum == 0 then
        mem

    else
        Memory.writeByte (objectAddress objNum mem + 5) value mem


writeObjectChild : Int -> Int -> Memory -> Memory
writeObjectChild objNum value mem =
    if objNum == 0 then
        mem

    else
        Memory.writeByte (objectAddress objNum mem + 6) value mem


readObjectPropTableAddr : Int -> Memory -> Int
readObjectPropTableAddr objNum mem =
    if objNum == 0 then
        0

    else
        Memory.readWord (objectAddress objNum mem + 7) mem


testObjectAttribute : Int -> Int -> Memory -> Bool
testObjectAttribute objNum attr mem =
    if objNum == 0 then
        False

    else
        let
            addr =
                objectAddress objNum mem + (attr // 8)

            byte =
                Memory.readByte addr mem

            bitIndex =
                7 - modBy 8 attr
        in
        Bitwise.and byte (Bitwise.shiftLeftBy bitIndex 1) /= 0


removeObjectFromParent : Int -> Memory -> Memory
removeObjectFromParent objNum mem =
    let
        parent =
            readObjectParent objNum mem
    in
    if parent == 0 then
        mem

    else
        let
            parentChild =
                readObjectChild parent mem
        in
        if parentChild == objNum then
            -- Object is the first child — parent's child becomes object's sibling
            let
                sibling =
                    readObjectSibling objNum mem
            in
            writeObjectChild parent sibling mem

        else
            -- Walk the sibling chain to find the one before this object
            removeSiblingFromChain parentChild objNum mem


removeSiblingFromChain : Int -> Int -> Memory -> Memory
removeSiblingFromChain current target mem =
    if current == 0 then
        mem

    else
        let
            sibling =
                readObjectSibling current mem
        in
        if sibling == target then
            writeObjectSibling current (readObjectSibling target mem) mem

        else
            removeSiblingFromChain sibling target mem



-- PROPERTY HELPERS


{-| Find a property on an object. Returns (dataAddress, dataLength) or Nothing.
-}
findProperty : Int -> Int -> Memory -> Maybe ( Int, Int )
findProperty objNum propNum mem =
    let
        propTableAddr =
            readObjectPropTableAddr objNum mem

        nameLen =
            Memory.readByte propTableAddr mem

        firstPropAddr =
            propTableAddr + 1 + nameLen * 2
    in
    findPropertyAt firstPropAddr propNum mem


findPropertyAt : Int -> Int -> Memory -> Maybe ( Int, Int )
findPropertyAt addr propNum mem =
    let
        sizeByte =
            Memory.readByte addr mem
    in
    if sizeByte == 0 then
        Nothing

    else
        let
            num =
                Bitwise.and sizeByte 0x1F

            dataLen =
                Bitwise.shiftRightZfBy 5 sizeByte + 1

            dataAddr =
                addr + 1
        in
        if num == propNum then
            Just ( dataAddr, dataLen )

        else if num < propNum then
            -- Properties are in descending order; we've passed it
            Nothing

        else
            findPropertyAt (dataAddr + dataLen) propNum mem


findNextPropertyNumber : Int -> Int -> Memory -> Int
findNextPropertyNumber addr targetPropNum mem =
    let
        sizeByte =
            Memory.readByte addr mem
    in
    if sizeByte == 0 then
        0

    else
        let
            num =
                Bitwise.and sizeByte 0x1F

            dataLen =
                Bitwise.shiftRightZfBy 5 sizeByte + 1

            nextAddr =
                addr + 1 + dataLen
        in
        if num == targetPropNum then
            -- Return the number of the next property
            let
                nextSizeByte =
                    Memory.readByte nextAddr mem
            in
            Bitwise.and nextSizeByte 0x1F

        else
            findNextPropertyNumber nextAddr targetPropNum mem



-- ARITHMETIC HELPERS


toSigned : Int -> Int
toSigned n =
    if n > 32767 then
        n - 65536

    else
        n


toUnsigned : Int -> Int
toUnsigned n =
    Bitwise.and n 0xFFFF


truncDiv : Int -> Int -> Int
truncDiv a b =
    let
        result =
            toFloat a / toFloat b
    in
    if result < 0 then
        ceiling result

    else
        floor result


truncMod : Int -> Int -> Int
truncMod a b =
    a - truncDiv a b * b



-- GENERAL HELPERS


getOp : Int -> List Int -> Int
getOp index ops =
    listGet index ops |> Maybe.withDefault 0


listGet : Int -> List a -> Maybe a
listGet index list =
    list |> List.drop index |> List.head


variableRefFromByte : Int -> VariableRef
variableRefFromByte byte =
    if byte == 0 then
        Stack

    else if byte <= 0x0F then
        Local byte

    else
        Global byte


{-| Read a variable whose number was supplied as an operand to an
"indirect" opcode (inc, dec, inc\_chk, dec\_chk, load, store, pull).
Per Z-machine Standard §6.3.4, when the variable number is 0 the
stack is read _in place_ (peek), not popped.
-}
readIndirect : Int -> ZMachine -> ( Int, ZMachine )
readIndirect byte machine =
    if byte == 0 then
        ( State.peekStack machine, machine )

    else
        State.readVariable (variableRefFromByte byte) machine


{-| Write a variable whose number was supplied as an operand to an
"indirect" opcode. Per Z-machine Standard §6.3.4, when the variable
number is 0 the stack is written _in place_ (poke), not pushed.
-}
writeIndirect : Int -> Int -> ZMachine -> ZMachine
writeIndirect byte value machine =
    if byte == 0 then
        State.pokeStack value machine

    else
        State.writeVariable (variableRefFromByte byte) value machine
