module ZMachine.Decode exposing (decode)

{-| Z-Machine instruction decoder.

Decodes a single instruction from memory at a given program counter
address. Handles all four instruction forms — short, long, variable —
and the operand-type / opcode-number extraction for each. The resulting
[`Instruction`](ZMachine-Opcode#Instruction) records are consumed by
[`ZMachine.Execute`](ZMachine-Execute).

@docs decode

-}

import Bitwise
import ZMachine.Memory as Memory exposing (Memory)
import ZMachine.Opcode as Opcode
    exposing
        ( BranchInfo
        , BranchTarget(..)
        , Instruction
        , OpExt(..)
        , Opcode(..)
        , Operand(..)
        )
import ZMachine.Text as Text


{-| Decode a single instruction from memory at the given address.
-}
decode : Int -> Memory -> Instruction
decode pc mem =
    let
        opcodeByte =
            Memory.readByte pc mem

        topBits =
            Bitwise.shiftRightZfBy 6 opcodeByte
    in
    case topBits of
        3 ->
            decodeVariable pc opcodeByte mem

        2 ->
            decodeShort pc opcodeByte mem

        _ ->
            decodeLong pc opcodeByte mem



-- SHORT FORM: top 2 bits = 10


decodeShort : Int -> Int -> Memory -> Instruction
decodeShort pc opcodeByte mem =
    let
        opType =
            Bitwise.and (Bitwise.shiftRightZfBy 4 opcodeByte) 0x03

        opcodeNum =
            Bitwise.and opcodeByte 0x0F

        pos =
            pc + 1
    in
    if opType == 3 && opcodeNum == 14 && (Memory.profile mem).version == Memory.V5 then
        -- Extended form (0xBE): V5+ only
        decodeExtended pc mem

    else if opType == 3 then
        -- 0OP
        let
            opcode =
                Op0 (Opcode.op0FromNumber opcodeNum)
        in
        decodePostOperands pc pos opcode [] mem

    else
        -- 1OP
        let
            opcode =
                Op1 (Opcode.op1FromNumber (Memory.profile mem).version opcodeNum)

            ( operand, nextPos ) =
                readOperandByType opType pos mem
        in
        decodePostOperands pc nextPos opcode [ operand ] mem



-- EXTENDED FORM: 0xBE prefix (V5+)


decodeExtended : Int -> Memory -> Instruction
decodeExtended pc mem =
    let
        extOpcodeNum =
            Memory.readByte (pc + 1) mem

        opcode =
            OpExt (Opcode.opExtFromNumber extOpcodeNum)

        typesByte =
            Memory.readByte (pc + 2) mem

        ( operands, nextPos ) =
            readOperandsFromTypes typesByte (pc + 3) mem
    in
    decodePostOperands pc nextPos opcode operands mem



-- LONG FORM: top 2 bits = 00 or 01


decodeLong : Int -> Int -> Memory -> Instruction
decodeLong pc opcodeByte mem =
    let
        opcodeNum =
            Bitwise.and opcodeByte 0x1F

        opcode =
            Op2 (Opcode.op2FromNumber opcodeNum)

        -- Bit 6: first operand type (0=small constant, 1=variable)
        type1 =
            if Bitwise.and opcodeByte 0x40 /= 0 then
                2
                -- variable
            else
                1

        -- small constant
        -- Bit 5: second operand type
        type2 =
            if Bitwise.and opcodeByte 0x20 /= 0 then
                2
            else
                1

        pos1 =
            pc + 1

        ( op1, pos2 ) =
            readOperandByType type1 pos1 mem

        ( op2, pos3 ) =
            readOperandByType type2 pos2 mem
    in
    decodePostOperands pc pos3 opcode [ op1, op2 ] mem



-- VARIABLE FORM: top 2 bits = 11


decodeVariable : Int -> Int -> Memory -> Instruction
decodeVariable pc opcodeByte mem =
    let
        opcodeNum =
            Bitwise.and opcodeByte 0x1F

        -- Bit 5: 0 = 2OP, 1 = VAR
        opcode =
            if Bitwise.and opcodeByte 0x20 == 0 then
                Op2 (Opcode.op2FromNumber opcodeNum)

            else
                OpVar (Opcode.opVarFromNumber opcodeNum)

        -- call_vs2 (VAR:12) and call_vn2 (VAR:26) use two types bytes
        hasDoubleTypes =
            Bitwise.and opcodeByte 0x20 /= 0 && (opcodeNum == 12 || opcodeNum == 26)

        typesByte1 =
            Memory.readByte (pc + 1) mem

        ( operands, nextPos ) =
            if hasDoubleTypes then
                let
                    typesByte2 =
                        Memory.readByte (pc + 2) mem

                    ( ops1, pos1 ) =
                        readOperandsFromTypes typesByte1 (pc + 3) mem

                    ( ops2, pos2 ) =
                        readOperandsFromTypes typesByte2 pos1 mem
                in
                ( ops1 ++ ops2, pos2 )

            else
                readOperandsFromTypes typesByte1 (pc + 2) mem
    in
    decodePostOperands pc nextPos opcode operands mem



-- OPERAND READING


{-| Read a single operand given a 2-bit type code.

  - 0 = large constant (2 bytes)
  - 1 = small constant (1 byte)
  - 2 = variable (1 byte)

-}
readOperandByType : Int -> Int -> Memory -> ( Operand, Int )
readOperandByType opType pos mem =
    case opType of
        0 ->
            ( LargeConstant (Memory.readWord pos mem), pos + Memory.wordLength )

        1 ->
            ( SmallConstant (Memory.readByte pos mem), pos + 1 )

        2 ->
            ( Variable (Opcode.variableRefFromByte (Memory.readByte pos mem)), pos + 1 )

        _ ->
            -- Type 3 = omitted, shouldn't be called
            ( SmallConstant 0, pos )


{-| Read operands according to a types byte (4 x 2-bit fields, MSB first).
Stops at the first "omitted" (11) type.
-}
readOperandsFromTypes : Int -> Int -> Memory -> ( List Operand, Int )
readOperandsFromTypes typesByte startPos mem =
    let
        types =
            [ Bitwise.and (Bitwise.shiftRightZfBy 6 typesByte) 0x03
            , Bitwise.and (Bitwise.shiftRightZfBy 4 typesByte) 0x03
            , Bitwise.and (Bitwise.shiftRightZfBy 2 typesByte) 0x03
            , Bitwise.and typesByte 0x03
            ]
    in
    readOperandsList types startPos mem []


readOperandsList : List Int -> Int -> Memory -> List Operand -> ( List Operand, Int )
readOperandsList types pos mem acc =
    case types of
        [] ->
            ( List.reverse acc, pos )

        t :: rest ->
            if t == 3 then
                -- Omitted: stop here
                ( List.reverse acc, pos )

            else
                let
                    ( operand, nextPos ) =
                        readOperandByType t pos mem
                in
                readOperandsList rest nextPos mem (operand :: acc)



-- POST-OPERAND DECODING (store, branch, text)


decodePostOperands : Int -> Int -> Opcode -> List Operand -> Memory -> Instruction
decodePostOperands pc pos opcode operands mem =
    let
        version =
            (Memory.profile mem).version

        ( store, posAfterStore ) =
            if Opcode.storesResult version opcode then
                ( Just (Opcode.variableRefFromByte (Memory.readByte pos mem)), pos + 1 )

            else
                ( Nothing, pos )

        ( branch, posAfterBranch ) =
            if Opcode.branches version opcode then
                decodeBranch posAfterStore mem

            else
                ( Nothing, posAfterStore )

        ( text, posAfterText ) =
            if Opcode.hasText opcode then
                decodeInlineText posAfterBranch mem

            else
                ( Nothing, posAfterBranch )
    in
    { opcode = opcode
    , operands = operands
    , store = store
    , branch = branch
    , textLiteral = text
    , length = posAfterText - pc
    }


{-| Decode a branch offset. Returns (BranchInfo, next position).
-}
decodeBranch : Int -> Memory -> ( Maybe BranchInfo, Int )
decodeBranch pos mem =
    let
        firstByte =
            Memory.readByte pos mem

        condition =
            Bitwise.and firstByte 0x80 /= 0

        singleByte =
            Bitwise.and firstByte 0x40 /= 0
    in
    if singleByte then
        let
            offset =
                Bitwise.and firstByte 0x3F
        in
        ( Just { condition = condition, target = branchTargetFromOffset offset }
        , pos + 1
        )

    else
        let
            highBits =
                Bitwise.and firstByte 0x3F

            lowByte =
                Memory.readByte (pos + 1) mem

            raw14 =
                Bitwise.or (Bitwise.shiftLeftBy 8 highBits) lowByte

            -- 14-bit signed value
            offset =
                if raw14 > 8191 then
                    raw14 - 16384

                else
                    raw14
        in
        ( Just { condition = condition, target = branchTargetFromOffset offset }
        , pos + Memory.wordLength
        )


branchTargetFromOffset : Int -> BranchTarget
branchTargetFromOffset offset =
    case offset of
        0 ->
            ReturnFalse

        1 ->
            ReturnTrue

        _ ->
            Offset offset


{-| Decode an inline Z-encoded text string. Returns the raw Z-character words
(as a list of 16-bit ints) and the position after the string.
We store the raw words here; actual decoding to a String happens in ZMachine.Text.
-}
decodeInlineText : Int -> Memory -> ( Maybe (List Int), Int )
decodeInlineText pos mem =
    let
        ( words, endPos ) =
            Text.readZWords pos mem []
    in
    ( Just words, endPos )
