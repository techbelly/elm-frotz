module ZMachine.Instruction exposing
    ( Instruction
    , Opcode(..)
    , Op2(..)
    , Op1(..)
    , Op0(..)
    , OpVar(..)
    , Operand(..)
    , VariableRef(..)
    , BranchInfo
    , BranchTarget(..)
    , decode
    , storesResult
    , branches
    , hasText
    , variableRefFromByte
    )

{-| Z-Machine instruction decoder.

Decodes a single instruction from memory at a given program counter address.
Handles all four instruction forms: short, long, variable, and the operand
type / opcode number extraction for each.

@docs Instruction, Opcode, Op2, Op1, Op0, OpVar
@docs Operand, VariableRef, BranchInfo, BranchTarget
@docs decode, storesResult, branches, hasText

-}

import Bitwise
import ZMachine.Memory as Memory exposing (Memory)


{-| A fully decoded Z-Machine instruction.
-}
type alias Instruction =
    { opcode : Opcode
    , operands : List Operand
    , store : Maybe VariableRef
    , branch : Maybe BranchInfo
    , textLiteral : Maybe (List Int)
    , length : Int
    }


{-| Top-level opcode, tagged by operand count category.
-}
type Opcode
    = Op2 Op2
    | Op1 Op1
    | Op0 Op0
    | OpVar OpVar


{-| Two-operand opcodes (V3).
-}
type Op2
    = Je
    | Jl
    | Jg
    | DecChk
    | IncChk
    | Jin
    | Test
    | Or
    | And
    | TestAttr
    | SetAttr
    | ClearAttr
    | Store
    | InsertObj
    | Loadw
    | Loadb
    | GetProp
    | GetPropAddr
    | GetNextProp
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | Unknown2Op Int


{-| One-operand opcodes (V3).
-}
type Op1
    = Jz
    | GetSibling
    | GetChild
    | GetParent
    | GetPropLen
    | Inc
    | Dec
    | PrintAddr
    | CallS1
    | RemoveObj
    | PrintObj
    | Ret
    | Jump
    | PrintPaddr
    | Load
    | Not
    | Unknown1Op Int


{-| Zero-operand opcodes (V3).
-}
type Op0
    = Rtrue
    | Rfalse
    | Print
    | PrintRet
    | Nop
    | Save
    | Restore
    | Restart
    | RetPopped
    | Pop
    | Quit
    | NewLine
    | ShowStatus
    | Verify
    | Piracy
    | Unknown0Op Int


{-| Variable-operand opcodes (V3).
-}
type OpVar
    = Call
    | Storew
    | Storeb
    | PutProp
    | Sread
    | PrintChar
    | PrintNum
    | Random
    | Push
    | Pull
    | SplitWindow
    | SetWindow
    | OutputStream
    | InputStream
    | SoundEffect
    | UnknownVar Int


{-| An instruction operand.
-}
type Operand
    = LargeConstant Int
    | SmallConstant Int
    | Variable VariableRef


{-| A reference to a Z-Machine variable.
-}
type VariableRef
    = Stack
    | Local Int
    | Global Int


{-| Branch information attached to branch instructions.
-}
type alias BranchInfo =
    { condition : Bool
    , target : BranchTarget
    }


{-| Where a branch goes.
-}
type BranchTarget
    = ReturnFalse
    | ReturnTrue
    | Offset Int



-- OPCODE NUMBER → TYPE MAPPINGS


op2FromNumber : Int -> Op2
op2FromNumber n =
    case n of
        1 -> Je
        2 -> Jl
        3 -> Jg
        4 -> DecChk
        5 -> IncChk
        6 -> Jin
        7 -> Test
        8 -> Or
        9 -> And
        10 -> TestAttr
        11 -> SetAttr
        12 -> ClearAttr
        13 -> Store
        14 -> InsertObj
        15 -> Loadw
        16 -> Loadb
        17 -> GetProp
        18 -> GetPropAddr
        19 -> GetNextProp
        20 -> Add
        21 -> Sub
        22 -> Mul
        23 -> Div
        24 -> Mod
        _ -> Unknown2Op n


op1FromNumber : Int -> Op1
op1FromNumber n =
    case n of
        0 -> Jz
        1 -> GetSibling
        2 -> GetChild
        3 -> GetParent
        4 -> GetPropLen
        5 -> Inc
        6 -> Dec
        7 -> PrintAddr
        8 -> CallS1
        9 -> RemoveObj
        10 -> PrintObj
        11 -> Ret
        12 -> Jump
        13 -> PrintPaddr
        14 -> Load
        15 -> Not
        _ -> Unknown1Op n


op0FromNumber : Int -> Op0
op0FromNumber n =
    case n of
        0 -> Rtrue
        1 -> Rfalse
        2 -> Print
        3 -> PrintRet
        4 -> Nop
        5 -> Save
        6 -> Restore
        7 -> Restart
        8 -> RetPopped
        9 -> Pop
        10 -> Quit
        11 -> NewLine
        12 -> ShowStatus
        13 -> Verify
        15 -> Piracy
        _ -> Unknown0Op n


opVarFromNumber : Int -> OpVar
opVarFromNumber n =
    case n of
        0 -> Call
        1 -> Storew
        2 -> Storeb
        3 -> PutProp
        4 -> Sread
        5 -> PrintChar
        6 -> PrintNum
        7 -> Random
        8 -> Push
        9 -> Pull
        10 -> SplitWindow
        11 -> SetWindow
        19 -> OutputStream
        20 -> InputStream
        21 -> SoundEffect
        _ -> UnknownVar n



-- OPCODE CLASSIFICATION


{-| Does this opcode store a result into a variable?
-}
storesResult : Opcode -> Bool
storesResult opcode =
    case opcode of
        Op2 Or -> True
        Op2 And -> True
        Op2 Loadw -> True
        Op2 Loadb -> True
        Op2 GetProp -> True
        Op2 GetPropAddr -> True
        Op2 GetNextProp -> True
        Op2 Add -> True
        Op2 Sub -> True
        Op2 Mul -> True
        Op2 Div -> True
        Op2 Mod -> True
        Op1 GetSibling -> True
        Op1 GetChild -> True
        Op1 GetParent -> True
        Op1 GetPropLen -> True
        Op1 Load -> True
        Op1 Not -> True
        Op1 CallS1 -> True
        OpVar Call -> True
        OpVar Random -> True
        _ -> False


{-| Does this opcode have a branch offset?
-}
branches : Opcode -> Bool
branches opcode =
    case opcode of
        Op2 Je -> True
        Op2 Jl -> True
        Op2 Jg -> True
        Op2 DecChk -> True
        Op2 IncChk -> True
        Op2 Jin -> True
        Op2 Test -> True
        Op2 TestAttr -> True
        Op1 Jz -> True
        Op1 GetSibling -> True
        Op1 GetChild -> True
        Op0 Save -> True
        Op0 Restore -> True
        Op0 Verify -> True
        Op0 Piracy -> True
        _ -> False


{-| Does this opcode have an inline text literal?
-}
hasText : Opcode -> Bool
hasText opcode =
    case opcode of
        Op0 Print -> True
        Op0 PrintRet -> True
        _ -> False



-- DECODING


{-| Decode a single instruction from memory at the given address.
Returns the decoded instruction, or Nothing if the opcode byte is unreadable.
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
    if opType == 3 then
        -- 0OP
        let
            opcode =
                Op0 (op0FromNumber opcodeNum)
        in
        decodePostOperands pc pos opcode [] mem

    else
        -- 1OP
        let
            opcode =
                Op1 (op1FromNumber opcodeNum)

            ( operand, nextPos ) =
                readOperandByType opType pos mem
        in
        decodePostOperands pc nextPos opcode [ operand ] mem



-- LONG FORM: top 2 bits = 00 or 01


decodeLong : Int -> Int -> Memory -> Instruction
decodeLong pc opcodeByte mem =
    let
        opcodeNum =
            Bitwise.and opcodeByte 0x1F

        opcode =
            Op2 (op2FromNumber opcodeNum)

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
                Op2 (op2FromNumber opcodeNum)

            else
                OpVar (opVarFromNumber opcodeNum)

        typesByte =
            Memory.readByte (pc + 1) mem

        ( operands, nextPos ) =
            readOperandsFromTypes typesByte (pc + 2) mem
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
            ( Variable (variableRefFromByte (Memory.readByte pos mem)), pos + 1 )

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
        ( store, posAfterStore ) =
            if storesResult opcode then
                ( Just (variableRefFromByte (Memory.readByte pos mem)), pos + 1 )

            else
                ( Nothing, pos )

        ( branch, posAfterBranch ) =
            if branches opcode then
                decodeBranch posAfterStore mem

            else
                ( Nothing, posAfterStore )

        ( text, posAfterText ) =
            if hasText opcode then
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
    decodeZWords pos mem []


decodeZWords : Int -> Memory -> List Int -> ( Maybe (List Int), Int )
decodeZWords pos mem acc =
    let
        word =
            Memory.readWord pos mem

        newAcc =
            word :: acc

        isEnd =
            Bitwise.and word 0x8000 /= 0
    in
    if isEnd then
        ( Just (List.reverse newAcc), pos + Memory.wordLength )

    else
        decodeZWords (pos + Memory.wordLength) mem newAcc



-- VARIABLE REF


{-| Decode a raw variable-reference byte into a [`VariableRef`](#VariableRef).
`0` = evaluation stack, `1..15` = local, `16..255` = global.
-}
variableRefFromByte : Int -> VariableRef
variableRefFromByte byte =
    if byte == 0 then
        Stack

    else if byte <= 0x0F then
        Local byte

    else
        Global byte
