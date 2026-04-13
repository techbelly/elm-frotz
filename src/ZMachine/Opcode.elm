module ZMachine.Opcode exposing
    ( Instruction
    , Opcode(..)
    , Op2(..)
    , Op1(..)
    , Op0(..)
    , OpVar(..)
    , OpExt(..)
    , Operand(..)
    , VariableRef(..)
    , BranchInfo
    , BranchTarget(..)
    , storesResult
    , branches
    , hasText
    , op2FromNumber
    , op1FromNumber
    , op0FromNumber
    , opVarFromNumber
    , opExtFromNumber
    , variableRefFromByte
    )

{-| Z-Machine instruction data model.

This module defines the tagged types that represent a decoded Z-Machine
instruction along with the purely structural classification of each
opcode (does it store a result, does it branch, does it carry an inline
text literal). The actual byte-level decoding lives in
[`ZMachine.Decode`](ZMachine-Decode).

@docs Instruction, Opcode, Op2, Op1, Op0, OpVar, OpExt
@docs Operand, VariableRef, BranchInfo, BranchTarget
@docs storesResult, branches, hasText
@docs op2FromNumber, op1FromNumber, op0FromNumber, opVarFromNumber, opExtFromNumber
@docs variableRefFromByte

-}

import ZMachine.Memory as Memory


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
    | OpExt OpExt


{-| Two-operand opcodes.
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
    | CallS2
    | CallN2
    | SetColour
    | Throw
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
    | CallN1
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


{-| Variable-operand opcodes.
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
    | CallVs2
    | EraseWindow
    | EraseLine
    | SetCursor
    | GetCursor
    | SetTextStyle
    | BufferMode
    | OutputStream
    | InputStream
    | SoundEffect
    | ReadChar
    | ScanTable
    | NotV5
    | CallVn
    | CallVn2
    | Tokenise
    | EncodeText
    | CopyTable
    | PrintTable
    | CheckArgCount
    | UnknownVar Int


{-| Extended opcodes (V5+, reached via 0xBE prefix).
-}
type OpExt
    = ExtSave
    | ExtRestore
    | LogShift
    | ArtShift
    | SetFont
    | SaveUndo
    | RestoreUndo
    | UnknownExt Int


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


{-| Decode a raw 2OP opcode number.
-}
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
        25 -> CallS2
        26 -> CallN2
        27 -> SetColour
        28 -> Throw
        _ -> Unknown2Op n


{-| Decode a raw 1OP opcode number.
In V5, opcode 15 is `call_1n` instead of `not`.
-}
op1FromNumber : Memory.Version -> Int -> Op1
op1FromNumber version n =
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
        15 ->
            case version of
                Memory.V3 -> Not
                Memory.V5 -> CallN1
        _ -> Unknown1Op n


{-| Decode a raw 0OP opcode number.
-}
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


{-| Decode a raw VAR opcode number.
-}
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
        12 -> CallVs2
        13 -> EraseWindow
        14 -> EraseLine
        15 -> SetCursor
        16 -> GetCursor
        17 -> SetTextStyle
        18 -> BufferMode
        19 -> OutputStream
        20 -> InputStream
        21 -> SoundEffect
        22 -> ReadChar
        23 -> ScanTable
        24 -> NotV5
        25 -> CallVn
        26 -> CallVn2
        27 -> Tokenise
        28 -> EncodeText
        29 -> CopyTable
        30 -> PrintTable
        31 -> CheckArgCount
        _ -> UnknownVar n



{-| Decode a raw EXT opcode number (V5+).
-}
opExtFromNumber : Int -> OpExt
opExtFromNumber n =
    case n of
        0 -> ExtSave
        1 -> ExtRestore
        2 -> LogShift
        3 -> ArtShift
        4 -> SetFont
        9 -> SaveUndo
        10 -> RestoreUndo
        _ -> UnknownExt n



-- OPCODE CLASSIFICATION


{-| Does this opcode store a result into a variable?
In V5, Save/Restore store a result instead of branching, and several
new opcodes store results.
-}
storesResult : Memory.Version -> Opcode -> Bool
storesResult version opcode =
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
        Op2 CallS2 -> True
        Op1 GetSibling -> True
        Op1 GetChild -> True
        Op1 GetParent -> True
        Op1 GetPropLen -> True
        Op1 Load -> True
        Op1 Not -> True
        Op1 CallS1 -> True
        OpVar Call -> True
        OpVar CallVs2 -> True
        OpVar Random -> True
        OpVar ReadChar -> True
        OpVar ScanTable -> True
        OpVar NotV5 -> True
        OpVar Sread ->
            case version of
                Memory.V5 -> True
                Memory.V3 -> False
        Op0 Save ->
            case version of
                Memory.V5 -> True
                Memory.V3 -> False
        Op0 Restore ->
            case version of
                Memory.V5 -> True
                Memory.V3 -> False
        Op0 Pop ->
            -- In V5, 0OP:9 is catch (stores result) instead of pop
            case version of
                Memory.V5 -> True
                Memory.V3 -> False
        OpExt ExtSave -> True
        OpExt ExtRestore -> True
        OpExt LogShift -> True
        OpExt ArtShift -> True
        OpExt SetFont -> True
        OpExt SaveUndo -> True
        OpExt RestoreUndo -> True
        _ -> False


{-| Does this opcode have a branch offset?
In V5, Save/Restore no longer branch (they store instead).
-}
branches : Memory.Version -> Opcode -> Bool
branches version opcode =
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
        Op0 Save ->
            case version of
                Memory.V3 -> True
                Memory.V5 -> False
        Op0 Restore ->
            case version of
                Memory.V3 -> True
                Memory.V5 -> False
        Op0 Verify -> True
        Op0 Piracy -> True
        OpVar ScanTable -> True
        OpVar CheckArgCount -> True
        _ -> False


{-| Does this opcode have an inline text literal?
-}
hasText : Opcode -> Bool
hasText opcode =
    case opcode of
        Op0 Print -> True
        Op0 PrintRet -> True
        _ -> False



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
