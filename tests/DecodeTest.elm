module DecodeTest exposing (suite)

import Bitwise
import Bytes.Encode as Encode
import Expect
import Test exposing (Test, describe, test)
import ZMachine.Decode as Decode
import ZMachine.Memory as Memory exposing (Memory)
import ZMachine.Opcode as Opcode
    exposing
        ( BranchTarget(..)
        , Op0(..)
        , Op1(..)
        , Op2(..)
        , OpVar(..)
        , Opcode(..)
        , Operand(..)
        , VariableRef(..)
        )


suite : Test
suite =
    describe "ZMachine.Decode"
        [ shortFormTests
        , longFormTests
        , variableFormTests
        , branchTests
        , storeTests
        , inlineTextTests
        , lengthTests
        , opcodeClassificationTests
        ]



-- TEST HELPERS


{-| Build a memory image with given bytes placed at address 0x80 (in static
memory for easy instruction placement). We set static base to 0x40 so 0x80
is in static/high memory where instructions typically live. But for simplicity,
let's place instructions in dynamic memory so we can also test writes.

We use a 512-byte image, static base at 256, and place instruction bytes
starting at address 64 (safely in dynamic memory, past the header).
-}
memWithBytes : List Int -> Memory
memWithBytes instrBytes =
    let
        totalSize =
            512

        staticBase =
            256

        base =
            List.repeat totalSize 0
                |> setAt 0 3
                |> setAt 14 (Bitwise.shiftRightZfBy 8 staticBase)
                |> setAt 15 (Bitwise.and staticBase 0xFF)

        withInstr =
            List.foldl
                (\( i, b ) acc -> setAt (instrAddr + i) b acc)
                base
                (List.indexedMap Tuple.pair instrBytes)
    in
    withInstr
        |> List.map Encode.unsignedInt8
        |> Encode.sequence
        |> Encode.encode
        |> Memory.fromBytes
        |> unwrap


instrAddr : Int
instrAddr =
    64


unwrap : Result String a -> a
unwrap result =
    case result of
        Ok v ->
            v

        Err msg ->
            Debug.todo msg


setAt : Int -> Int -> List Int -> List Int
setAt index value list =
    List.indexedMap
        (\i v ->
            if i == index then
                value

            else
                v
        )
        list



-- SHORT FORM TESTS


shortFormTests : Test
shortFormTests =
    describe "short form"
        [ test "0OP: rtrue (0xB0)" <|
            \_ ->
                let
                    mem =
                        memWithBytes [ 0xB0 ]

                    instr =
                        Decode.decode instrAddr mem
                in
                instr.opcode
                    |> Expect.equal (Op0 Rtrue)
        , test "0OP: rfalse (0xB1)" <|
            \_ ->
                let
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0xB1 ])
                in
                instr.opcode
                    |> Expect.equal (Op0 Rfalse)
        , test "0OP: print (0xB2) has no operands" <|
            \_ ->
                let
                    -- 0xB2 followed by a Z-string word with end bit set: 0x8000 + some chars
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0xB2, 0x80, 0x00 ])
                in
                instr.operands
                    |> Expect.equal []
        , test "0OP: quit (0xBA)" <|
            \_ ->
                let
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0xBA ])
                in
                instr.opcode
                    |> Expect.equal (Op0 Quit)
        , test "1OP with large constant: bits 4-5 = 00" <|
            \_ ->
                let
                    -- 0x8F = 10 00 1111 = short form, large constant, opcode 15 (not)
                    -- Followed by 2-byte large constant 0x1234
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0x8F, 0x12, 0x34 ])
                in
                ( instr.opcode, instr.operands )
                    |> Expect.equal ( Op1 Not, [ LargeConstant 0x1234 ] )
        , test "1OP with small constant: bits 4-5 = 01" <|
            \_ ->
                let
                    -- 0x90 = 10 01 0000 = short form, small constant, opcode 0 (jz)
                    -- Followed by 1-byte small constant 0x42, then branch byte
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0x90, 0x42, 0xC0 ])
                in
                ( instr.opcode, instr.operands )
                    |> Expect.equal ( Op1 Jz, [ SmallConstant 0x42 ] )
        , test "1OP with variable: bits 4-5 = 10" <|
            \_ ->
                let
                    -- 0xAB = 10 10 1011 = short form, variable, opcode 11 (ret)
                    -- Variable 0x03 = Local 3
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0xAB, 0x03 ])
                in
                ( instr.opcode, instr.operands )
                    |> Expect.equal ( Op1 Ret, [ Variable (Local 3) ] )
        , test "1OP with stack variable (0x00)" <|
            \_ ->
                let
                    -- 0xAB = ret with variable operand, variable 0x00 = Stack
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0xAB, 0x00 ])
                in
                instr.operands
                    |> Expect.equal [ Variable Stack ]
        , test "1OP with global variable (0x10+)" <|
            \_ ->
                let
                    -- 0xAB = ret with variable operand, variable 0x10 = Global 0x10
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0xAB, 0x10 ])
                in
                instr.operands
                    |> Expect.equal [ Variable (Global 0x10) ]
        ]



-- LONG FORM TESTS


longFormTests : Test
longFormTests =
    describe "long form"
        [ test "2OP with two small constants (bits 6,5 = 0,0)" <|
            \_ ->
                let
                    -- 0x01 = 00 0 0 00001 = long, small, small, opcode 1 (je)
                    -- operands: 0x05, 0x0A
                    -- branch byte: 0xC0 (condition=true, single byte, offset 0 = return false)
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0x01, 0x05, 0x0A, 0xC0 ])
                in
                ( instr.opcode, instr.operands )
                    |> Expect.equal ( Op2 Je, [ SmallConstant 0x05, SmallConstant 0x0A ] )
        , test "2OP with variable + small (bits 6,5 = 1,0)" <|
            \_ ->
                let
                    -- 0x54 = 01 0 1 10100 = long, variable(bit6=1), small(bit5=0), opcode 20 (add)
                    -- operands: var 0x02 (Local 2), small 0x03
                    -- store: variable 0x01 (Local 1)
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0x54, 0x02, 0x03, 0x01 ])
                in
                ( instr.opcode, instr.operands, instr.store )
                    |> Expect.equal
                        ( Op2 Add
                        , [ Variable (Local 2), SmallConstant 0x03 ]
                        , Just (Local 1)
                        )
        , test "2OP with small + variable (bits 6,5 = 0,1)" <|
            \_ ->
                let
                    -- 0x35 = 0b00110101
                    -- bits 7-6: 00 (long form)
                    -- bit 6: 0 → first operand is small constant
                    -- bit 5: 1 → second operand is variable
                    -- bits 4-0: 10101 = 21 (sub)
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0x35, 0x01, 0x02, 0x00 ])
                in
                ( instr.opcode, instr.operands, instr.store )
                    |> Expect.equal
                        ( Op2 Sub
                        , [ SmallConstant 0x01, Variable (Local 2) ]
                        , Just Stack
                        )
        , test "2OP je opcode number 1" <|
            \_ ->
                let
                    -- 0x01 = long, small, small, je
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0x01, 0x00, 0x00, 0xC0 ])
                in
                instr.opcode
                    |> Expect.equal (Op2 Je)
        , test "2OP loadw (opcode 15) stores result" <|
            \_ ->
                let
                    -- 0x0F = long, small, small, loadw(15)
                    -- operands: array addr, index
                    -- store variable
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0x0F, 0x10, 0x02, 0x05 ])
                in
                instr.store
                    |> Expect.equal (Just (Local 5))
        ]



-- VARIABLE FORM TESTS


variableFormTests : Test
variableFormTests =
    describe "variable form"
        [ test "VAR call with 1 arg" <|
            \_ ->
                let
                    -- 0xE0 = 1110 0000 = variable form, bit5=1 (VAR), opcode 0 (call)
                    -- types: 00 01 11 11 = large, small, omit, omit → 0x1F
                    -- operands: large 0x1234, small 0x05
                    -- store: variable 0x00 (stack)
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0xE0, 0x1F, 0x12, 0x34, 0x05, 0x00 ])
                in
                ( instr.opcode, instr.operands, instr.store )
                    |> Expect.equal
                        ( OpVar Call
                        , [ LargeConstant 0x1234, SmallConstant 0x05 ]
                        , Just Stack
                        )
        , test "VAR call with no args (just routine address)" <|
            \_ ->
                let
                    -- types: 00 11 11 11 = large, omit, omit, omit → 0x3F
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0xE0, 0x3F, 0x12, 0x34, 0x00 ])
                in
                ( instr.opcode, instr.operands )
                    |> Expect.equal ( OpVar Call, [ LargeConstant 0x1234 ] )
        , test "VAR storew with 3 args" <|
            \_ ->
                let
                    -- 0xE1 = variable form, VAR, opcode 1 (storew)
                    -- types: 00 01 01 11 = large, small, small, omit → 0b00010111 = 0x17
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0xE1, 0x17, 0x10, 0x00, 0x02, 0xAB ])
                in
                ( instr.opcode, instr.operands )
                    |> Expect.equal
                        ( OpVar Storew
                        , [ LargeConstant 0x1000, SmallConstant 0x02, SmallConstant 0xAB ]
                        )
        , test "variable form 2OP (bit 5 = 0): je with types byte" <|
            \_ ->
                let
                    -- 0xC1 = 1100 0001 = variable form, bit5=0 (2OP), opcode 1 (je)
                    -- This allows je to have more than 2 operands
                    -- types: 01 01 01 11 = small, small, small, omit → 0x57
                    -- branch byte after 3 operands
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0xC1, 0x57, 0x05, 0x0A, 0x0F, 0xC0 ])
                in
                ( instr.opcode, List.length instr.operands )
                    |> Expect.equal ( Op2 Je, 3 )
        , test "VAR with all operand types" <|
            \_ ->
                let
                    -- types: 00 01 10 11 = large, small, variable, omit → 0b00011011 = 0x1B
                    -- 0xE0 = call
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0xE0, 0x1B, 0xAB, 0xCD, 0x42, 0x03, 0x00 ])
                in
                instr.operands
                    |> Expect.equal
                        [ LargeConstant 0xABCD
                        , SmallConstant 0x42
                        , Variable (Local 3)
                        ]
        ]



-- BRANCH TESTS


branchTests : Test
branchTests =
    describe "branch decoding"
        [ test "single-byte branch, condition true, offset 0 = ReturnFalse" <|
            \_ ->
                let
                    -- jz (0x90) with small constant 0, then branch byte 0xC0
                    -- 0xC0 = 1 1 000000: condition=true(bit7), single(bit6), offset=0
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0x90, 0x00, 0xC0 ])
                in
                instr.branch
                    |> Expect.equal (Just { condition = True, target = ReturnFalse })
        , test "single-byte branch, condition true, offset 1 = ReturnTrue" <|
            \_ ->
                let
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0x90, 0x00, 0xC1 ])
                in
                instr.branch
                    |> Expect.equal (Just { condition = True, target = ReturnTrue })
        , test "single-byte branch, condition false, offset 2" <|
            \_ ->
                let
                    -- 0x42 = 0 1 000010: condition=false, single, offset=2
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0x90, 0x00, 0x42 ])
                in
                instr.branch
                    |> Expect.equal (Just { condition = False, target = Offset 2 })
        , test "two-byte branch, condition true, positive offset" <|
            \_ ->
                let
                    -- 0x80 0x10 = 1 0 000000 00010000: condition=true, two-byte, offset = 0x0010 = 16
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0x90, 0x00, 0x80, 0x10 ])
                in
                instr.branch
                    |> Expect.equal (Just { condition = True, target = Offset 16 })
        , test "two-byte branch, negative offset" <|
            \_ ->
                let
                    -- 0xBF 0xFE = 1 0 111111 11111110: condition=true, two-byte
                    -- raw14 = 0x3FFE = 16382, > 8191 so offset = 16382 - 16384 = -2
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0x90, 0x00, 0xBF, 0xFE ])
                in
                instr.branch
                    |> Expect.equal (Just { condition = True, target = Offset -2 })
        , test "no branch for non-branch opcode" <|
            \_ ->
                let
                    -- rtrue (0xB0) does not branch
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0xB0 ])
                in
                instr.branch
                    |> Expect.equal Nothing
        ]



-- STORE TESTS


storeTests : Test
storeTests =
    describe "store decoding"
        [ test "add stores to stack" <|
            \_ ->
                let
                    -- long form add: 0x14 = opcode 20
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0x14, 0x01, 0x02, 0x00 ])
                in
                instr.store
                    |> Expect.equal (Just Stack)
        , test "add stores to local" <|
            \_ ->
                let
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0x14, 0x01, 0x02, 0x05 ])
                in
                instr.store
                    |> Expect.equal (Just (Local 5))
        , test "add stores to global" <|
            \_ ->
                let
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0x14, 0x01, 0x02, 0x10 ])
                in
                instr.store
                    |> Expect.equal (Just (Global 0x10))
        , test "no store for non-store opcode (je)" <|
            \_ ->
                let
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0x01, 0x01, 0x02, 0xC0 ])
                in
                instr.store
                    |> Expect.equal Nothing
        , test "get_child has both store and branch" <|
            \_ ->
                let
                    -- 1OP get_child: short form, small constant
                    -- 0x92 = 10 01 0010 = short, small, opcode 2 (get_child)
                    -- operand: 0x05
                    -- store: 0x03 (local 3)
                    -- branch: 0xC1 (true, single, offset 1 = ReturnTrue)
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0x92, 0x05, 0x03, 0xC1 ])
                in
                ( instr.store, instr.branch )
                    |> Expect.equal
                        ( Just (Local 3)
                        , Just { condition = True, target = ReturnTrue }
                        )
        ]



-- INLINE TEXT TESTS


inlineTextTests : Test
inlineTextTests =
    describe "inline text"
        [ test "print has inline text" <|
            \_ ->
                let
                    -- 0xB2 = print, followed by Z-string
                    -- Single word with end bit: 0x8000
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0xB2, 0x80, 0x00 ])
                in
                instr.textLiteral
                    |> Expect.equal (Just [ 0x8000 ])
        , test "print_ret has inline text" <|
            \_ ->
                let
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0xB3, 0x80, 0x00 ])
                in
                instr.textLiteral
                    |> Expect.equal (Just [ 0x8000 ])
        , test "multi-word inline text" <|
            \_ ->
                let
                    -- Two words: first without end bit, second with end bit
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0xB2, 0x11, 0x22, 0x93, 0x44 ])
                in
                instr.textLiteral
                    |> Expect.equal (Just [ 0x1122, 0x9344 ])
        , test "non-print opcode has no text" <|
            \_ ->
                let
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0xB0 ])
                in
                instr.textLiteral
                    |> Expect.equal Nothing
        ]



-- LENGTH TESTS


lengthTests : Test
lengthTests =
    describe "instruction length"
        [ test "rtrue is 1 byte" <|
            \_ ->
                let
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0xB0 ])
                in
                instr.length
                    |> Expect.equal 1
        , test "long form 2OP with store is 4 bytes" <|
            \_ ->
                let
                    -- add: opcode(1) + small(1) + small(1) + store(1) = 4
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0x14, 0x01, 0x02, 0x00 ])
                in
                instr.length
                    |> Expect.equal 4
        , test "long form 2OP je with single-byte branch is 4 bytes" <|
            \_ ->
                let
                    -- je: opcode(1) + small(1) + small(1) + branch(1) = 4
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0x01, 0x05, 0x0A, 0xC0 ])
                in
                instr.length
                    |> Expect.equal 4
        , test "long form 2OP je with two-byte branch is 5 bytes" <|
            \_ ->
                let
                    -- je: opcode(1) + small(1) + small(1) + branch(2) = 5
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0x01, 0x05, 0x0A, 0x80, 0x10 ])
                in
                instr.length
                    |> Expect.equal 5
        , test "short form 1OP with large constant is 3 bytes" <|
            \_ ->
                let
                    -- not: opcode(1) + large(2) + store(1) = 4
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0x8F, 0x12, 0x34, 0x01 ])
                in
                instr.length
                    |> Expect.equal 4
        , test "variable form call with types byte and 2 operands + store" <|
            \_ ->
                let
                    -- call: opcode(1) + types(1) + large(2) + small(1) + store(1) = 6
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0xE0, 0x1F, 0x12, 0x34, 0x05, 0x00 ])
                in
                instr.length
                    |> Expect.equal 6
        , test "print with 2-word inline text is 5 bytes" <|
            \_ ->
                let
                    -- print: opcode(1) + 2 words of text(4) = 5
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0xB2, 0x11, 0x22, 0x93, 0x44 ])
                in
                instr.length
                    |> Expect.equal 5
        , test "get_child with store + branch" <|
            \_ ->
                let
                    -- short 1OP: opcode(1) + small(1) + store(1) + branch(1) = 4
                    instr =
                        Decode.decode instrAddr (memWithBytes [ 0x92, 0x05, 0x03, 0xC1 ])
                in
                instr.length
                    |> Expect.equal 4
        ]



-- OPCODE CLASSIFICATION TESTS


opcodeClassificationTests : Test
opcodeClassificationTests =
    describe "opcode classification"
        [ test "add stores result" <|
            \_ ->
                Opcode.storesResult (Op2 Add)
                    |> Expect.equal True
        , test "je does not store" <|
            \_ ->
                Opcode.storesResult (Op2 Je)
                    |> Expect.equal False
        , test "je branches" <|
            \_ ->
                Opcode.branches (Op2 Je)
                    |> Expect.equal True
        , test "add does not branch" <|
            \_ ->
                Opcode.branches (Op2 Add)
                    |> Expect.equal False
        , test "get_child both stores and branches" <|
            \_ ->
                ( Opcode.storesResult (Op1 GetChild), Opcode.branches (Op1 GetChild) )
                    |> Expect.equal ( True, True )
        , test "get_sibling both stores and branches" <|
            \_ ->
                ( Opcode.storesResult (Op1 GetSibling), Opcode.branches (Op1 GetSibling) )
                    |> Expect.equal ( True, True )
        , test "save branches" <|
            \_ ->
                Opcode.branches (Op0 Save)
                    |> Expect.equal True
        , test "print has text" <|
            \_ ->
                Opcode.hasText (Op0 Print)
                    |> Expect.equal True
        , test "print_ret has text" <|
            \_ ->
                Opcode.hasText (Op0 PrintRet)
                    |> Expect.equal True
        , test "rtrue has no text" <|
            \_ ->
                Opcode.hasText (Op0 Rtrue)
                    |> Expect.equal False
        , test "call stores" <|
            \_ ->
                Opcode.storesResult (OpVar Call)
                    |> Expect.equal True
        , test "random stores" <|
            \_ ->
                Opcode.storesResult (OpVar Random)
                    |> Expect.equal True
        , test "storew does not store" <|
            \_ ->
                Opcode.storesResult (OpVar Storew)
                    |> Expect.equal False
        ]
