module ExecuteTest exposing (suite)

import Array
import Bitwise
import Bytes.Encode as Encode
import Expect
import Test exposing (Test, describe, test)
import ZMachine.Execute as Execute exposing (Outcome(..))
import ZMachine.Memory as Memory
import ZMachine.Opcode exposing (VariableRef(..))
import ZMachine.State as State
import ZMachine.Types
    exposing
        ( OutputEvent(..)
        , ZMachine
        , ZMachineError(..)
        )


suite : Test
suite =
    describe "ZMachine.Execute"
        [ arithmeticTests
        , bitwiseTests
        , branchTests
        , variableTests
        , controlFlowTests
        , callTests
        , memoryAccessTests
        , ioTests
        , stackTests
        ]



-- TEST HELPERS


{-| Build a minimal Z-Machine state with instructions at the PC.

Memory layout:

  - Header at 0x00 (64 bytes): version=3, static base=0x100 (256)
  - Global variables at 0x80 (128 bytes, 64 vars)
  - Instructions placed at the PC (default 0x40 = 64)
  - Static base at 0x100

-}
makeZM : List Int -> ZMachine
makeZM instrBytes =
    makeZMAt 0x40 instrBytes


makeZMAt : Int -> List Int -> ZMachine
makeZMAt pc instrBytes =
    makeZMVersionAt 3 pc instrBytes


makeZMv5At : Int -> List Int -> ZMachine
makeZMv5At pc instrBytes =
    makeZMVersionAt 5 pc instrBytes


makeZMVersionAt : Int -> Int -> List Int -> ZMachine
makeZMVersionAt version pc instrBytes =
    let
        totalSize =
            512

        staticBase =
            0x0100

        globalsAddr =
            0x80

        base =
            List.repeat totalSize 0
                |> setAt 0 version
                -- version
                |> setAt 0x04 0x01
                |> setAt 0x05 0x00
                -- high memory base
                |> setAt 0x06 (Bitwise.shiftRightZfBy 8 pc)
                |> setAt 0x07 (Bitwise.and pc 0xFF)
                -- initial PC
                |> setAt 0x0C (Bitwise.shiftRightZfBy 8 globalsAddr)
                |> setAt 0x0D (Bitwise.and globalsAddr 0xFF)
                -- globals
                |> setAt 0x0E (Bitwise.shiftRightZfBy 8 staticBase)
                |> setAt 0x0F (Bitwise.and staticBase 0xFF)

        -- static base
        withInstr =
            List.foldl
                (\( i, b ) acc -> setAt (pc + i) b acc)
                base
                (List.indexedMap Tuple.pair instrBytes)

        mem =
            withInstr
                |> List.map Encode.unsignedInt8
                |> Encode.sequence
                |> Encode.encode
                |> Memory.fromBytes
                |> unwrap
    in
    State.init mem


{-| Build a ZM and set a global variable value before running.
-}
makeZMWithGlobal : Int -> Int -> List Int -> ZMachine
makeZMWithGlobal globalNum value instrBytes =
    let
        zm =
            makeZM instrBytes

        globalsAddr =
            0x80

        addr =
            globalsAddr + (globalNum - 0x10) * 2
    in
    { zm | memory = Memory.writeWord addr value zm.memory }


getOutputText : ZMachine -> String
getOutputText zm =
    List.reverse zm.output
        |> List.filterMap
            (\event ->
                case event of
                    PrintText s ->
                        Just s

                    NewLine ->
                        Just "\n"

                    _ ->
                        Nothing
            )
        |> String.concat


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



-- ARITHMETIC TESTS


arithmeticTests : Test
arithmeticTests =
    describe "arithmetic opcodes"
        [ test "add: 3 + 5 = 8" <|
            \_ ->
                -- Long form add: 0x14 = small+small add, operands 3, 5, store to stack (0x00)
                let
                    zm =
                        makeZM [ 0x14, 0x03, 0x05, 0x00 ]

                    result =
                        Execute.step zm
                in
                case result of
                    Continue m ->
                        m.stack |> Expect.equal [ 8 ]

                    _ ->
                        Expect.fail "Expected Continue"
        , test "sub: 10 - 3 = 7" <|
            \_ ->
                let
                    zm =
                        makeZM [ 0x15, 0x0A, 0x03, 0x00 ]

                    result =
                        Execute.step zm
                in
                case result of
                    Continue m ->
                        m.stack |> Expect.equal [ 7 ]

                    _ ->
                        Expect.fail "Expected Continue"
        , test "mul: 6 * 7 = 42" <|
            \_ ->
                let
                    zm =
                        makeZM [ 0x16, 0x06, 0x07, 0x00 ]

                    result =
                        Execute.step zm
                in
                case result of
                    Continue m ->
                        m.stack |> Expect.equal [ 42 ]

                    _ ->
                        Expect.fail "Expected Continue"
        , test "div: 17 / 5 = 3" <|
            \_ ->
                let
                    zm =
                        makeZM [ 0x17, 0x11, 0x05, 0x00 ]

                    result =
                        Execute.step zm
                in
                case result of
                    Continue m ->
                        m.stack |> Expect.equal [ 3 ]

                    _ ->
                        Expect.fail "Expected Continue"
        , test "div: -7 / 2 = -3 (truncates toward zero)" <|
            \_ ->
                let
                    -- -7 unsigned = 0xFFF9, need variable form for large constants
                    -- Use variable form add: 0xD7 = var form 2OP, opcode 0x17 (div)
                    -- types: 00 00 11 11 = large, large, omit, omit = 0x0F
                    zm =
                        makeZM [ 0xD7, 0x0F, 0xFF, 0xF9, 0x00, 0x02, 0x00 ]

                    result =
                        Execute.step zm
                in
                case result of
                    Continue m ->
                        -- -3 unsigned = 0xFFFD = 65533
                        m.stack |> Expect.equal [ 65533 ]

                    _ ->
                        Expect.fail "Expected Continue"
        , test "div by zero returns error" <|
            \_ ->
                let
                    zm =
                        makeZM [ 0x17, 0x0A, 0x00, 0x00 ]

                    result =
                        Execute.step zm
                in
                case result of
                    Error DivisionByZero _ ->
                        Expect.pass

                    _ ->
                        Expect.fail "Expected DivisionByZero error"
        , test "mod: 17 mod 5 = 2" <|
            \_ ->
                let
                    zm =
                        makeZM [ 0x18, 0x11, 0x05, 0x00 ]

                    result =
                        Execute.step zm
                in
                case result of
                    Continue m ->
                        m.stack |> Expect.equal [ 2 ]

                    _ ->
                        Expect.fail "Expected Continue"
        , test "add wraps at 16 bits: 0x7FFF + 1 = 0x8000" <|
            \_ ->
                let
                    -- Variable form for large constant
                    -- 0xD4 = var form 2OP, opcode 0x14 (add)
                    -- types: 00 01 11 11 = large, small, omit, omit = 0x1F
                    zm =
                        makeZM [ 0xD4, 0x1F, 0x7F, 0xFF, 0x01, 0x00 ]

                    result =
                        Execute.step zm
                in
                case result of
                    Continue m ->
                        m.stack |> Expect.equal [ 0x8000 ]

                    _ ->
                        Expect.fail "Expected Continue"
        ]



-- BITWISE TESTS


bitwiseTests : Test
bitwiseTests =
    describe "bitwise opcodes"
        [ test "or: 0x0F | 0xF0 = 0xFF" <|
            \_ ->
                let
                    zm =
                        makeZM [ 0x08, 0x0F, 0xF0, 0x00 ]

                    result =
                        Execute.step zm
                in
                case result of
                    Continue m ->
                        m.stack |> Expect.equal [ 0xFF ]

                    _ ->
                        Expect.fail "Expected Continue"
        , test "and: 0xFF & 0x0F = 0x0F" <|
            \_ ->
                let
                    zm =
                        makeZM [ 0x09, 0xFF, 0x0F, 0x00 ]

                    result =
                        Execute.step zm
                in
                case result of
                    Continue m ->
                        m.stack |> Expect.equal [ 0x0F ]

                    _ ->
                        Expect.fail "Expected Continue"
        , test "not: ~0x00FF = 0xFF00" <|
            \_ ->
                let
                    -- Short form 1OP not: 0x8F, large constant 0x00FF, store to stack
                    zm =
                        makeZM [ 0x8F, 0x00, 0xFF, 0x00 ]

                    result =
                        Execute.step zm
                in
                case result of
                    Continue m ->
                        m.stack |> Expect.equal [ 0xFF00 ]

                    _ ->
                        Expect.fail "Expected Continue"
        ]



-- BRANCH TESTS


branchTests : Test
branchTests =
    describe "branch opcodes"
        [ test "jz branches when operand is 0" <|
            \_ ->
                let
                    -- jz short form: 0x90, small constant 0, branch byte 0xC5 (true, +5)
                    zm =
                        makeZM [ 0x90, 0x00, 0xC5 ]

                    result =
                        Execute.step zm
                in
                case result of
                    Continue m ->
                        -- PC should be: (0x40 + 3) + 5 - 2 = 0x46
                        m.pc |> Expect.equal 0x46

                    _ ->
                        Expect.fail "Expected Continue"
        , test "jz does not branch when operand is nonzero" <|
            \_ ->
                let
                    zm =
                        makeZM [ 0x90, 0x01, 0xC5 ]

                    result =
                        Execute.step zm
                in
                case result of
                    Continue m ->
                        -- PC should advance past the instruction: 0x40 + 3 = 0x43
                        m.pc |> Expect.equal 0x43

                    _ ->
                        Expect.fail "Expected Continue"
        , test "je branches when equal" <|
            \_ ->
                let
                    -- long form je: 0x01, small 5, small 5, branch 0xC5
                    zm =
                        makeZM [ 0x01, 0x05, 0x05, 0xC5 ]

                    result =
                        Execute.step zm
                in
                case result of
                    Continue m ->
                        m.pc |> Expect.equal 0x47

                    _ ->
                        Expect.fail "Expected Continue"
        , test "je does not branch when not equal" <|
            \_ ->
                let
                    zm =
                        makeZM [ 0x01, 0x05, 0x06, 0xC5 ]

                    result =
                        Execute.step zm
                in
                case result of
                    Continue m ->
                        m.pc |> Expect.equal 0x44

                    _ ->
                        Expect.fail "Expected Continue"
        , test "jl branches when less" <|
            \_ ->
                let
                    zm =
                        makeZM [ 0x02, 0x03, 0x05, 0xC5 ]

                    result =
                        Execute.step zm
                in
                case result of
                    Continue m ->
                        m.pc |> Expect.equal 0x47

                    _ ->
                        Expect.fail "Expected Continue"
        , test "jg branches when greater" <|
            \_ ->
                let
                    zm =
                        makeZM [ 0x03, 0x0A, 0x05, 0xC5 ]

                    result =
                        Execute.step zm
                in
                case result of
                    Continue m ->
                        m.pc |> Expect.equal 0x47

                    _ ->
                        Expect.fail "Expected Continue"
        , test "branch with condition=false inverts logic" <|
            \_ ->
                let
                    -- jz with operand 1, branch byte 0x45 (condition=false, single, offset 5)
                    -- Since operand != 0, jz condition is false, and branch.condition is also false
                    -- false == false → branch taken
                    zm =
                        makeZM [ 0x90, 0x01, 0x45 ]

                    result =
                        Execute.step zm
                in
                case result of
                    Continue m ->
                        m.pc |> Expect.equal 0x46

                    _ ->
                        Expect.fail "Expected Continue"
        , test "branch to ReturnTrue" <|
            \_ ->
                let
                    -- jz 0, branch byte 0xC1 (true, single, offset 1 = ReturnTrue)
                    -- Need a call frame to return from
                    zm =
                        makeZM [ 0x90, 0x00, 0xC1 ]
                            |> pushFrame
                in
                case Execute.step zm of
                    Continue m ->
                        -- Should have returned 1, stack should have return value
                        m.stack |> List.head |> Expect.equal (Just 1)

                    _ ->
                        Expect.fail "Expected Continue with return"
        ]



-- VARIABLE TESTS


variableTests : Test
variableTests =
    describe "variable opcodes"
        [ test "push and pull via stack" <|
            \_ ->
                let
                    -- push 42: 0xE8 (VAR push), types 0x7F (small, omit*3), value 42
                    zm =
                        makeZM [ 0xE8, 0x7F, 0x2A ]

                    result =
                        Execute.step zm
                in
                case result of
                    Continue m ->
                        m.stack |> Expect.equal [ 42 ]

                    _ ->
                        Expect.fail "Expected Continue"
        , test "store writes to global variable" <|
            \_ ->
                let
                    -- 2OP store: 0x0D, var ref 0x10 (global 0), value 99
                    zm =
                        makeZM [ 0x0D, 0x10, 0x63 ]

                    result =
                        Execute.step zm
                in
                case result of
                    Continue m ->
                        Memory.readWord 0x80 m.memory |> Expect.equal 99

                    _ ->
                        Expect.fail "Expected Continue"
        , test "inc increments global variable" <|
            \_ ->
                let
                    -- 1OP inc: 0x95 (short, small, opcode 5), var ref 0x10 (global 0)
                    zm =
                        makeZMWithGlobal 0x10 10 [ 0x95, 0x10 ]

                    result =
                        Execute.step zm
                in
                case result of
                    Continue m ->
                        Memory.readWord 0x80 m.memory |> Expect.equal 11

                    _ ->
                        Expect.fail "Expected Continue"
        , test "dec decrements global variable" <|
            \_ ->
                let
                    zm =
                        makeZMWithGlobal 0x10 10 [ 0x96, 0x10 ]

                    result =
                        Execute.step zm
                in
                case result of
                    Continue m ->
                        Memory.readWord 0x80 m.memory |> Expect.equal 9

                    _ ->
                        Expect.fail "Expected Continue"
        ]



-- CONTROL FLOW TESTS


controlFlowTests : Test
controlFlowTests =
    describe "control flow"
        [ test "rtrue returns 1" <|
            \_ ->
                let
                    zm =
                        makeZM [ 0xB0 ] |> pushFrame

                    result =
                        Execute.step zm
                in
                case result of
                    Continue m ->
                        m.stack |> List.head |> Expect.equal (Just 1)

                    _ ->
                        Expect.fail "Expected Continue"
        , test "rfalse returns 0" <|
            \_ ->
                let
                    zm =
                        makeZM [ 0xB1 ] |> pushFrame

                    result =
                        Execute.step zm
                in
                case result of
                    Continue m ->
                        m.stack |> List.head |> Expect.equal (Just 0)

                    _ ->
                        Expect.fail "Expected Continue"
        , test "jump moves PC by signed offset" <|
            \_ ->
                let
                    -- 1OP jump: 0x8C (short, large, opcode 12), offset 0x000A = +10
                    zm =
                        makeZM [ 0x8C, 0x00, 0x0A ]

                    result =
                        Execute.step zm
                in
                case result of
                    Continue m ->
                        -- nextPC = 0x40+3 = 0x43, then +10-2 = 0x4B
                        m.pc |> Expect.equal 0x4B

                    _ ->
                        Expect.fail "Expected Continue"
        , test "quit halts" <|
            \_ ->
                let
                    zm =
                        makeZM [ 0xBA ]

                    result =
                        Execute.step zm
                in
                case result of
                    Halted _ ->
                        Expect.pass

                    _ ->
                        Expect.fail "Expected Halted"
        , test "nop advances PC" <|
            \_ ->
                let
                    zm =
                        makeZM [ 0xB4 ]

                    result =
                        Execute.step zm
                in
                case result of
                    Continue m ->
                        m.pc |> Expect.equal 0x41

                    _ ->
                        Expect.fail "Expected Continue"
        , test "ret_popped pops stack and returns" <|
            \_ ->
                let
                    zm =
                        makeZM [ 0xB8 ]
                            |> pushFrame
                            |> (\m -> { m | stack = [ 42 ] })

                    result =
                        Execute.step zm
                in
                case result of
                    Continue m ->
                        m.stack |> List.head |> Expect.equal (Just 42)

                    _ ->
                        Expect.fail "Expected Continue"
        , test "pop discards top of stack" <|
            \_ ->
                let
                    zm =
                        makeZM [ 0xB9 ]
                            |> (\m -> { m | stack = [ 99, 42 ] })

                    result =
                        Execute.step zm
                in
                case result of
                    Continue m ->
                        m.stack |> Expect.equal [ 42 ]

                    _ ->
                        Expect.fail "Expected Continue"
        ]



-- MEMORY ACCESS TESTS


memoryAccessTests : Test
memoryAccessTests =
    describe "memory access opcodes"
        [ test "loadw reads word from array" <|
            \_ ->
                let
                    -- Write 0xABCD at address 0x70
                    zm =
                        makeZM [ 0x0F, 0x70, 0x00, 0x00 ]
                            |> (\m -> { m | memory = Memory.writeWord 0x70 0xABCD m.memory })

                    result =
                        Execute.step zm
                in
                case result of
                    Continue m ->
                        m.stack |> Expect.equal [ 0xABCD ]

                    _ ->
                        Expect.fail "Expected Continue"
        , test "loadb reads byte from array" <|
            \_ ->
                let
                    zm =
                        makeZM [ 0x10, 0x70, 0x00, 0x00 ]
                            |> (\m -> { m | memory = Memory.writeByte 0x70 0xEF m.memory })

                    result =
                        Execute.step zm
                in
                case result of
                    Continue m ->
                        m.stack |> Expect.equal [ 0xEF ]

                    _ ->
                        Expect.fail "Expected Continue"
        , test "storew writes word to array" <|
            \_ ->
                let
                    -- VAR storew: 0xE1, types 0x15 (small,small,small,omit=01 01 01 11? No)
                    -- Actually: 0x15 = 00 01 01 01 = large, small, small, small
                    -- We want: array=0x70, index=0, value=0xBEEF
                    -- types: 01 01 00 11 = small, small, large, omit = 0x53
                    zm =
                        makeZM [ 0xE1, 0x53, 0x70, 0x00, 0xBE, 0xEF ]

                    result =
                        Execute.step zm
                in
                case result of
                    Continue m ->
                        Memory.readWord 0x70 m.memory |> Expect.equal 0xBEEF

                    _ ->
                        Expect.fail "Expected Continue"
        , test "storeb writes byte to array" <|
            \_ ->
                let
                    -- VAR storeb: 0xE2, types 0x57 (small, small, small, omit)
                    zm =
                        makeZM [ 0xE2, 0x57, 0x70, 0x00, 0xAB ]

                    result =
                        Execute.step zm
                in
                case result of
                    Continue m ->
                        Memory.readByte 0x70 m.memory |> Expect.equal 0xAB

                    _ ->
                        Expect.fail "Expected Continue"
        ]



-- I/O TESTS


ioTests : Test
ioTests =
    describe "I/O opcodes"
        [ test "print_num outputs signed number" <|
            \_ ->
                let
                    -- VAR print_num: 0xE6, types 0x7F (small, omit*3), value 42
                    zm =
                        makeZM [ 0xE6, 0x7F, 0x2A ]

                    result =
                        Execute.step zm
                in
                case result of
                    Continue m ->
                        getOutputText m |> Expect.equal "42"

                    _ ->
                        Expect.fail "Expected Continue"
        , test "print_num with negative number" <|
            \_ ->
                let
                    -- -1 = 0xFFFF as large constant
                    -- VAR print_num: 0xE6, types 0x3F (large, omit*3)
                    zm =
                        makeZM [ 0xE6, 0x3F, 0xFF, 0xFF ]

                    result =
                        Execute.step zm
                in
                case result of
                    Continue m ->
                        getOutputText m |> Expect.equal "-1"

                    _ ->
                        Expect.fail "Expected Continue"
        , test "print_char outputs ZSCII character" <|
            \_ ->
                let
                    -- VAR print_char: 0xE5, types 0x3F (large, omit*3), value 65 = 'A'
                    zm =
                        makeZM [ 0xE5, 0x3F, 0x00, 0x41 ]

                    result =
                        Execute.step zm
                in
                case result of
                    Continue m ->
                        getOutputText m |> Expect.equal "A"

                    _ ->
                        Expect.fail "Expected Continue"
        , test "new_line outputs newline" <|
            \_ ->
                let
                    zm =
                        makeZM [ 0xBB ]

                    result =
                        Execute.step zm
                in
                case result of
                    Continue m ->
                        getOutputText m |> Expect.equal "\n"

                    _ ->
                        Expect.fail "Expected Continue"
        ]



-- STACK TESTS


stackTests : Test
stackTests =
    describe "stack operations"
        [ test "push then pop roundtrip" <|
            \_ ->
                let
                    -- Push 99, then pop (two instructions)
                    zm =
                        makeZM [ 0xE8, 0x7F, 0x63 ]

                    result =
                        Execute.step zm
                in
                case result of
                    Continue m ->
                        m.stack |> Expect.equal [ 99 ]

                    _ ->
                        Expect.fail "Expected Continue"
        , test "multiple pushes maintain order" <|
            \_ ->
                let
                    zm =
                        makeZM [ 0xE8, 0x7F, 0x01 ]
                            |> (\m -> { m | stack = [ 2, 3 ] })

                    result =
                        Execute.step zm
                in
                case result of
                    Continue m ->
                        m.stack |> Expect.equal [ 1, 2, 3 ]

                    _ ->
                        Expect.fail "Expected Continue"
        ]



-- HELPER: Push a call frame so returns work


callTests : Test
callTests =
    describe "call convention"
        [ test "V3 call reads initial local values from routine header" <|
            \_ ->
                let
                    -- Routine at packed address 0x60 -> byte address 0xC0
                    -- Routine header: 2 locals, initial values 0x000A, 0x0014
                    -- Then rtrue (0xB0)
                    -- call_vs 0x60 -> store to stack
                    -- VAR call: 0xE0 0x00 (opcode + 1-arg types), 0x00 0x60 (packed addr), 0x00 (store to stack)
                    zm =
                        makeZMAt 0x40
                            ([ 0xE0, 0x3F, 0x00, 0x60, 0x00 ]
                                -- padding to fill up to 0xC0
                                ++ List.repeat (0xC0 - 0x40 - 5) 0
                                -- routine at 0xC0: 2 locals, values 10 and 20, then ret_popped
                                ++ [ 0x02, 0x00, 0x0A, 0x00, 0x14, 0xB8 ]
                            )
                            |> pushFrame

                    result =
                        Execute.step zm
                in
                case result of
                    Continue m ->
                        -- PC should be at the rtrue (0xC0 + 1 + 2*2 = 0xC5)
                        m.pc |> Expect.equal 0xC5

                    _ ->
                        Expect.fail "Expected Continue"
        , test "V5 call zeros locals and skips only count byte" <|
            \_ ->
                let
                    -- Routine at packed address 0x30 -> byte address 0xC0 (V5: 0x30 * 4)
                    -- Routine header: 2 locals (no initial values)
                    -- Then rtrue (0xB0)
                    zm =
                        makeZMv5At 0x40
                            ([ 0xE0, 0x3F, 0x00, 0x30, 0x00 ]
                                ++ List.repeat (0xC0 - 0x40 - 5) 0
                                -- routine at 0xC0: 2 locals, then ret_popped
                                ++ [ 0x02, 0xB8 ]
                            )
                            |> pushFrame

                    result =
                        Execute.step zm
                in
                case result of
                    Continue m ->
                        -- PC should be at ret_popped (0xC0 + 1 = 0xC1)
                        m.pc |> Expect.equal 0xC1

                    _ ->
                        Expect.fail "Expected Continue"
        , test "V5 call locals are zero-initialised" <|
            \_ ->
                let
                    -- Routine at packed address 0x30 -> byte address 0xC0
                    zm =
                        makeZMv5At 0x40
                            ([ 0xE0, 0x3F, 0x00, 0x30, 0x00 ]
                                ++ List.repeat (0xC0 - 0x40 - 5) 0
                                -- routine at 0xC0: 2 locals, then ret_popped
                                ++ [ 0x02, 0xB8 ]
                            )
                            |> pushFrame

                    result =
                        Execute.step zm
                in
                case result of
                    Continue m ->
                        -- Check that the new frame's locals are both 0
                        case m.callStack of
                            frame :: _ ->
                                frame.locals
                                    |> Array.toList
                                    |> Expect.equal [ 0, 0 ]

                            [] ->
                                Expect.fail "Expected call frame on stack"

                    _ ->
                        Expect.fail "Expected Continue"
        ]


pushFrame : ZMachine -> ZMachine
pushFrame zm =
    let
        frame =
            { returnPC = 0x50
            , returnStore = Just Stack
            , locals = Array.empty
            , evalStack = []
            }
    in
    { zm | callStack = frame :: zm.callStack }
