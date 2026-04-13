module ObjectTableTest exposing (suite)

import Bitwise
import Bytes.Encode as Encode
import Expect
import Test exposing (Test, describe, test)
import ZMachine.Memory as Memory exposing (Memory)
import ZMachine.ObjectTable as OT


suite : Test
suite =
    describe "ZMachine.ObjectTable"
        [ v3TreeTests
        , v5TreeTests
        , v3AttributeTests
        , v5AttributeTests
        , v3PropertyTests
        , v5PropertyTests
        ]



-- V3 TREE


v3TreeTests : Test
v3TreeTests =
    describe "V3 tree relations"
        [ test "parent reads 1-byte pointer" <|
            \_ ->
                let
                    mem =
                        makeV3ObjectMem
                            { objNum = 1
                            , parent = 5
                            , sibling = 3
                            , child = 2
                            , propPtr = 0
                            }
                in
                OT.parent 1 mem |> Expect.equal 5
        , test "sibling reads 1-byte pointer" <|
            \_ ->
                let
                    mem =
                        makeV3ObjectMem
                            { objNum = 1
                            , parent = 5
                            , sibling = 3
                            , child = 2
                            , propPtr = 0
                            }
                in
                OT.sibling 1 mem |> Expect.equal 3
        , test "child reads 1-byte pointer" <|
            \_ ->
                let
                    mem =
                        makeV3ObjectMem
                            { objNum = 1
                            , parent = 5
                            , sibling = 3
                            , child = 2
                            , propPtr = 0
                            }
                in
                OT.child 1 mem |> Expect.equal 2
        , test "setParent writes 1-byte pointer" <|
            \_ ->
                let
                    mem =
                        makeV3ObjectMem
                            { objNum = 1
                            , parent = 0
                            , sibling = 0
                            , child = 0
                            , propPtr = 0
                            }
                            |> OT.setParent 1 7
                in
                OT.parent 1 mem |> Expect.equal 7
        , test "object 0 returns 0" <|
            \_ ->
                let
                    mem =
                        makeV3ObjectMem
                            { objNum = 1
                            , parent = 5
                            , sibling = 3
                            , child = 2
                            , propPtr = 0
                            }
                in
                ( OT.parent 0 mem, OT.sibling 0 mem, OT.child 0 mem )
                    |> Expect.equal ( 0, 0, 0 )
        ]



-- V5 TREE


v5TreeTests : Test
v5TreeTests =
    describe "V5 tree relations"
        [ test "parent reads 2-byte pointer" <|
            \_ ->
                let
                    mem =
                        makeV5ObjectMem
                            { objNum = 1
                            , parent = 300
                            , sibling = 150
                            , child = 200
                            , propPtr = 0
                            }
                in
                OT.parent 1 mem |> Expect.equal 300
        , test "sibling reads 2-byte pointer" <|
            \_ ->
                let
                    mem =
                        makeV5ObjectMem
                            { objNum = 1
                            , parent = 300
                            , sibling = 150
                            , child = 200
                            , propPtr = 0
                            }
                in
                OT.sibling 1 mem |> Expect.equal 150
        , test "child reads 2-byte pointer" <|
            \_ ->
                let
                    mem =
                        makeV5ObjectMem
                            { objNum = 1
                            , parent = 300
                            , sibling = 150
                            , child = 200
                            , propPtr = 0
                            }
                in
                OT.child 1 mem |> Expect.equal 200
        , test "setParent writes 2-byte pointer" <|
            \_ ->
                let
                    mem =
                        makeV5ObjectMem
                            { objNum = 1
                            , parent = 0
                            , sibling = 0
                            , child = 0
                            , propPtr = 0
                            }
                            |> OT.setParent 1 500
                in
                OT.parent 1 mem |> Expect.equal 500
        , test "large object number works" <|
            \_ ->
                let
                    mem =
                        makeV5ObjectMem
                            { objNum = 2
                            , parent = 1000
                            , sibling = 0
                            , child = 0
                            , propPtr = 0
                            }
                in
                OT.parent 2 mem |> Expect.equal 1000
        ]



-- V3 ATTRIBUTES


v3AttributeTests : Test
v3AttributeTests =
    describe "V3 attributes"
        [ test "test attribute 0 (highest bit of first byte)" <|
            \_ ->
                let
                    -- Attr 0 = bit 7 of byte 0 of the attribute bytes
                    mem =
                        makeV3ObjectMemWithAttrs 1 [ 0x80, 0, 0, 0 ]
                in
                OT.testAttribute 1 0 mem |> Expect.equal True
        , test "test attribute 31 (lowest bit of last byte)" <|
            \_ ->
                let
                    mem =
                        makeV3ObjectMemWithAttrs 1 [ 0, 0, 0, 0x01 ]
                in
                OT.testAttribute 1 31 mem |> Expect.equal True
        , test "set and clear attribute" <|
            \_ ->
                let
                    mem =
                        makeV3ObjectMemWithAttrs 1 [ 0, 0, 0, 0 ]
                            |> OT.setAttribute 1 15
                in
                ( OT.testAttribute 1 15 mem
                , OT.testAttribute 1 14 mem
                )
                    |> Expect.equal ( True, False )
        ]



-- V5 ATTRIBUTES


v5AttributeTests : Test
v5AttributeTests =
    describe "V5 attributes"
        [ test "test attribute 0" <|
            \_ ->
                let
                    mem =
                        makeV5ObjectMemWithAttrs 1 [ 0x80, 0, 0, 0, 0, 0 ]
                in
                OT.testAttribute 1 0 mem |> Expect.equal True
        , test "test attribute 47 (highest attribute in V5)" <|
            \_ ->
                let
                    -- Attr 47 = bit 0 of byte 5 (6th attribute byte)
                    mem =
                        makeV5ObjectMemWithAttrs 1 [ 0, 0, 0, 0, 0, 0x01 ]
                in
                OT.testAttribute 1 47 mem |> Expect.equal True
        , test "set attribute 40" <|
            \_ ->
                let
                    mem =
                        makeV5ObjectMemWithAttrs 1 [ 0, 0, 0, 0, 0, 0 ]
                            |> OT.setAttribute 1 40
                in
                OT.testAttribute 1 40 mem |> Expect.equal True
        ]



-- V3 PROPERTIES


v3PropertyTests : Test
v3PropertyTests =
    describe "V3 properties"
        [ test "findProperty returns data address and length" <|
            \_ ->
                let
                    mem =
                        makeV3WithProperty
                            { objNum = 1
                            , propNum = 5
                            , dataLen = 2
                            , dataBytes = [ 0xAB, 0xCD ]
                            }
                in
                case OT.findProperty 1 5 mem of
                    Just ( _, len ) ->
                        len |> Expect.equal 2

                    Nothing ->
                        Expect.fail "Property not found"
        , test "propertyLength from data address" <|
            \_ ->
                let
                    mem =
                        makeV3WithProperty
                            { objNum = 1
                            , propNum = 5
                            , dataLen = 2
                            , dataBytes = [ 0xAB, 0xCD ]
                            }
                in
                case OT.findProperty 1 5 mem of
                    Just ( dataAddr, _ ) ->
                        OT.propertyLength dataAddr mem |> Expect.equal 2

                    Nothing ->
                        Expect.fail "Property not found"
        ]



-- V5 PROPERTIES


v5PropertyTests : Test
v5PropertyTests =
    describe "V5 properties"
        [ test "single-byte header with 1-byte data (bit 6 clear)" <|
            \_ ->
                let
                    -- Property header: 0x05 (prop 5, bit 7=0, bit 6=0 -> 1 byte)
                    -- Data: 0xFF
                    mem =
                        makeV5WithProperty
                            { objNum = 1
                            , headerBytes = [ 0x05 ]
                            , dataBytes = [ 0xFF ]
                            }
                in
                case OT.findProperty 1 5 mem of
                    Just ( _, len ) ->
                        len |> Expect.equal 1

                    Nothing ->
                        Expect.fail "Property not found"
        , test "single-byte header with 2-byte data (bit 6 set)" <|
            \_ ->
                let
                    -- Property header: 0x45 (prop 5, bit 7=0, bit 6=1 -> 2 bytes)
                    mem =
                        makeV5WithProperty
                            { objNum = 1
                            , headerBytes = [ 0x45 ]
                            , dataBytes = [ 0xAB, 0xCD ]
                            }
                in
                case OT.findProperty 1 5 mem of
                    Just ( _, len ) ->
                        len |> Expect.equal 2

                    Nothing ->
                        Expect.fail "Property not found"
        , test "two-byte header with specified length" <|
            \_ ->
                let
                    -- First byte: 0x85 (prop 5, bit 7=1 -> two-byte header)
                    -- Second byte: 0x84 (bit 7=1, length=4)
                    mem =
                        makeV5WithProperty
                            { objNum = 1
                            , headerBytes = [ 0x85, 0x84 ]
                            , dataBytes = [ 1, 2, 3, 4 ]
                            }
                in
                case OT.findProperty 1 5 mem of
                    Just ( _, len ) ->
                        len |> Expect.equal 4

                    Nothing ->
                        Expect.fail "Property not found"
        , test "two-byte header with length 0 means 64" <|
            \_ ->
                let
                    -- First byte: 0x85 (prop 5, bit 7=1)
                    -- Second byte: 0x80 (bit 7=1, length=0 -> means 64)
                    mem =
                        makeV5WithPropertyLarge
                            { objNum = 1
                            , headerBytes = [ 0x85, 0x80 ]
                            , dataBytes = List.repeat 64 0
                            }
                in
                case OT.findProperty 1 5 mem of
                    Just ( _, len ) ->
                        len |> Expect.equal 64

                    Nothing ->
                        Expect.fail "Property not found"
        , test "propertyLength works with two-byte header" <|
            \_ ->
                let
                    mem =
                        makeV5WithProperty
                            { objNum = 1
                            , headerBytes = [ 0x85, 0x83 ]
                            , dataBytes = [ 1, 2, 3 ]
                            }
                in
                case OT.findProperty 1 5 mem of
                    Just ( dataAddr, _ ) ->
                        OT.propertyLength dataAddr mem |> Expect.equal 3

                    Nothing ->
                        Expect.fail "Property not found"
        ]



-- HELPERS


{-| Object table address in our test layout.
-}
objTableAddr : Int
objTableAddr =
    0x40


{-| Build V3 memory with one object at objNum having given relations.
Object table at 0x40. Property table at 0x00C0 (empty: name len 0, terminator 0).
-}
makeV3ObjectMem :
    { objNum : Int, parent : Int, sibling : Int, child : Int, propPtr : Int }
    -> Memory
makeV3ObjectMem obj =
    makeV3ObjectMemWithAttrs obj.objNum [ 0, 0, 0, 0 ]
        |> OT.setParent obj.objNum obj.parent
        |> OT.setSibling obj.objNum obj.sibling
        |> OT.setChild obj.objNum obj.child


makeV3ObjectMemWithAttrs : Int -> List Int -> Memory
makeV3ObjectMemWithAttrs objNum attrs =
    let
        totalSize =
            512

        staticBase =
            0x0100

        propTableAddr =
            0x00C0

        -- Object entry address: objTableAddr + 31*2 + (objNum-1)*9
        entryAddr =
            objTableAddr + 62 + (objNum - 1) * 9

        base =
            List.repeat totalSize 0
                |> setAt 0 3
                |> setAt 0x0A (Bitwise.shiftRightZfBy 8 objTableAddr)
                |> setAt 0x0B (Bitwise.and objTableAddr 0xFF)
                |> setAt 0x0E (Bitwise.shiftRightZfBy 8 staticBase)
                |> setAt 0x0F (Bitwise.and staticBase 0xFF)
                -- Attribute bytes
                |> setAttrs entryAddr attrs
                -- Property pointer (2 bytes at entryAddr + 7)
                |> setAt (entryAddr + 7) (Bitwise.shiftRightZfBy 8 propTableAddr)
                |> setAt (entryAddr + 8) (Bitwise.and propTableAddr 0xFF)
                -- Property table: name length 0, terminator 0
                |> setAt propTableAddr 0
                |> setAt (propTableAddr + 1) 0
    in
    base |> toMemory


{-| Build V5 memory with one object having given relations.
-}
makeV5ObjectMem :
    { objNum : Int, parent : Int, sibling : Int, child : Int, propPtr : Int }
    -> Memory
makeV5ObjectMem obj =
    makeV5ObjectMemWithAttrs obj.objNum [ 0, 0, 0, 0, 0, 0 ]
        |> OT.setParent obj.objNum obj.parent
        |> OT.setSibling obj.objNum obj.sibling
        |> OT.setChild obj.objNum obj.child


makeV5ObjectMemWithAttrs : Int -> List Int -> Memory
makeV5ObjectMemWithAttrs objNum attrs =
    let
        totalSize =
            512

        staticBase =
            0x0100

        -- V5 entries start at objTableAddr + 126 and are 14 bytes each,
        -- so place the property table well past the object entries.
        propTableAddr =
            0x00E0

        -- Object entry address: objTableAddr + 63*2 + (objNum-1)*14
        entryAddr =
            objTableAddr + 126 + (objNum - 1) * 14

        base =
            List.repeat totalSize 0
                |> setAt 0 5
                |> setAt 0x0A (Bitwise.shiftRightZfBy 8 objTableAddr)
                |> setAt 0x0B (Bitwise.and objTableAddr 0xFF)
                |> setAt 0x0E (Bitwise.shiftRightZfBy 8 staticBase)
                |> setAt 0x0F (Bitwise.and staticBase 0xFF)
                -- Attribute bytes
                |> setAttrs entryAddr attrs
                -- Property pointer (2 bytes at entryAddr + 12)
                |> setAt (entryAddr + 12) (Bitwise.shiftRightZfBy 8 propTableAddr)
                |> setAt (entryAddr + 13) (Bitwise.and propTableAddr 0xFF)
                -- Property table: name length 0, terminator 0
                |> setAt propTableAddr 0
                |> setAt (propTableAddr + 1) 0
    in
    base |> toMemory


{-| Build V3 memory with one object that has a single property.
-}
makeV3WithProperty :
    { objNum : Int, propNum : Int, dataLen : Int, dataBytes : List Int }
    -> Memory
makeV3WithProperty { objNum, propNum, dataLen, dataBytes } =
    let
        totalSize =
            512

        staticBase =
            0x0100

        propTableAddr =
            0x00C0

        entryAddr =
            objTableAddr + 62 + (objNum - 1) * 9

        -- V3 size byte: (dataLen - 1) << 5 | propNum
        sizeByte =
            Bitwise.or (Bitwise.shiftLeftBy 5 (dataLen - 1)) propNum

        -- Property table: name len (1 byte) = 0, then property, then terminator
        propBytes =
            0 :: sizeByte :: dataBytes ++ [ 0 ]

        base =
            List.repeat totalSize 0
                |> setAt 0 3
                |> setAt 0x0A (Bitwise.shiftRightZfBy 8 objTableAddr)
                |> setAt 0x0B (Bitwise.and objTableAddr 0xFF)
                |> setAt 0x0E (Bitwise.shiftRightZfBy 8 staticBase)
                |> setAt 0x0F (Bitwise.and staticBase 0xFF)
                -- Property pointer
                |> setAt (entryAddr + 7) (Bitwise.shiftRightZfBy 8 propTableAddr)
                |> setAt (entryAddr + 8) (Bitwise.and propTableAddr 0xFF)
                -- Write property table bytes
                |> setBytesAt propTableAddr propBytes
    in
    base |> toMemory


{-| Build V5 memory with one object that has a single property.
headerBytes are the raw property header byte(s), dataBytes the data.
-}
makeV5WithProperty :
    { objNum : Int, headerBytes : List Int, dataBytes : List Int }
    -> Memory
makeV5WithProperty { objNum, headerBytes, dataBytes } =
    let
        totalSize =
            512

        staticBase =
            0x0100

        propTableAddr =
            0x00E0

        entryAddr =
            objTableAddr + 126 + (objNum - 1) * 14

        -- Property table: name len (1 byte) = 0, then property, then terminator
        propBytes =
            0 :: headerBytes ++ dataBytes ++ [ 0 ]

        base =
            List.repeat totalSize 0
                |> setAt 0 5
                |> setAt 0x0A (Bitwise.shiftRightZfBy 8 objTableAddr)
                |> setAt 0x0B (Bitwise.and objTableAddr 0xFF)
                |> setAt 0x0E (Bitwise.shiftRightZfBy 8 staticBase)
                |> setAt 0x0F (Bitwise.and staticBase 0xFF)
                -- Property pointer
                |> setAt (entryAddr + 12) (Bitwise.shiftRightZfBy 8 propTableAddr)
                |> setAt (entryAddr + 13) (Bitwise.and propTableAddr 0xFF)
                -- Write property table bytes
                |> setBytesAt propTableAddr propBytes
    in
    base |> toMemory


{-| Like makeV5WithProperty but with larger memory for big property data.
-}
makeV5WithPropertyLarge :
    { objNum : Int, headerBytes : List Int, dataBytes : List Int }
    -> Memory
makeV5WithPropertyLarge { objNum, headerBytes, dataBytes } =
    let
        totalSize =
            1024

        staticBase =
            0x0200

        propTableAddr =
            0x00E0

        entryAddr =
            objTableAddr + 126 + (objNum - 1) * 14

        propBytes =
            0 :: headerBytes ++ dataBytes ++ [ 0 ]

        base =
            List.repeat totalSize 0
                |> setAt 0 5
                |> setAt 0x0A (Bitwise.shiftRightZfBy 8 objTableAddr)
                |> setAt 0x0B (Bitwise.and objTableAddr 0xFF)
                |> setAt 0x0E (Bitwise.shiftRightZfBy 8 staticBase)
                |> setAt 0x0F (Bitwise.and staticBase 0xFF)
                |> setAt (entryAddr + 12) (Bitwise.shiftRightZfBy 8 propTableAddr)
                |> setAt (entryAddr + 13) (Bitwise.and propTableAddr 0xFF)
                |> setBytesAt propTableAddr propBytes
    in
    base |> toMemory


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


setAttrs : Int -> List Int -> List Int -> List Int
setAttrs baseAddr attrs mem =
    List.foldl
        (\( i, b ) acc -> setAt (baseAddr + i) b acc)
        mem
        (List.indexedMap Tuple.pair attrs)


setBytesAt : Int -> List Int -> List Int -> List Int
setBytesAt baseAddr bytes mem =
    List.foldl
        (\( i, b ) acc -> setAt (baseAddr + i) b acc)
        mem
        (List.indexedMap Tuple.pair bytes)


toMemory : List Int -> Memory
toMemory bytes =
    bytes
        |> List.map Encode.unsignedInt8
        |> Encode.sequence
        |> Encode.encode
        |> Memory.fromBytes
        |> unwrap


unwrap : Result String a -> a
unwrap result =
    case result of
        Ok v ->
            v

        Err msg ->
            Debug.todo msg
