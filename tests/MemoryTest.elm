module MemoryTest exposing (suite)

import Bitwise
import Bytes exposing (Bytes)
import Bytes.Encode as Encode
import Expect
import Test exposing (Test, describe, test)
import ZMachine.Memory as Memory exposing (Memory, Version(..))


suite : Test
suite =
    describe "ZMachine.Memory"
        [ fromBytesTests
        , versionProfileTests
        , readByteTests
        , readWordTests
        , readSignedWordTests
        , writeByteTests
        , writeWordTests
        , readSliceTests
        , sizeTests
        , unpackAddressTests
        , boundaryTests
        ]



-- TEST HELPERS


{-| Build a minimal valid V3 story file of the given total size.
Static memory base is placed at `staticBase`.
The rest is zero-filled except the header fields we need.
-}
makeStoryFile : Int -> Int -> Bytes
makeStoryFile totalSize staticBase =
    let
        -- Build 64-byte header + remaining bytes
        headerBytes =
            List.concat
                [ [ 3 ]

                -- Byte 0: version = 3
                , List.repeat 13 0

                -- Bytes 1-13: padding
                , [ Bitwise.shiftRightZfBy 8 staticBase
                  , Bitwise.and staticBase 0xFF
                  ]

                -- Bytes 14-15: static memory base (big-endian)
                , List.repeat (totalSize - 16) 0

                -- Rest: zero-filled
                ]

        encoders =
            headerBytes
                |> List.take totalSize
                |> List.map Encode.unsignedInt8
    in
    Encode.encode (Encode.sequence encoders)


{-| Build a story file with specific byte values at given addresses.
Total size is 256 bytes, static base at 128.
-}
makeStoryWithData : List ( Int, Int ) -> Bytes
makeStoryWithData patches =
    let
        baseBytes =
            List.repeat 256 0

        -- Start with header: version=3 at byte 0, static base=128 at bytes 14-15
        headerPatched =
            baseBytes
                |> setAt 0 3
                |> setAt 14 0
                |> setAt 15 128

        fullyPatched =
            List.foldl (\( addr, val ) bytes -> setAt addr val bytes) headerPatched patches

        encoders =
            fullyPatched |> List.map Encode.unsignedInt8
    in
    Encode.encode (Encode.sequence encoders)


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


loadOrFail : Bytes -> Memory
loadOrFail raw =
    case Memory.fromBytes raw of
        Ok mem ->
            mem

        Err msg ->
            -- This will cause test failures with a useful message
            Debug.todo ("Failed to load story file: " ++ msg)



-- TESTS


fromBytesTests : Test
fromBytesTests =
    describe "fromBytes"
        [ test "rejects files smaller than 64 bytes" <|
            \_ ->
                let
                    tiny =
                        Encode.encode (Encode.sequence (List.repeat 32 (Encode.unsignedInt8 0)))
                in
                Memory.fromBytes tiny
                    |> Expect.err
        , test "rejects unsupported versions" <|
            \_ ->
                let
                    v4 =
                        makeStoryFileWithVersion 4 256 128
                in
                Memory.fromBytes v4
                    |> Expect.err
        , test "rejects version 6" <|
            \_ ->
                Memory.fromBytes (makeStoryFileWithVersion 6 256 128)
                    |> Expect.err
        , test "accepts valid V3 file" <|
            \_ ->
                Memory.fromBytes (makeStoryFile 256 128)
                    |> Expect.ok
        , test "accepts valid V5 file" <|
            \_ ->
                Memory.fromBytes (makeStoryFileWithVersion 5 256 128)
                    |> Expect.ok
        , test "rejects static base below 64" <|
            \_ ->
                Memory.fromBytes (makeStoryFile 256 32)
                    |> Expect.err
        , test "rejects static base beyond file length" <|
            \_ ->
                Memory.fromBytes (makeStoryFile 128 256)
                    |> Expect.err
        ]


versionProfileTests : Test
versionProfileTests =
    describe "profile"
        [ test "V3 profile has correct version" <|
            \_ ->
                let
                    mem =
                        loadOrFail (makeStoryFile 256 128)
                in
                (Memory.profile mem).version
                    |> Expect.equal V3
        , test "V5 profile has correct version" <|
            \_ ->
                let
                    mem =
                        loadOrFail (makeStoryFileWithVersion 5 256 128)
                in
                (Memory.profile mem).version
                    |> Expect.equal V5
        , test "V3 packing shift is 1" <|
            \_ ->
                let
                    mem =
                        loadOrFail (makeStoryFile 256 128)
                in
                (Memory.profile mem).packingShift
                    |> Expect.equal 1
        , test "V5 packing shift is 2" <|
            \_ ->
                let
                    mem =
                        loadOrFail (makeStoryFileWithVersion 5 256 128)
                in
                (Memory.profile mem).packingShift
                    |> Expect.equal 2
        , test "V3 object entry size is 9" <|
            \_ ->
                (Memory.profile (loadOrFail (makeStoryFile 256 128))).objectEntrySize
                    |> Expect.equal 9
        , test "V5 object entry size is 14" <|
            \_ ->
                (Memory.profile (loadOrFail (makeStoryFileWithVersion 5 256 128))).objectEntrySize
                    |> Expect.equal 14
        , test "V3 has status line" <|
            \_ ->
                (Memory.profile (loadOrFail (makeStoryFile 256 128))).hasStatusLine
                    |> Expect.equal True
        , test "V5 has no status line" <|
            \_ ->
                (Memory.profile (loadOrFail (makeStoryFileWithVersion 5 256 128))).hasStatusLine
                    |> Expect.equal False
        ]


readByteTests : Test
readByteTests =
    describe "readByte"
        [ test "reads version byte at address 0" <|
            \_ ->
                let
                    mem =
                        loadOrFail (makeStoryFile 256 128)
                in
                Memory.readByte 0 mem
                    |> Expect.equal 3
        , test "reads byte from dynamic memory" <|
            \_ ->
                let
                    mem =
                        loadOrFail (makeStoryWithData [ ( 64, 0xAB ) ])
                in
                Memory.readByte 64 mem
                    |> Expect.equal 0xAB
        , test "reads byte from static memory" <|
            \_ ->
                let
                    mem =
                        loadOrFail (makeStoryWithData [ ( 200, 0xCD ) ])
                in
                Memory.readByte 200 mem
                    |> Expect.equal 0xCD
        , test "returns 0 for negative address" <|
            \_ ->
                let
                    mem =
                        loadOrFail (makeStoryFile 256 128)
                in
                Memory.readByte -1 mem
                    |> Expect.equal 0
        , test "returns 0 for address beyond file" <|
            \_ ->
                let
                    mem =
                        loadOrFail (makeStoryFile 256 128)
                in
                Memory.readByte 999 mem
                    |> Expect.equal 0
        ]


readWordTests : Test
readWordTests =
    describe "readWord"
        [ test "reads big-endian word" <|
            \_ ->
                let
                    mem =
                        loadOrFail (makeStoryWithData [ ( 64, 0x12 ), ( 65, 0x34 ) ])
                in
                Memory.readWord 64 mem
                    |> Expect.equal 0x1234
        , test "reads word spanning dynamic/static boundary" <|
            \_ ->
                let
                    -- static base = 128, so byte 127 is last dynamic, 128 is first static
                    mem =
                        loadOrFail (makeStoryWithData [ ( 127, 0xAA ), ( 128, 0xBB ) ])
                in
                Memory.readWord 127 mem
                    |> Expect.equal 0xAABB
        , test "reads static base from header" <|
            \_ ->
                let
                    mem =
                        loadOrFail (makeStoryFile 256 128)
                in
                -- Header bytes 14-15 contain static memory base = 128 = 0x0080
                Memory.readWord 14 mem
                    |> Expect.equal 0x0080
        ]


readSignedWordTests : Test
readSignedWordTests =
    describe "readSignedWord"
        [ test "positive value unchanged" <|
            \_ ->
                let
                    mem =
                        loadOrFail (makeStoryWithData [ ( 64, 0x00 ), ( 65, 0x7F ) ])
                in
                Memory.readSignedWord 64 mem
                    |> Expect.equal 127
        , test "0x8000 is -32768" <|
            \_ ->
                let
                    mem =
                        loadOrFail (makeStoryWithData [ ( 64, 0x80 ), ( 65, 0x00 ) ])
                in
                Memory.readSignedWord 64 mem
                    |> Expect.equal -32768
        , test "0xFFFF is -1" <|
            \_ ->
                let
                    mem =
                        loadOrFail (makeStoryWithData [ ( 64, 0xFF ), ( 65, 0xFF ) ])
                in
                Memory.readSignedWord 64 mem
                    |> Expect.equal -1
        , test "0x7FFF is 32767" <|
            \_ ->
                let
                    mem =
                        loadOrFail (makeStoryWithData [ ( 64, 0x7F ), ( 65, 0xFF ) ])
                in
                Memory.readSignedWord 64 mem
                    |> Expect.equal 32767
        ]


writeByteTests : Test
writeByteTests =
    describe "writeByte"
        [ test "write then read roundtrip" <|
            \_ ->
                let
                    mem =
                        loadOrFail (makeStoryFile 256 128)
                            |> Memory.writeByte 64 0xDE
                in
                Memory.readByte 64 mem
                    |> Expect.equal 0xDE
        , test "write to static memory is ignored" <|
            \_ ->
                let
                    mem =
                        loadOrFail (makeStoryWithData [ ( 200, 0xAA ) ])
                            |> Memory.writeByte 200 0xBB
                in
                Memory.readByte 200 mem
                    |> Expect.equal 0xAA
        , test "write to negative address is ignored" <|
            \_ ->
                let
                    mem =
                        loadOrFail (makeStoryFile 256 128)
                            |> Memory.writeByte -1 0xFF
                in
                -- Should not crash, memory unchanged
                Memory.readByte 0 mem
                    |> Expect.equal 3
        , test "write truncates to 8 bits" <|
            \_ ->
                let
                    mem =
                        loadOrFail (makeStoryFile 256 128)
                            |> Memory.writeByte 64 0x1FF
                in
                Memory.readByte 64 mem
                    |> Expect.equal 0xFF
        , test "multiple writes to different addresses" <|
            \_ ->
                let
                    mem =
                        loadOrFail (makeStoryFile 256 128)
                            |> Memory.writeByte 64 0x11
                            |> Memory.writeByte 65 0x22
                            |> Memory.writeByte 66 0x33
                in
                ( Memory.readByte 64 mem, Memory.readByte 65 mem, Memory.readByte 66 mem )
                    |> Expect.equal ( 0x11, 0x22, 0x33 )
        , test "write does not affect other addresses" <|
            \_ ->
                let
                    mem =
                        loadOrFail (makeStoryWithData [ ( 64, 0xAA ), ( 65, 0xBB ) ])
                            |> Memory.writeByte 64 0xCC
                in
                Memory.readByte 65 mem
                    |> Expect.equal 0xBB
        ]


writeWordTests : Test
writeWordTests =
    describe "writeWord"
        [ test "write then read word roundtrip" <|
            \_ ->
                let
                    mem =
                        loadOrFail (makeStoryFile 256 128)
                            |> Memory.writeWord 64 0xABCD
                in
                Memory.readWord 64 mem
                    |> Expect.equal 0xABCD
        , test "write word stores big-endian" <|
            \_ ->
                let
                    mem =
                        loadOrFail (makeStoryFile 256 128)
                            |> Memory.writeWord 64 0x1234
                in
                ( Memory.readByte 64 mem, Memory.readByte 65 mem )
                    |> Expect.equal ( 0x12, 0x34 )
        , test "write word at boundary of dynamic memory writes only dynamic part" <|
            \_ ->
                let
                    -- static base = 128, writing at 127 means high byte goes to dynamic, low byte to static (ignored)
                    mem =
                        loadOrFail (makeStoryFile 256 128)
                            |> Memory.writeWord 127 0xAABB
                in
                -- High byte (0xAA) written to addr 127 (dynamic)
                -- Low byte (0xBB) write to addr 128 (static, ignored)
                ( Memory.readByte 127 mem, Memory.readByte 128 mem )
                    |> Expect.equal ( 0xAA, 0x00 )
        ]


readSliceTests : Test
readSliceTests =
    describe "readSlice"
        [ test "reads consecutive bytes" <|
            \_ ->
                let
                    mem =
                        loadOrFail (makeStoryWithData [ ( 64, 0x10 ), ( 65, 0x20 ), ( 66, 0x30 ) ])
                in
                Memory.readSlice 64 3 mem
                    |> Expect.equal [ 0x10, 0x20, 0x30 ]
        , test "empty slice returns empty list" <|
            \_ ->
                let
                    mem =
                        loadOrFail (makeStoryFile 256 128)
                in
                Memory.readSlice 64 0 mem
                    |> Expect.equal []
        ]


sizeTests : Test
sizeTests =
    describe "size and dynamicSize"
        [ test "size returns total file length" <|
            \_ ->
                let
                    mem =
                        loadOrFail (makeStoryFile 256 128)
                in
                Memory.size mem
                    |> Expect.equal 256
        , test "dynamicSize returns static base" <|
            \_ ->
                let
                    mem =
                        loadOrFail (makeStoryFile 256 128)
                in
                Memory.dynamicSize mem
                    |> Expect.equal 128
        ]


unpackAddressTests : Test
unpackAddressTests =
    describe "unpackAddress"
        [ test "V3 packed address is multiplied by 2" <|
            \_ ->
                let
                    mem =
                        loadOrFail (makeStoryFile 256 128)
                in
                Memory.unpackAddress 0x1000 mem
                    |> Expect.equal 0x2000
        , test "V5 packed address is multiplied by 4" <|
            \_ ->
                let
                    mem =
                        loadOrFail (makeStoryFileWithVersion 5 256 128)
                in
                Memory.unpackAddress 0x1000 mem
                    |> Expect.equal 0x4000
        , test "zero stays zero" <|
            \_ ->
                let
                    mem =
                        loadOrFail (makeStoryFile 256 128)
                in
                Memory.unpackAddress 0 mem
                    |> Expect.equal 0
        ]


boundaryTests : Test
boundaryTests =
    describe "boundary conditions"
        [ test "read last byte of dynamic memory" <|
            \_ ->
                let
                    mem =
                        loadOrFail (makeStoryWithData [ ( 127, 0xEE ) ])
                in
                Memory.readByte 127 mem
                    |> Expect.equal 0xEE
        , test "read first byte of static memory" <|
            \_ ->
                let
                    mem =
                        loadOrFail (makeStoryWithData [ ( 128, 0xDD ) ])
                in
                Memory.readByte 128 mem
                    |> Expect.equal 0xDD
        , test "write to last dynamic byte works" <|
            \_ ->
                let
                    mem =
                        loadOrFail (makeStoryFile 256 128)
                            |> Memory.writeByte 127 0xFE
                in
                Memory.readByte 127 mem
                    |> Expect.equal 0xFE
        , test "write to first static byte is ignored" <|
            \_ ->
                let
                    mem =
                        loadOrFail (makeStoryFile 256 128)
                            |> Memory.writeByte 128 0xFE
                in
                Memory.readByte 128 mem
                    |> Expect.equal 0x00
        ]



-- ADDITIONAL HELPER


makeStoryFileWithVersion : Int -> Int -> Int -> Bytes
makeStoryFileWithVersion version totalSize staticBase =
    let
        headerBytes =
            List.concat
                [ [ version ]
                , List.repeat 13 0
                , [ Bitwise.shiftRightZfBy 8 staticBase
                  , Bitwise.and staticBase 0xFF
                  ]
                , List.repeat (totalSize - 16) 0
                ]

        encoders =
            headerBytes
                |> List.take totalSize
                |> List.map Encode.unsignedInt8
    in
    Encode.encode (Encode.sequence encoders)
