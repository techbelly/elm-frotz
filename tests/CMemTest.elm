module CMemTest exposing (suite)

import Array
import Expect
import Library.CMem as CMem
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Library.CMem"
        [ compressTests
        , decompressTests
        , roundTripTests
        ]


compressTests : Test
compressTests =
    describe "compress"
        [ test "identical arrays produce empty output" <|
            \_ ->
                let
                    orig =
                        Array.fromList [ 1, 2, 3, 4, 5 ]
                in
                CMem.compress { original = orig, current = orig }
                    |> Expect.equal []
        , test "single changed byte produces one XOR byte" <|
            \_ ->
                let
                    orig =
                        Array.fromList [ 0, 0, 0 ]

                    cur =
                        Array.fromList [ 0, 0x42, 0 ]
                in
                -- XOR: [0, 0x42, 0]. The leading zero becomes 0x00 0x00
                -- (run of 1 zero = 0x00 followed by count 0), then 0x42,
                -- trailing zero is omitted.
                CMem.compress { original = orig, current = cur }
                    |> Expect.equal [ 0x00, 0x00, 0x42 ]
        , test "changed byte at position 0 has no leading zero run" <|
            \_ ->
                let
                    orig =
                        Array.fromList [ 0x10, 0, 0 ]

                    cur =
                        Array.fromList [ 0xFF, 0, 0 ]
                in
                -- XOR at 0: 0x10 XOR 0xFF = 0xEF, rest are zeros (omitted).
                CMem.compress { original = orig, current = cur }
                    |> Expect.equal [ 0xEF ]
        , test "long zero run is split at 256 boundaries" <|
            \_ ->
                let
                    orig =
                        Array.repeat 260 0

                    cur =
                        Array.repeat 260 0
                            |> Array.set 259 0x01
                in
                -- 259 leading zeros: first 256 → [0x00, 0xFF], next 3 → [0x00, 0x02], then 0x01.
                CMem.compress { original = orig, current = cur }
                    |> Expect.equal [ 0x00, 0xFF, 0x00, 0x02, 0x01 ]
        ]


decompressTests : Test
decompressTests =
    describe "decompress"
        [ test "empty compressed data returns original unchanged" <|
            \_ ->
                let
                    orig =
                        Array.fromList [ 1, 2, 3 ]
                in
                CMem.decompress { compressed = [], original = orig }
                    |> Expect.equal (Ok orig)
        , test "overflow produces an error" <|
            \_ ->
                let
                    orig =
                        Array.fromList [ 0, 0 ]
                in
                case CMem.decompress { compressed = [ 1, 2, 3 ], original = orig } of
                    Err _ ->
                        Expect.pass

                    Ok _ ->
                        Expect.fail "expected overflow error"
        ]


roundTripTests : Test
roundTripTests =
    describe "compress → decompress round-trip"
        [ test "single mutation round-trips" <|
            \_ ->
                let
                    orig =
                        Array.fromList (List.range 0 255)

                    cur =
                        Array.set 100 0xFF orig

                    compressed =
                        CMem.compress { original = orig, current = cur }
                in
                CMem.decompress { compressed = compressed, original = orig }
                    |> Expect.equal (Ok cur)
        , test "multiple scattered mutations round-trip" <|
            \_ ->
                let
                    orig =
                        Array.repeat 512 0

                    cur =
                        orig
                            |> Array.set 0 0xAA
                            |> Array.set 100 0xBB
                            |> Array.set 200 0xCC
                            |> Array.set 511 0xDD

                    compressed =
                        CMem.compress { original = orig, current = cur }
                in
                CMem.decompress { compressed = compressed, original = orig }
                    |> Expect.equal (Ok cur)
        , test "fully changed memory round-trips" <|
            \_ ->
                let
                    orig =
                        Array.repeat 100 0

                    cur =
                        Array.fromList (List.range 1 100)

                    compressed =
                        CMem.compress { original = orig, current = cur }
                in
                CMem.decompress { compressed = compressed, original = orig }
                    |> Expect.equal (Ok cur)
        ]
