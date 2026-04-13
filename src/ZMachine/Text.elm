module ZMachine.Text exposing
    ( decodeZString, decodeZStringWords, encodeToZChars
    , zsciiToChar, charToZscii
    , packZCharsToWords
    , readZWords
    )

{-| Z-Machine text encoding and decoding.

Handles the conversion between Z-characters (5-bit values packed 3 per word),
ZSCII character codes, and Unicode strings.

@docs decodeZString, decodeZStringWords, encodeToZChars
@docs zsciiToChar, charToZscii, packZCharsToWords
@docs readZWords

-}

import Bitwise
import Library.ListExtra exposing (findIndex, getAt, padTo)
import ZMachine.Memory as Memory exposing (Memory)
import ZMachine.Header as Header


{-| Decode a Z-encoded string starting at a byte address in memory.
Returns the decoded string and the number of bytes consumed.
-}
decodeZString : Int -> Memory -> ( String, Int )
decodeZString addr mem =
    let
        ( words, endAddr ) =
            readZWords addr mem []

        abbrTableAddr =
            Header.abbreviationsTableAddress mem

        chars =
            decodeWords words abbrTableAddr mem False
    in
    ( String.fromList chars, endAddr - addr )


{-| Decode a Z-encoded string from a list of pre-read 16-bit words.
Used for inline text in print/print\_ret instructions.
-}
decodeZStringWords : List Int -> Memory -> String
decodeZStringWords words mem =
    let
        abbrTableAddr =
            Header.abbreviationsTableAddress mem

        chars =
            decodeWords words abbrTableAddr mem False
    in
    String.fromList chars


{-| Encode a string into Z-characters for dictionary lookup.
Returns exactly 6 Z-characters (V3 dictionary format), padded with the
dictionary pad character.
-}
encodeToZChars : String -> List Int
encodeToZChars str =
    str
        |> String.toLower
        |> String.toList
        |> List.concatMap charToZCharSequence
        |> padTo 6 dictionaryPadChar


{-| Pack Z-characters into 2-byte words for dictionary comparison.
Returns exactly 2 words (V3: 6 Z-chars = 2 words of 3 Z-chars each).
The last word has bit 15 set.
-}
packZCharsToWords : List Int -> List Int
packZCharsToWords zchars =
    let
        padded =
            padTo 6 dictionaryPadChar zchars

        word1 =
            packThreeZChars (List.take 3 padded) False

        word2 =
            packThreeZChars (List.drop 3 padded) True
    in
    [ word1, word2 ]


packThreeZChars : List Int -> Bool -> Int
packThreeZChars chars isLast =
    let
        z1 =
            getAt 0 chars |> Maybe.withDefault dictionaryPadChar

        z2 =
            getAt 1 chars |> Maybe.withDefault dictionaryPadChar

        z3 =
            getAt 2 chars |> Maybe.withDefault dictionaryPadChar

        topBit =
            if isLast then
                0x8000

            else
                0
    in
    topBit
        |> Bitwise.or (Bitwise.shiftLeftBy (2 * zcharBits) z1)
        |> Bitwise.or (Bitwise.shiftLeftBy zcharBits z2)
        |> Bitwise.or z3


{-| Convert a ZSCII code to a Unicode character.
-}
zsciiToChar : Int -> Char
zsciiToChar code =
    if code == 13 then
        '\n'

    else if code >= 32 && code <= 126 then
        Char.fromCode code

    else
        '?'


{-| Convert a Unicode character to a ZSCII code.
Returns Nothing for characters not representable in ZSCII.
-}
charToZscii : Char -> Maybe Int
charToZscii ch =
    if ch == '\n' then
        Just 13

    else
        let
            code =
                Char.toCode ch
        in
        if code >= 32 && code <= 126 then
            Just code

        else
            Nothing



-- INTERNAL: Z-character constants


{-| Z-char 4 shifts the next Z-char into alphabet A1 (uppercase letters).
-}
a1Shift : Int
a1Shift =
    4


{-| Z-char 5 shifts the next Z-char into alphabet A2 (digits and punctuation).
-}
a2Shift : Int
a2Shift =
    5


{-| V3 dictionary entries are padded to exactly 6 Z-characters using Z-char 5.
Same numeric value as `a2Shift`, but used in a dictionary-padding role.
-}
dictionaryPadChar : Int
dictionaryPadChar =
    5


{-| In alphabet A2, Z-char 6 introduces a 10-bit ZSCII escape: the next
two Z-chars supply the high and low 5 bits of a ZSCII code. Used for
characters that don't appear in any of the three alphabets.
-}
zsciiEscape : Int
zsciiEscape =
    6


{-| Number of bits used to represent a single Z-character. Three Z-chars
are packed into a 16-bit word (5 + 5 + 5 = 15 bits, leaving the top bit
as an end-of-string marker).
-}
zcharBits : Int
zcharBits =
    5


{-| Bit mask matching one Z-character (the low `zcharBits` bits of an int).
-}
zcharMask : Int
zcharMask =
    0x1F



-- Reading Z-words from memory


{-| Walk Z-encoded string words starting at `addr`, stopping after the
first word whose top bit is set. Returns the collected words (in reading
order) and the byte address immediately after the terminator word. The
`acc` parameter should normally be `[]`; it exists to support tail-call
accumulation.
-}
readZWords : Int -> Memory -> List Int -> ( List Int, Int )
readZWords addr mem acc =
    let
        word =
            Memory.readWord addr mem

        newAcc =
            word :: acc

        nextAddr =
            addr + Memory.wordLength

        isEnd =
            Bitwise.and word 0x8000 /= 0
    in
    if isEnd then
        ( List.reverse newAcc, nextAddr )

    else
        readZWords nextAddr mem newAcc



-- INTERNAL: Decoding Z-characters from words


type alias DecodeState =
    { output : List Char
    , alphabet : Int
    , pending : Pending
    }


type Pending
    = None
    | Abbreviation Int
    | ZsciiHigh
    | ZsciiLow Int


decodeWords : List Int -> Int -> Memory -> Bool -> List Char
decodeWords words abbrTableAddr mem isAbbreviation =
    let
        zchars =
            words |> List.concatMap extractZChars

        initial =
            { output = [], alphabet = 0, pending = None }

        final =
            List.foldl (processZChar abbrTableAddr mem isAbbreviation) initial zchars
    in
    List.reverse final.output


extractZChars : Int -> List Int
extractZChars word =
    [ Bitwise.and (Bitwise.shiftRightZfBy (2 * zcharBits) word) zcharMask
    , Bitwise.and (Bitwise.shiftRightZfBy zcharBits word) zcharMask
    , Bitwise.and word zcharMask
    ]


processZChar : Int -> Memory -> Bool -> Int -> DecodeState -> DecodeState
processZChar abbrTableAddr mem isAbbreviation zchar state =
    case state.pending of
        Abbreviation abbrBase ->
            let
                abbrChars =
                    if isAbbreviation then
                        -- Abbreviations cannot contain abbreviations
                        []

                    else
                        let
                            abbrIndex =
                                abbrBase + zchar

                            abbrWordAddr =
                                abbrTableAddr + abbrIndex * Memory.wordLength

                            abbrStringAddr =
                                Memory.unpackAddress (Memory.readWord abbrWordAddr mem) mem

                            ( abbrWords, _ ) =
                                readZWords abbrStringAddr mem []
                        in
                        decodeWords abbrWords abbrTableAddr mem True
            in
            { state
                | output = List.reverse abbrChars ++ state.output
                , pending = None
                , alphabet = 0
            }

        ZsciiHigh ->
            { state | pending = ZsciiLow zchar }

        ZsciiLow highBits ->
            let
                zsciiCode =
                    Bitwise.or (Bitwise.shiftLeftBy zcharBits highBits) zchar

                ch =
                    zsciiToChar zsciiCode
            in
            { state
                | output = ch :: state.output
                , pending = None
                , alphabet = 0
            }

        None ->
            if zchar == 0 then
                { state | output = ' ' :: state.output, alphabet = 0 }

            else if zchar >= 1 && zchar <= 3 then
                { state | pending = Abbreviation ((zchar - 1) * 32) }

            else if zchar == a1Shift then
                { state | alphabet = 1 }

            else if zchar == a2Shift then
                { state | alphabet = 2 }

            else if state.alphabet == 2 && zchar == zsciiEscape then
                { state | pending = ZsciiHigh, alphabet = 0 }

            else if state.alphabet == 2 && zchar == 7 then
                { state | output = '\n' :: state.output, alphabet = 0 }

            else
                let
                    ch =
                        alphabetChar state.alphabet zchar
                in
                { state | output = ch :: state.output, alphabet = 0 }



-- INTERNAL: Alphabet tables


{-| V3 default alphabet table. Maps (alphabet, zchar 6-31) to a character.
-}
alphabetChar : Int -> Int -> Char
alphabetChar alphabet zchar =
    let
        index =
            zchar - 6
    in
    case alphabet of
        0 ->
            getAt index a0 |> Maybe.withDefault '?'

        1 ->
            getAt index a1 |> Maybe.withDefault '?'

        2 ->
            getAt index a2 |> Maybe.withDefault '?'

        _ ->
            '?'


a0 : List Char
a0 =
    String.toList "abcdefghijklmnopqrstuvwxyz"


a1 : List Char
a1 =
    String.toList "ABCDEFGHIJKLMNOPQRSTUVWXYZ"


a2 : List Char
a2 =
    -- Positions 0-1 (zchars 6-7) are special (10-bit escape and newline),
    -- handled before we reach the lookup. We include placeholders so that
    -- indexing by (zchar - 6) works correctly for zchars 8-31.
    String.toList "^^0123456789.,!?_#'\"/\\-:()"



-- INTERNAL: Encoding characters to Z-chars


charToZCharSequence : Char -> List Int
charToZCharSequence ch =
    case findInAlphabet 0 ch of
        Just zchar ->
            [ zchar ]

        Nothing ->
            case findInAlphabet 1 ch of
                Just zchar ->
                    [ a1Shift, zchar ]

                Nothing ->
                    case findInAlphabet 2 ch of
                        Just zchar ->
                            [ a2Shift, zchar ]

                        Nothing ->
                            -- Fall back to 10-bit ZSCII escape
                            case charToZscii ch of
                                Just code ->
                                    [ a2Shift
                                    , zsciiEscape
                                    , Bitwise.and (Bitwise.shiftRightZfBy zcharBits code) zcharMask
                                    , Bitwise.and code zcharMask
                                    ]

                                Nothing ->
                                    []


findInAlphabet : Int -> Char -> Maybe Int
findInAlphabet alphabet ch =
    let
        table =
            case alphabet of
                0 ->
                    a0

                1 ->
                    a1

                _ ->
                    a2
    in
    findIndex ch table
        |> Maybe.map (\i -> i + 6)


