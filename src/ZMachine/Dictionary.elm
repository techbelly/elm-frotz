module ZMachine.Dictionary exposing (tokenize, lookupWord, TokenResult)

{-| Z-Machine dictionary lookup and tokenization.

Handles splitting input text into words, encoding them for dictionary lookup,
and writing the results to the parse buffer.

@docs tokenize, lookupWord, TokenResult

-}

import ZMachine.Memory as Memory exposing (Memory)
import ZMachine.Header as Header
import ZMachine.Text as Text


{-| Result of tokenizing a single word.
-}
type alias TokenResult =
    { dictAddr : Int
    , textLength : Int
    , textPosition : Int
    }


{-| Tokenize input text and write results to the text buffer and parse buffer.

  - Writes the lowercased input to textBufAddr (starting at byte 1, null-terminated)
  - Splits into words using dictionary separators
  - Looks up each word in the dictionary
  - Writes token results to parseBufAddr

Returns the updated memory.

-}
tokenize : String -> Int -> Int -> Memory -> Memory
tokenize input textBufAddr parseBufAddr mem =
    let
        separators =
            readSeparators mem

        lowered =
            String.toLower input

        -- Write input to text buffer (byte 1 onward, null-terminated)
        memWithText =
            writeTextBuffer lowered textBufAddr mem

        -- Split into tokens
        tokens =
            splitIntoWords lowered separators

        -- Look up each token in the dictionary
        maxWords =
            Memory.readByte parseBufAddr mem

        textOffset =
            (Memory.profile mem).textBufferOffset

        tokenResults =
            tokens
                |> List.take maxWords
                |> List.map
                    (\token ->
                        { dictAddr = lookupWord token.text mem
                        , textLength = token.length
                        , textPosition = token.position + textOffset
                        }
                    )
    in
    writeParseBuffer tokenResults parseBufAddr memWithText


{-| Look up a word in the dictionary. Returns the dictionary entry address,
or 0 if not found.
-}
lookupWord : String -> Memory -> Int
lookupWord word mem =
    let
        dictAddr =
            Header.dictionaryAddress mem

        -- Read dictionary header
        numSeparators =
            Memory.readByte dictAddr mem

        entryLength =
            Memory.readByte (dictAddr + 1 + numSeparators) mem

        numEntries =
            Memory.readSignedWord (dictAddr + 2 + numSeparators) mem

        entriesStart =
            dictAddr + 4 + numSeparators

        -- Encode the word to Z-character dictionary form
        p =
            Memory.profile mem

        zchars =
            Text.encodeToZChars mem word

        target =
            Text.packZCharsToWords mem zchars
    in
    if numEntries >= 0 then
        -- Sorted: binary search
        binarySearch entriesStart entryLength p.dictWordWords 0 (numEntries - 1) target mem

    else
        -- Unsorted: linear search
        linearSearch entriesStart entryLength p.dictWordWords (abs numEntries) target mem



-- INTERNAL: Text buffer writing


writeTextBuffer : String -> Int -> Memory -> Memory
writeTextBuffer text addr mem =
    let
        p =
            Memory.profile mem

        offset =
            p.textBufferOffset

        chars =
            String.toList text

        memWithChars =
            List.foldl
                (\( i, ch ) m ->
                    Memory.writeByte (addr + offset + i) (Char.toCode ch) m
                )
                mem
                (List.indexedMap Tuple.pair chars)
    in
    case p.version of
        Memory.V3 ->
            -- Null-terminate
            Memory.writeByte (addr + offset + List.length chars) 0 memWithChars

        Memory.V5 ->
            -- Write character count to byte 1
            Memory.writeByte (addr + 1) (List.length chars) memWithChars



-- INTERNAL: Parse buffer writing


writeParseBuffer : List TokenResult -> Int -> Memory -> Memory
writeParseBuffer results parseBufAddr mem =
    let
        -- Byte 1: number of words found
        memWithCount =
            Memory.writeByte (parseBufAddr + 1) (List.length results) mem
    in
    List.foldl
        (\( i, token ) m ->
            let
                entryAddr =
                    parseBufAddr + 2 + i * 4
            in
            m
                |> Memory.writeWord entryAddr token.dictAddr
                |> Memory.writeByte (entryAddr + 2) token.textLength
                |> Memory.writeByte (entryAddr + 3) token.textPosition
        )
        memWithCount
        (List.indexedMap Tuple.pair results)



-- INTERNAL: Word splitting


type alias Token =
    { text : String
    , position : Int
    , length : Int
    }


splitIntoWords : String -> List Char -> List Token
splitIntoWords input separators =
    splitHelper (String.toList input) separators 0 [] []


splitHelper : List Char -> List Char -> Int -> List Char -> List Token -> List Token
splitHelper chars separators pos currentWord tokens =
    let
        -- `tokens` with the current word appended (if any). Used by every
        -- branch that ends the current word.
        flushed =
            if List.isEmpty currentWord then
                tokens

            else
                let
                    word =
                        String.fromList (List.reverse currentWord)

                    wordStart =
                        pos - List.length currentWord
                in
                { text = word, position = wordStart, length = String.length word } :: tokens
    in
    case chars of
        [] ->
            List.reverse flushed

        ch :: rest ->
            if ch == ' ' then
                -- Space: flush current word, skip the space
                splitHelper rest separators (pos + 1) [] flushed

            else if List.member ch separators then
                -- Separator: flush current word, then add separator as its own token
                let
                    sepToken =
                        { text = String.fromChar ch, position = pos, length = 1 }
                in
                splitHelper rest separators (pos + 1) [] (sepToken :: flushed)

            else
                -- Regular character: accumulate
                splitHelper rest separators (pos + 1) (ch :: currentWord) tokens



-- INTERNAL: Dictionary separator reading


readSeparators : Memory -> List Char
readSeparators mem =
    let
        dictAddr =
            Header.dictionaryAddress mem

        count =
            Memory.readByte dictAddr mem
    in
    List.range 1 count
        |> List.map (\i -> Char.fromCode (Memory.readByte (dictAddr + i) mem))



-- INTERNAL: Dictionary search


readEntryKey : Int -> Int -> Memory -> List Int
readEntryKey numWords addr mem =
    List.range 0 (numWords - 1)
        |> List.map (\i -> Memory.readWord (addr + i * Memory.wordLength) mem)


binarySearch : Int -> Int -> Int -> Int -> Int -> List Int -> Memory -> Int
binarySearch entriesStart entryLength numWords low high target mem =
    if low > high then
        0

    else
        let
            mid =
                (low + high) // 2

            midAddr =
                entriesStart + mid * entryLength

            midKey =
                readEntryKey numWords midAddr mem
        in
        if midKey == target then
            midAddr

        else if target < midKey then
            binarySearch entriesStart entryLength numWords low (mid - 1) target mem

        else
            binarySearch entriesStart entryLength numWords (mid + 1) high target mem


linearSearch : Int -> Int -> Int -> Int -> List Int -> Memory -> Int
linearSearch entriesStart entryLength numWords count target mem =
    linearSearchHelper entriesStart entryLength numWords 0 count target mem


linearSearchHelper : Int -> Int -> Int -> Int -> Int -> List Int -> Memory -> Int
linearSearchHelper entriesStart entryLength numWords index count target mem =
    if index >= count then
        0

    else
        let
            addr =
                entriesStart + index * entryLength

            key =
                readEntryKey numWords addr mem
        in
        if key == target then
            addr

        else
            linearSearchHelper entriesStart entryLength numWords (index + 1) count target mem
