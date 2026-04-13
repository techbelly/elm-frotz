module ZMachine.ObjectTable exposing
    ( parent, sibling, child
    , setParent, setSibling, setChild
    , shortName
    , testAttribute, setAttribute, clearAttribute
    , removeFromParent
    , findProperty, nextPropertyNumber
    , propertyDefault, propertyLength
    )

{-| Z-Machine object table access (Standard §12).

Version-dependent layout is read from `Memory.profile`:

  - V3: 31 property defaults, 9-byte entries (4 attr bytes, 1-byte
    parent/sibling/child, 2-byte property pointer).
  - V5: 63 property defaults, 14-byte entries (6 attr bytes, 2-byte
    parent/sibling/child, 2-byte property pointer).

Object numbers are 1-indexed; number 0 is the "nothing" sentinel — reads
return 0 and writes are no-ops, so callers can walk sibling/child chains
without guarding explicitly.


# Tree

@docs parent, sibling, child, setParent, setSibling, setChild, removeFromParent


# Attributes

@docs testAttribute, setAttribute, clearAttribute


# Properties

@docs shortName, findProperty, nextPropertyNumber
@docs propertyDefault, propertyLength

-}

import Bitwise
import ZMachine.Header as Header
import ZMachine.Memory as Memory exposing (Memory)
import ZMachine.Text as Text



-- ENTRIES


{-| Byte address of an object entry. The caller should guard
against `objNum == 0` — this function returns a meaningless address.
-}
address : Int -> Memory -> Int
address objNum mem =
    let
        p =
            Memory.profile mem

        tableBase =
            Header.objectTableAddress mem

        defaultsSize =
            p.numPropertyDefaults * Memory.wordLength
    in
    tableBase + defaultsSize + (objNum - 1) * p.objectEntrySize



-- TREE


{-| Parent object number, or 0 if `objNum` is 0.
-}
parent : Int -> Memory -> Int
parent objNum mem =
    readRelation (Memory.profile mem).parentOffset objNum mem


{-| Next sibling object number, or 0 if `objNum` is 0.
-}
sibling : Int -> Memory -> Int
sibling objNum mem =
    readRelation (Memory.profile mem).siblingOffset objNum mem


{-| First child object number, or 0 if `objNum` is 0.
-}
child : Int -> Memory -> Int
child objNum mem =
    readRelation (Memory.profile mem).childOffset objNum mem


{-| Overwrite the parent pointer. No-op if `objNum` is 0.
-}
setParent : Int -> Int -> Memory -> Memory
setParent objNum value mem =
    writeRelation (Memory.profile mem).parentOffset objNum value mem


{-| Overwrite the sibling pointer. No-op if `objNum` is 0.
-}
setSibling : Int -> Int -> Memory -> Memory
setSibling objNum value mem =
    writeRelation (Memory.profile mem).siblingOffset objNum value mem


{-| Overwrite the child pointer. No-op if `objNum` is 0.
-}
setChild : Int -> Int -> Memory -> Memory
setChild objNum value mem =
    writeRelation (Memory.profile mem).childOffset objNum value mem


readRelation : Int -> Int -> Memory -> Int
readRelation offset objNum mem =
    if objNum == 0 then
        0

    else
        let
            addr =
                address objNum mem + offset
        in
        if (Memory.profile mem).objectPointerSize == 1 then
            Memory.readByte addr mem

        else
            Memory.readWord addr mem


writeRelation : Int -> Int -> Int -> Memory -> Memory
writeRelation offset objNum value mem =
    if objNum == 0 then
        mem

    else
        let
            addr =
                address objNum mem + offset
        in
        if (Memory.profile mem).objectPointerSize == 1 then
            Memory.writeByte addr value mem

        else
            Memory.writeWord addr value mem


{-| Detach `objNum` from its current parent, unlinking it from the parent's
child/sibling chain. The object's own parent/sibling fields are left
untouched; callers that want `remove_obj` semantics should overwrite them
separately.
-}
removeFromParent : Int -> Memory -> Memory
removeFromParent objNum mem =
    let
        p =
            parent objNum mem
    in
    if p == 0 then
        mem

    else
        let
            parentChild =
                child p mem
        in
        if parentChild == objNum then
            setChild p (sibling objNum mem) mem

        else
            unlinkFromSiblingChain parentChild objNum mem


unlinkFromSiblingChain : Int -> Int -> Memory -> Memory
unlinkFromSiblingChain current target mem =
    if current == 0 then
        mem

    else
        let
            next =
                sibling current mem
        in
        if next == target then
            setSibling current (sibling target mem) mem

        else
            unlinkFromSiblingChain next target mem



-- ATTRIBUTES


{-| True iff attribute `attr` is set on `objNum`. False for `objNum == 0`.
-}
testAttribute : Int -> Int -> Memory -> Bool
testAttribute objNum attr mem =
    if objNum == 0 then
        False

    else
        let
            ( addr, mask ) =
                attributeLocation objNum attr mem
        in
        Bitwise.and (Memory.readByte addr mem) mask /= 0


{-| Set attribute `attr` on `objNum`. No-op for `objNum == 0`.
-}
setAttribute : Int -> Int -> Memory -> Memory
setAttribute =
    updateAttribute Bitwise.or


{-| Clear attribute `attr` on `objNum`. No-op for `objNum == 0`.
-}
clearAttribute : Int -> Int -> Memory -> Memory
clearAttribute =
    updateAttribute (\byte mask -> Bitwise.and byte (Bitwise.complement mask))


updateAttribute : (Int -> Int -> Int) -> Int -> Int -> Memory -> Memory
updateAttribute combine objNum attr mem =
    if objNum == 0 then
        mem

    else
        let
            ( addr, mask ) =
                attributeLocation objNum attr mem

            byte =
                Memory.readByte addr mem
        in
        Memory.writeByte addr (combine byte mask) mem


attributeLocation : Int -> Int -> Memory -> ( Int, Int )
attributeLocation objNum attr mem =
    ( address objNum mem + (attr // 8)
    , Bitwise.shiftLeftBy (7 - modBy 8 attr) 1
    )



-- PROPERTIES


{-| Address of the property table for `objNum` (where the short-name
header begins). Returns 0 for `objNum == 0`.
-}
propertyTableAddress : Int -> Memory -> Int
propertyTableAddress objNum mem =
    if objNum == 0 then
        0

    else
        Memory.readWord (address objNum mem + (Memory.profile mem).propPtrOffset) mem


{-| The object's short name — the encoded Z-string stored at the head of
its property table. Returns `""` for `objNum == 0` and for objects whose
name header has zero length.
-}
shortName : Int -> Memory -> String
shortName objNum mem =
    if objNum == 0 then
        ""

    else
        let
            propTableAddr =
                propertyTableAddress objNum mem

            nameLen =
                Memory.readByte propTableAddr mem
        in
        if nameLen == 0 then
            ""

        else
            Tuple.first (Text.decodeZString (propTableAddr + 1) mem)


{-| Address of the first property entry, i.e. just past the short-name
header of the property table.
-}
firstPropertyAddress : Int -> Memory -> Int
firstPropertyAddress objNum mem =
    let
        propTableAddr =
            propertyTableAddress objNum mem

        nameLen =
            Memory.readByte propTableAddr mem
    in
    propTableAddr + 1 + nameLen * Memory.wordLength


{-| Find a property on an object. Returns `Just (dataAddress, dataLength)`
if present, `Nothing` otherwise.
-}
findProperty : Int -> Int -> Memory -> Maybe ( Int, Int )
findProperty objNum propNum mem =
    findPropertyAt (firstPropertyAddress objNum mem) propNum mem


findPropertyAt : Int -> Int -> Memory -> Maybe ( Int, Int )
findPropertyAt addr propNum mem =
    if Memory.readByte addr mem == 0 then
        Nothing

    else
        let
            decoded =
                decodePropertyHeader addr mem

            dataAddr =
                addr + decoded.headerSize
        in
        if decoded.num == propNum then
            Just ( dataAddr, decoded.dataLen )

        else if decoded.num < propNum then
            -- Properties are stored in descending order; we've passed it.
            Nothing

        else
            findPropertyAt (dataAddr + decoded.dataLen) propNum mem


{-| Implements the `get_next_prop` opcode. If `propNum` is 0, returns the
first property number on the object. Otherwise returns the number of the
property that follows `propNum`, or 0 if `propNum` is the last one.
-}
nextPropertyNumber : Int -> Int -> Memory -> Int
nextPropertyNumber objNum propNum mem =
    let
        addr =
            firstPropertyAddress objNum mem
    in
    if propNum == 0 then
        propertyNumberAt addr mem

    else
        walkToNextNumber addr propNum mem


propertyNumberAt : Int -> Memory -> Int
propertyNumberAt addr mem =
    Bitwise.and (Memory.readByte addr mem) (Memory.profile mem).propertyNumberMask


walkToNextNumber : Int -> Int -> Memory -> Int
walkToNextNumber addr targetPropNum mem =
    if Memory.readByte addr mem == 0 then
        0

    else
        let
            decoded =
                decodePropertyHeader addr mem

            nextAddr =
                addr + decoded.headerSize + decoded.dataLen
        in
        if decoded.num == targetPropNum then
            propertyNumberAt nextAddr mem

        else
            walkToNextNumber nextAddr targetPropNum mem


{-| Decode a property header at `addr` into its property number, data
length, and the number of header bytes consumed.

V3: single size byte — bits 7-5 encode (dataLen - 1), bits 4-0 the
property number.

V5: if bit 7 is clear, single byte — bit 6 selects 1-byte (0) or
2-byte (1) data, bits 5-0 are the property number. If bit 7 is set,
this is the second byte of a two-byte header — bits 5-0 give the data
length (0 means 64), and the property number was in the first byte.

-}
decodePropertyHeader : Int -> Memory -> { num : Int, dataLen : Int, headerSize : Int }
decodePropertyHeader addr mem =
    let
        firstByte =
            Memory.readByte addr mem

        p =
            Memory.profile mem
    in
    case p.version of
        Memory.V3 ->
            { num = Bitwise.and firstByte p.propertyNumberMask
            , dataLen = Bitwise.shiftRightZfBy 5 firstByte + 1
            , headerSize = 1
            }

        Memory.V5 ->
            if Bitwise.and firstByte 0x80 /= 0 then
                -- Bit 7 set on first byte means two-byte header follows
                let
                    secondByte =
                        Memory.readByte (addr + 1) mem

                    rawLen =
                        Bitwise.and secondByte 0x3F
                in
                { num = Bitwise.and firstByte p.propertyNumberMask
                , dataLen =
                    if rawLen == 0 then
                        64

                    else
                        rawLen
                , headerSize = 2
                }

            else
                -- Single-byte header: bit 6 selects 1 or 2 byte data
                { num = Bitwise.and firstByte p.propertyNumberMask
                , dataLen =
                    if Bitwise.and firstByte 0x40 /= 0 then
                        2

                    else
                        1
                , headerSize = 1
                }


{-| Default value to use for property `propNum` when the object doesn't
define it (per §12.2).
-}
propertyDefault : Int -> Memory -> Int
propertyDefault propNum mem =
    Memory.readWord
        (Header.objectTableAddress mem + (propNum - 1) * Memory.wordLength)
        mem


{-| Length in bytes of the property whose data address is `dataAddr`.
Returns 0 if `dataAddr` is 0 (matching `get_prop_len`'s spec).

In V3, the size byte is always at `dataAddr - 1`. In V5, if the byte
at `dataAddr - 1` has bit 7 set it is the second byte of a two-byte
header (and contains the length directly); otherwise the single-byte
header at `dataAddr - 1` encodes the length via bit 6.

-}
propertyLength : Int -> Memory -> Int
propertyLength dataAddr mem =
    if dataAddr == 0 then
        0

    else
        let
            prevByte =
                Memory.readByte (dataAddr - 1) mem
        in
        case (Memory.profile mem).version of
            Memory.V3 ->
                Bitwise.shiftRightZfBy 5 prevByte + 1

            Memory.V5 ->
                if Bitwise.and prevByte 0x80 /= 0 then
                    -- Two-byte header: bits 5-0 are the length
                    let
                        rawLen =
                            Bitwise.and prevByte 0x3F
                    in
                    if rawLen == 0 then
                        64

                    else
                        rawLen

                else if Bitwise.and prevByte 0x40 /= 0 then
                    2

                else
                    1
