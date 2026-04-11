module ZMachine.ObjectTable exposing
    ( parent, sibling, child
    , setParent, setSibling, setChild
    , shortName
    , testAttribute, setAttribute, clearAttribute
    , removeFromParent
    , findProperty, nextPropertyNumber
    , propertyDefault, propertyLength
    )

{-| Z-Machine V3 object table access (Standard §12).

The object table consists of:

  - 31 property default words (62 bytes)
  - N object entries, each 9 bytes: 4 attribute bytes, parent / sibling /
    child (1 byte each), and a 2-byte pointer to the property table.

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



-- LAYOUT CONSTANTS


{-| Number of property default entries (V3).
-}
numPropertyDefaults : Int
numPropertyDefaults =
    31


{-| Size of a single object entry in bytes (V3).
-}
objectEntrySize : Int
objectEntrySize =
    9


{-| Number of attribute bytes at the start of an object entry (V3).
-}
numAttributeBytes : Int
numAttributeBytes =
    4


parentOffset : Int
parentOffset =
    numAttributeBytes


siblingOffset : Int
siblingOffset =
    numAttributeBytes + 1


childOffset : Int
childOffset =
    numAttributeBytes + 2


propertyPointerOffset : Int
propertyPointerOffset =
    numAttributeBytes + 3


{-| Five low bits of a property size byte hold the property number.
-}
propertyNumberMask : Int
propertyNumberMask =
    0x1F



-- ENTRIES


{-| Byte address of an object's 9-byte entry. The caller should guard
against `objNum == 0` — this function returns a meaningless address.
-}
address : Int -> Memory -> Int
address objNum mem =
    let
        tableBase =
            Header.objectTableAddress mem

        defaultsSize =
            numPropertyDefaults * Memory.wordLength
    in
    tableBase + defaultsSize + (objNum - 1) * objectEntrySize



-- TREE


{-| Parent object number, or 0 if `objNum` is 0.
-}
parent : Int -> Memory -> Int
parent objNum mem =
    readRelation parentOffset objNum mem


{-| Next sibling object number, or 0 if `objNum` is 0.
-}
sibling : Int -> Memory -> Int
sibling objNum mem =
    readRelation siblingOffset objNum mem


{-| First child object number, or 0 if `objNum` is 0.
-}
child : Int -> Memory -> Int
child objNum mem =
    readRelation childOffset objNum mem


{-| Overwrite the parent pointer. No-op if `objNum` is 0.
-}
setParent : Int -> Int -> Memory -> Memory
setParent objNum value mem =
    writeRelation parentOffset objNum value mem


{-| Overwrite the sibling pointer. No-op if `objNum` is 0.
-}
setSibling : Int -> Int -> Memory -> Memory
setSibling objNum value mem =
    writeRelation siblingOffset objNum value mem


{-| Overwrite the child pointer. No-op if `objNum` is 0.
-}
setChild : Int -> Int -> Memory -> Memory
setChild objNum value mem =
    writeRelation childOffset objNum value mem


readRelation : Int -> Int -> Memory -> Int
readRelation offset objNum mem =
    if objNum == 0 then
        0

    else
        Memory.readByte (address objNum mem + offset) mem


writeRelation : Int -> Int -> Int -> Memory -> Memory
writeRelation offset objNum value mem =
    if objNum == 0 then
        mem

    else
        Memory.writeByte (address objNum mem + offset) value mem


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
        Memory.readWord (address objNum mem + propertyPointerOffset) mem


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
    let
        sizeByte =
            Memory.readByte addr mem
    in
    if sizeByte == 0 then
        Nothing

    else
        let
            decoded =
                decodeSizeByte sizeByte

            dataAddr =
                addr + 1
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
    Bitwise.and (Memory.readByte addr mem) propertyNumberMask


walkToNextNumber : Int -> Int -> Memory -> Int
walkToNextNumber addr targetPropNum mem =
    let
        sizeByte =
            Memory.readByte addr mem
    in
    if sizeByte == 0 then
        0

    else
        let
            decoded =
                decodeSizeByte sizeByte

            nextAddr =
                addr + 1 + decoded.dataLen
        in
        if decoded.num == targetPropNum then
            propertyNumberAt nextAddr mem

        else
            walkToNextNumber nextAddr targetPropNum mem


{-| Decode a property size byte into its `num` (property number) and
`dataLen` (length of the property's data in bytes) fields.
-}
decodeSizeByte : Int -> { num : Int, dataLen : Int }
decodeSizeByte sizeByte =
    { num = Bitwise.and sizeByte propertyNumberMask
    , dataLen = Bitwise.shiftRightZfBy 5 sizeByte + 1
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
-}
propertyLength : Int -> Memory -> Int
propertyLength dataAddr mem =
    if dataAddr == 0 then
        0

    else
        .dataLen (decodeSizeByte (Memory.readByte (dataAddr - 1) mem))
