module Library.ListExtra exposing (findIndex, getAt, padTo)

{-| Generic helpers for working with `List` values.

@docs findIndex, getAt, padTo

-}


{-| Return the zero-based index of the first element equal to the given
value, or `Nothing` if no element matches.

    findIndex 'b' [ 'a', 'b', 'c' ] == Just 1

    findIndex 'z' [ 'a', 'b', 'c' ] == Nothing

-}
findIndex : a -> List a -> Maybe Int
findIndex target list =
    findIndexHelper target list 0


findIndexHelper : a -> List a -> Int -> Maybe Int
findIndexHelper target list idx =
    case list of
        [] ->
            Nothing

        x :: rest ->
            if x == target then
                Just idx

            else
                findIndexHelper target rest (idx + 1)


{-| Return the element at a zero-based index, or `Nothing` if the index
is out of range.

    getAt 0 [ 'a', 'b', 'c' ] == Just 'a'

    getAt 5 [ 'a', 'b', 'c' ] == Nothing

-}
getAt : Int -> List a -> Maybe a
getAt index list =
    list |> List.drop index |> List.head


{-| Right-pad a list to a target length with a fill value, truncating if
the list is already longer. The result is always exactly `length` elements.

    padTo 4 0 [ 1, 2 ] == [ 1, 2, 0, 0 ]

    padTo 2 0 [ 1, 2, 3 ] == [ 1, 2 ]

-}
padTo : Int -> a -> List a -> List a
padTo length fill list =
    let
        truncated =
            List.take length list

        missing =
            length - List.length truncated
    in
    truncated ++ List.repeat missing fill
