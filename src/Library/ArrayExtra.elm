module Library.ArrayExtra exposing (merge)

{-| Generic helpers for working with `Array` values.

@docs merge

-}

import Array exposing (Array)


{-| Overwrite the leading elements of `target` with those of `source`,
leaving the tail of `target` unchanged. The result always has the same
length as `target`: if `source` is longer, it is truncated.

    merge (Array.fromList [ 9, 8, 7 ]) (Array.fromList [ 1, 2, 3, 4, 5 ])
        == Array.fromList [ 9, 8, 7, 4, 5 ]

    merge (Array.fromList [ 9, 8 ]) (Array.fromList [ 1, 2, 3 ])
        == Array.fromList [ 9, 8, 3 ]

    merge (Array.fromList [ 9, 8, 7, 6 ]) (Array.fromList [ 1, 2 ])
        == Array.fromList [ 9, 8 ]

-}
merge : Array a -> Array a -> Array a
merge source target =
    let
        targetLen =
            Array.length target

        sourceLen =
            Array.length source
    in
    if sourceLen >= targetLen then
        Array.slice 0 targetLen source

    else
        Array.append source (Array.slice sourceLen targetLen target)
