module ZMachine.Stack exposing (CallFrame, newFrame, getLocal, setLocal)

{-| Call stack frame management for the Z-Machine.

Each routine call creates a `CallFrame` that captures the return address,
where to store the return value, the routine's local variables, and the
evaluation stack at the point of the call.

@docs CallFrame, newFrame, getLocal, setLocal

-}

import Array exposing (Array)
import ZMachine.Instruction exposing (VariableRef)


{-| A single call stack frame.
-}
type alias CallFrame =
    { returnPC : Int
    , returnStore : Maybe VariableRef
    , locals : Array Int
    , evalStack : List Int
    }


{-| Create a new call frame with the given number of locals and initial values.
-}
newFrame : Int -> List Int -> Int -> Maybe VariableRef -> List Int -> CallFrame
newFrame numLocals initialValues returnPC returnStore currentEvalStack =
    let
        locals =
            initialValues
                |> List.take numLocals
                |> Array.fromList
                |> padArray numLocals 0
    in
    { returnPC = returnPC
    , returnStore = returnStore
    , locals = locals
    , evalStack = currentEvalStack
    }


{-| Read a local variable (1-indexed, as per Z-Machine convention).
Returns 0 for out-of-range indices.
-}
getLocal : Int -> CallFrame -> Int
getLocal n frame =
    Array.get (n - 1) frame.locals |> Maybe.withDefault 0


{-| Write a local variable (1-indexed).
-}
setLocal : Int -> Int -> CallFrame -> CallFrame
setLocal n value frame =
    { frame | locals = Array.set (n - 1) (toUnsigned16 value) frame.locals }



-- INTERNAL


padArray : Int -> a -> Array a -> Array a
padArray targetLength default arr =
    if Array.length arr >= targetLength then
        arr

    else
        padArray targetLength default (Array.push default arr)


toUnsigned16 : Int -> Int
toUnsigned16 n =
    modBy 65536
        (if n < 0 then
            n + 65536

         else
            n
        )
