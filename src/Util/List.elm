module Util.List exposing (..)

import List.Extra as List
import Maybe.Extra as Maybe
import Util exposing (flip)


{-| Move element at index one position towards the begining of the list
-}
bubbleUpAt : Int -> List a -> List a
bubbleUpAt idx list =
    case List.splitAt idx list of
        ( pre, x :: y :: rest ) ->
            pre ++ (y :: x :: rest)

        _ ->
            list


{-| Move element at index one position towards the end of the list
-}
bubbleDownAt : Int -> List a -> List a
bubbleDownAt idx =
    bubbleUpAt (idx - 1)


{-| Move element at index one position towards the end of the list
-}
bubbleDownIfAt : (a -> Bool) -> Int -> List a -> List a
bubbleDownIfAt cond idx list =
    case List.splitAt (idx - 1) list of
        ( pre, x :: y :: rest ) ->
            if cond y then
                pre ++ (y :: x :: rest)

            else
                list

        _ ->
            list


{-| Move element at index one position towards the begining of the list
-}
bubbleUpWhen : (a -> Bool) -> List a -> List a
bubbleUpWhen cond list =
    case List.splitWhen cond list |> Maybe.withDefault ( list, [] ) of
        ( pre, x :: y :: rest ) ->
            pre ++ (y :: x :: rest)

        _ ->
            list


{-| Move element at index one position towards the begining of the list
-}
bubbleDownWhen : (a -> Bool) -> List a -> List a
bubbleDownWhen cond list =
    List.findIndex cond list
        |> Maybe.unwrap list (flip bubbleDownAt list)
