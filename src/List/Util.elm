module List.Util exposing (..)


insertAt : Int -> a -> List a -> List a
insertAt i x xs =
    case ( i, xs ) of
        ( _, [] ) ->
            [ x ]

        ( 0, _ ) ->
            x :: xs

        ( _, y :: ys ) ->
            y :: insertAt (i - 1) x ys
