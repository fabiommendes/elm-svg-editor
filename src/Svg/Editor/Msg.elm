module Svg.Editor.Msg exposing (getSelectionMsg)

{-| Intercept and introspect messages


## Message queries

@docs getSelectionMsg

-}

import Msg exposing (Msg(..))
import Types exposing (Key, SubKey)


{-| Captures the selected (Key, SubKey) pair of a message when a new element is selected
-}
getSelectionMsg : Msg -> Maybe ( Key, SubKey )
getSelectionMsg msg =
    case msg of
        OnSelectFigure key subKey ->
            Just ( key, subKey )

        _ ->
            Nothing
