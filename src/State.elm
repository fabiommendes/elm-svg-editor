module State exposing (..)

{-| App as a state machine
-}

import Figure exposing (Figure)
import Geometry exposing (Point)
import Types exposing (Key)


{-| The current mode of operation
-}
type State fig
    = StandardEditor
    | ReadOnlyView
    | ClickToInsert String (Point -> Figure fig)
    | ConnectingLines Key


map : (a -> b) -> State a -> State b
map f st =
    case st of
        StandardEditor ->
            StandardEditor

        ReadOnlyView ->
            ReadOnlyView

        ClickToInsert key func ->
            ClickToInsert key (\pt -> Figure.map f (func pt))

        ConnectingLines key ->
            ConnectingLines key
