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
    | Connecting (Maybe Key)


map : (a -> b) -> State a -> State b
map f st =
    case st of
        StandardEditor ->
            StandardEditor

        ReadOnlyView ->
            ReadOnlyView

        ClickToInsert key func ->
            ClickToInsert key (\pt -> Figure.map f (func pt))

        Connecting key ->
            Connecting key


isSimilarTo : State a -> State b -> Bool
isSimilarTo st1 st2 =
    case ( st1, st2 ) of
        ( StandardEditor, StandardEditor ) ->
            True

        ( ReadOnlyView, ReadOnlyView ) ->
            True

        ( ClickToInsert _ _, ClickToInsert _ _ ) ->
            True

        ( Connecting _, Connecting _ ) ->
            True

        _ ->
            False


isClickToInsert : State fig -> Bool
isClickToInsert state =
    case state of
        ClickToInsert _ _ ->
            True

        _ ->
            False


isConnecting : State fig -> Bool
isConnecting state =
    case state of
        Connecting _ ->
            True

        _ ->
            False
