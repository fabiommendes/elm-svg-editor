module State exposing (..)

{-| App as a state machine
-}

import Figure exposing (Figure)
import Geometry exposing (Point)
import Types exposing (Key)
import Shape.Type exposing (Any)


{-| The current mode of operation
-}
type State
    = StandardEditor
    | ReadOnlyView
    | ClickToInsert String (Point -> Figure)
    | Connecting (Maybe Key)


map : (Any -> Any) -> State -> State
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


isSimilarTo : State -> State -> Bool
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


isClickToInsert : State -> Bool
isClickToInsert state =
    case state of
        ClickToInsert _ _ ->
            True

        _ ->
            False


isConnecting : State -> Bool
isConnecting state =
    case state of
        Connecting _ ->
            True

        _ ->
            False
