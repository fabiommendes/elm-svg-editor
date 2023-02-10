module State exposing (..)

{-| App as a state machine
-}
import Figure exposing (Figure)
import Geometry exposing (Point)


{-| The current mode of operation
-}
type State fig
    = StandardEditor
    | ReadOnlyView
    | ClickToInsert (Point -> Figure fig)
    | ConnectingLines
