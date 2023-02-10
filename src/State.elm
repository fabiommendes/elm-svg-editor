module State exposing (..)

{-| App as a state machine
-}
import Figure exposing (Figure)


{-| The current mode of operation
-}
type State fig
    = StandardEditor
    | Inserting (() -> Figure fig)
    | ReadOnlyView
    | ConnectingLines
