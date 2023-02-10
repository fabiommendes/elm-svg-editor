module State exposing (..)

{-| App as a state machine
-}


{-| The current mode of operation
-}
type State
    = StandardEditor
    | ReadOnlyView
    | InsertingPoints
    | ConnectingLines
