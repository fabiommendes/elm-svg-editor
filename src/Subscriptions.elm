module Subscriptions exposing (..)

import Browser.Events
import Draggable
import Model exposing (Model)
import Msg exposing (Msg(..))
import Types exposing (..)


subscriptions : Model a -> Sub (Msg a)
subscriptions model =
    Sub.batch
        [ Draggable.subscriptions OnDragMsg model.drag
        , Browser.Events.onResize (\_ _ -> OnWindowResize)
        ]
