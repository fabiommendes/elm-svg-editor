module Subscriptions exposing (..)

import Browser.Events
import Draggable
import Model exposing (Model)
import Msg exposing (Msg(..))
import Types exposing (..)
import Decode exposing (keyPress)
import Config exposing (Config)


subscriptions : Config a -> Model a -> Sub (Msg a)
subscriptions _ model =
    Sub.batch
        [ Draggable.subscriptions OnDragMsg model.drag
        , Browser.Events.onResize (\_ _ -> OnWindowResize)
        , Browser.Events.onKeyPress keyPress
        , Browser.Events.onKeyUp keyPress
        ]
