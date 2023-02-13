module Subscriptions exposing (..)

import Browser.Events
import Config exposing (Config)
import Decode exposing (keyPress)
import Draggable
import Model exposing (Model)
import Msg exposing (Msg(..))
import Types exposing (..)


subscriptions : Config -> Model -> Sub Msg
subscriptions _ model =
    Sub.batch
        [ Draggable.subscriptions OnDragMsg model.drag
        , Browser.Events.onResize (\_ _ -> OnWindowResize)
        , Browser.Events.onKeyPress keyPress
        , Browser.Events.onKeyUp keyPress
        ]
