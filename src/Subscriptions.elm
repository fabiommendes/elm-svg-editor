module Subscriptions exposing (..)

import Browser.Events
import Svg.Editor.Config exposing (Config)
import Decode exposing (keyPress)
import Draggable
import Model exposing (Model)
import Msg exposing (Msg(..))
import Types exposing (..)


subscriptions : Config -> Model -> Sub Msg
subscriptions _ m =
    Sub.batch
        [ Draggable.subscriptions OnDragMsg m.drag
        , Browser.Events.onResize (\_ _ -> OnWindowResize)
        , Browser.Events.onKeyPress keyPress
        , Browser.Events.onKeyUp keyPress
        ]
