module Internal.Types exposing (Config(..))

{-| Global application configuration

@docs Config, init


## Custom config

@docs config, controls, groups, pointRadius, sceneId, editable, shape

-}

import Draggable
import Msg exposing (Msg(..))
import State exposing (State(..))
import Types exposing (Key, Label, SubKey)


{-| The config object. Usually should be created using init or the config utilitiy function
-}
type Config
    = Cfg
        { pointRadius : Float
        , zoomControls : Bool
        , panControls : Bool
        , panWithTouch : Bool
        , groups : List Label
        , sceneId : String
        , shape : { width : Float, height : Float }
        , edit : Bool
        , drag : Draggable.Config ( Key, SubKey ) Msg
        , initialState : State
        , styleFontSize : Float
        , styleLineWidth : Float
        , styleLineWidthSelected : Float
        , stylePrimaryColor : String
        , styleConstrastColor : String
        , styleSelectedColor : String
        }
