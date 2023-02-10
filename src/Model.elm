module Model exposing (..)

import Draggable
import Scene
import State exposing (State(..))
import Types exposing (..)


type alias Model fig =
    { scene : Scene.Scene fig
    , scale : Float
    , drag : Draggable.State ( Key, SubKey )
    , error : Maybe String
    , state : State
    }


init : Model a
init =
    { scene = Scene.init 20 20
    , drag = Draggable.init
    , scale = 0.1
    , error = Nothing
    , state = StandardEditor
    }


withState : State -> Model fig -> Model fig
withState st m =
    { m | state = st }
