module Model exposing (..)

import Draggable
import Scene
import Types exposing (..)


type alias Model a =
    { scene : Scene.Scene a
    , scale : Float
    , drag : Draggable.State ( Key, SubKey )
    }


init : Model a
init =
    { scene = Scene.init 20 20
    , drag = Draggable.init
    , scale = 0.1
    }
