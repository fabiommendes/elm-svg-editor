module Model exposing (..)

import BoundingBox2d
import Draggable
import Geometry exposing (Point)
import Point2d
import Scene
import State exposing (State(..))
import Types exposing (..)
import Vector2d


type alias Model fig =
    { scene : Scene.Scene fig
    , scale : Float
    , drag : Draggable.State ( Key, SubKey )
    , error : Maybe String
    , state : State fig
    }


init : Model a
init =
    { scene = Scene.init 20 20
    , drag = Draggable.init
    , scale = 0.2
    , error = Nothing
    , state = StandardEditor
    }


withState : State fig -> Model fig -> Model fig
withState st m =
    { m | state = st }


notifyClick : Point -> Model fig -> Model fig
notifyClick pt m =
    case m.state of
        ClickToInsert key factory ->
            let
                offset =
                    Vector2d.xy
                        (BoundingBox2d.minX m.scene.bbox)
                        (BoundingBox2d.minY m.scene.bbox)

                pt_ =
                    pt
                        |> Point2d.scaleAbout Point2d.origin m.scale
                        |> Point2d.translateBy offset
            in
            { m | scene = Tuple.second (Scene.insertAs key (factory pt_) m.scene) }

        _ ->
            m
