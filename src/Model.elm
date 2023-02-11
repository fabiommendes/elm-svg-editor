module Model exposing (..)

import BoundingBox2d
import Draggable
import Geometry exposing (BBox, Point, Vector, point, vector)
import Point2d
import Quantity as Q
import Scene
import State exposing (State(..))
import Types exposing (..)
import Vector2d


type alias Model fig =
    { scene : Scene.Scene fig
    , scale : Float
    , translation : Vector
    , bbox : BBox
    , drag : Draggable.State ( Key, SubKey )
    , error : Maybe String
    , state : State fig
    }


focused : Model a -> BBox
focused =
    .bbox


focusTo : BBox -> Model a -> Model a
focusTo bbox m =
    let
        growth =
            Q.ratio
                (Tuple.first (BoundingBox2d.dimensions m.bbox))
                (Tuple.first (BoundingBox2d.dimensions bbox))

        newScale =
            m.scale * growth
    in
    { m | scale = newScale, bbox = bbox }


init : Model a
init =
    { scene = Scene.init
    , drag = Draggable.init
    , error = Nothing
    , state = StandardEditor
    , scale = 1.0
    , translation = vector ( 0, 0 )
    , bbox = BoundingBox2d.from (point ( 0, 0 )) (point ( 20, 20 ))
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
                        (BoundingBox2d.minX m.bbox)
                        (BoundingBox2d.minY m.bbox)

                pt_ =
                    pt
                        |> Point2d.scaleAbout Point2d.origin m.scale
                        |> Point2d.translateBy offset
            in
            { m | scene = Tuple.second (Scene.insertAs key (factory pt_) m.scene) }

        _ ->
            m
