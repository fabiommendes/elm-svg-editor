module Model exposing (..)

import BoundingBox2d
import Draggable
import Figure exposing (Figure)
import Geometry exposing (BBox, Point, Vector, point, vector)
import Point2d
import Quantity as Q
import Scene exposing (Scene)
import Shape
import State exposing (State(..))
import Types exposing (..)
import UndoList exposing (UndoList)
import Util exposing (flip)
import Vector2d


type alias Model =
    { scenes : UndoList Scene.Scene
    , scale : Float
    , translation : Vector
    , bbox : BBox
    , drag : Draggable.State ( Key, SubKey )
    , error : Maybe String
    , state : State
    }


focused : Model -> BBox
focused =
    .bbox


focusTo : BBox -> Model -> Model
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


init : Model
init =
    { scenes = UndoList.fresh Scene.init
    , drag = Draggable.init
    , error = Nothing
    , state = StandardEditor
    , scale = 1.0
    , translation = vector ( 0, 0 )
    , bbox = BoundingBox2d.from (point ( 0, 0 )) (point ( 20, 20 ))
    }


changeState : State -> Model -> Model
changeState st m =
    if st == m.state then
        m

    else
        case ( m.state, st ) of
            ( Connecting (Just key), _ ) ->
                m
                    |> transformScene
                        (\s ->
                            s
                                |> Scene.getElement key
                                |> Maybe.map (flip Shape.endConnection s)
                                |> Maybe.withDefault s
                        )
                    |> withState st

            _ ->
                { m | state = st }


withState : State -> Model -> Model
withState st m =
    { m | state = st }


withError : String -> Model -> Model
withError st m =
    { m | error = Just st }


withFigures : String -> List Figure -> Model -> Model
withFigures prefix figs =
    updateScene (Scene.insertManyAs prefix figs)


clearError : Model -> Model
clearError m =
    { m | error = Nothing }


{-| Transform present scene, putting it as the new present state
-}
updateScene : (Scene -> Scene) -> Model -> Model
updateScene f m =
    { m | scenes = m.scenes |> UndoList.new (f m.scenes.present) }


{-| Transform present scene, without registering in history
-}
transformScene : (Scene -> Scene) -> Model -> Model
transformScene f ({ scenes } as m) =
    { m | scenes = { scenes | present = f scenes.present } }


onScene : (Scene -> b) -> Model -> b
onScene f =
    scene >> f


pushScene : Scene -> Model -> Model
pushScene s =
    updateScene (\_ -> s)


scene : Model -> Scene
scene =
    .scenes >> .present


undo : Model -> Model
undo m =
    { m | scenes = UndoList.undo m.scenes }


redo : Model -> Model
redo m =
    { m | scenes = UndoList.redo m.scenes }


clearHistory : Model -> Model
clearHistory m =
    { m | scenes = UndoList.fresh m.scenes.present }


trimHistory : Model -> Model
trimHistory ({ scenes } as m) =
    { m | scenes = { scenes | past = List.take 50 scenes.past } }


notifyClick : Point -> Model -> Model
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
            { m
                | scenes =
                    m.scenes
                        |> UndoList.new (Scene.insertManyAs key [ factory pt_ ] m.scenes.present)
            }

        _ ->
            m
