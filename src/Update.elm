module Update exposing (..)

import BaseTypes exposing (Direction(..))
import BoundingBox2d
import Browser.Dom
import Config exposing (Config)
import Draggable
import Figure
import Length exposing (inMeters)
import Lens exposing (bbox, scene)
import Model exposing (Model)
import Monocle.Lens as L
import Msg exposing (Msg(..))
import Quantity as Q
import Result.Extra as Result
import Scene
import Task
import Types exposing (..)
import Vector2d


update : Config a -> Msg a -> Model a -> ( Model a, Cmd (Msg a) )
update cfg msg_ m =
    let
        return m_ =
            ( m_, Cmd.none )

        do f =
            return (f m)

        onScene f =
            do (L.modify scene f)
    in
    case msg_ of
        NoOp ->
            return m

        OnWindowResize ->
            ( m
            , Browser.Dom.getElement "editor-scene"
                |> Task.attempt (Result.unwrap NoOp OnRescaleViewport)
            )

        OnRescaleViewport { element } ->
            let
                factor =
                    BoundingBox2d.dimensions m.scene.bbox
                        |> Tuple.first
                        |> inMeters
                        |> (*) (1 / element.width)
            in
            return { m | scale = factor }

        OnDragMsg msg ->
            Draggable.update cfg.dragConfig msg m

        OnDragBy rawDelta ->
            let
                delta =
                    rawDelta |> Vector2d.scaleBy m.scale
            in
            case m.scene.selected of
                Just ( key, [] ) ->
                    if key == -1 then
                        update cfg
                            (OnChangeViewBox "mouse-drag"
                                (\bb ->
                                    bb
                                        |> BoundingBox2d.translateBy (Vector2d.reverse delta)
                                )
                            )
                            m

                    else
                        onScene <| Scene.update key (Figure.move delta)

                Just ( key, subKey ) ->
                    onScene <| Scene.update key (cfg.innerMove subKey delta)

                _ ->
                    return m

        OnSelectFigure key subKey ->
            onScene <| \s -> { s | selected = Just ( key, subKey ) }

        OnFigureCreate fig ->
            onScene <| Scene.insert fig >> Tuple.second

        OnFigureDiscard key ->
            onScene <| Scene.discard key

        OnFigureReplace fig key ->
            onScene <| Scene.put key fig

        OnFigureChangeOrder direction key ->
            onScene <| Scene.moveLayer direction key

        OnFigureUpdate _ lazy key ->
            case ( Scene.get key m.scene, lazy () ) of
                ( Just _, Just fig ) ->
                    onScene <| Scene.put key fig

                _ ->
                    return m

        OnGroupOrderChange direction key ->
            onScene (Scene.moveGroup direction key)

        OnGroupInclude label key ->
            onScene (Scene.group key label)

        OnChangeViewBox _ fn ->
            let
                bb =
                    m.scene.bbox

                scene =
                    L.modify bbox fn m.scene

                growth =
                    Q.ratio
                        (Tuple.first (BoundingBox2d.dimensions scene.bbox))
                        (Tuple.first (BoundingBox2d.dimensions bb))
            in
            return { m | scale = m.scale * growth, scene = scene }
