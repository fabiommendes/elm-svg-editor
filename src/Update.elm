module Update exposing (..)

import BaseTypes exposing (Direction(..))
import BoundingBox2d
import Browser.Dom
import Config exposing (Config)
import Decode
import Draggable
import Encode
import Figure
import File
import File.Download
import File.Select
import Json.Decode
import Json.Encode
import Length exposing (inMeters)
import Lens exposing (bbox, scene)
import Model exposing (Model)
import Monocle.Lens as L
import Msg exposing (Msg(..))
import Quantity as Q
import Scene
import State exposing (State(..))
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

        Batch msgs ->
            let
                reducer msg ( m_, cmds ) =
                    let
                        ( mNext, cmd ) =
                            update cfg msg m_
                    in
                    ( mNext, cmd :: cmds )
            in
            let
                ( m_, cmds ) =
                    List.foldl reducer ( m, [] ) msgs
            in
            ( m_, Cmd.batch cmds )

        OnWindowResize ->
            ( m
            , Browser.Dom.getElement cfg.params.sceneId
                |> Task.attempt
                    (\res ->
                        case res of
                            Ok elem ->
                                OnViewportRescaled elem

                            Err _ ->
                                OnErrorDetected "resize" "ERROR"
                    )
            )

        OnErrorDetected s1 s2 ->
            return { m | error = Just (s1 ++ ": " ++ s2) }

        OnViewportRescaled { element } ->
            let
                factor =
                    BoundingBox2d.dimensions m.scene.bbox
                        |> Tuple.first
                        |> inMeters
                        |> (*) (1 / element.width)
            in
            return { m | scale = factor }

        OnDragMsg msg ->
            Draggable.update cfg.config.drag msg m

        OnDragBy rawDelta ->
            let
                delta =
                    rawDelta |> Vector2d.scaleBy m.scale
            in
            case m.scene.selected of
                Just ( key, [] ) ->
                    if key == backgroundKey then
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
                    onScene <| Scene.update key (cfg.config.innerMove subKey delta)

                _ ->
                    return m

        OnSelectFigure key subKey ->
            let
                select k s =
                    { s | selected = Just k }
            in
            case m.state of
                Connecting (Just k) ->
                    if k == key then
                        return m

                    else
                        case ( Scene.get k m.scene, Scene.get key m.scene ) of
                            ( Just target, Just dest ) ->
                                let
                                    ( target_, dest_ ) =
                                        cfg.config.connectFigures target dest
                                in
                                onScene (Scene.put k target_ >> Scene.put key dest_ >> select ( k, [] ))

                            _ ->
                                return { m | scene = select ( key, subKey ) m.scene, state = Connecting Nothing }

                Connecting Nothing ->
                    let
                        target =
                            Scene.get key m.scene
                                |> Maybe.withDefault cfg.config.defaultTarget

                        ( k, scene ) =
                            Scene.insert target m.scene

                        msg =
                            OnSelectFigure key subKey

                        model =
                            { m | scene = scene |> select ( k, [] ), state = Connecting (Just k) }
                    in
                    update cfg msg model

                _ ->
                    onScene <| select ( key, subKey )

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

                newScale =
                    m.scale * growth
            in
            return { m | scale = newScale, scene = scene }

        OnDownloadRequest ->
            let
                data =
                    Json.Encode.encode 2 (Encode.scene cfg.config.shapeEncoder m.scene)
            in
            ( m, File.Download.string "data.json" "application/json" data )

        OnUploadRequest ->
            ( m, File.Select.file [ "application/json" ] OnUploadCompleted )

        OnUploadCompleted file ->
            ( m, File.toString file |> Task.perform OnUploadProcessed )

        OnUploadProcessed st ->
            case Json.Decode.decodeString (Decode.scene cfg.config.shapeDecoder) st of
                Ok scene ->
                    return { m | scene = scene, error = Nothing }

                Err err ->
                    return { m | error = Just <| Json.Decode.errorToString err }

        OnClickAt pt ->
            return (Model.notifyClick pt m)

        OnStateChange st ->
            return { m | state = st }
