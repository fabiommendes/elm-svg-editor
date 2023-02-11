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
import Lens exposing (scene)
import Model as M exposing (Model)
import Msg exposing (KeyBoardCommands(..), Msg(..))
import Scene as S
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
            do (M.updateScene f)

        onSceneTransform f =
            do (M.transformScene f)
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
                    BoundingBox2d.dimensions m.bbox
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
            case m |> M.onScene S.selected of
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
                        onSceneTransform <| S.update key (Figure.move delta)

                Just ( key, subKey ) ->
                    onSceneTransform <| S.update key (cfg.config.innerMove subKey delta)

                _ ->
                    return m

        OnSelectFigure key subKey ->
            case m.state of
                Connecting (Just k) ->
                    if k == key then
                        return m

                    else
                        case ( S.get k (M.scene m), S.get key (M.scene m) ) of
                            ( Just target, Just dest ) ->
                                let
                                    ( target_, dest_ ) =
                                        cfg.config.connectFigures target dest
                                in
                                onScene (S.put k target_ >> S.put key dest_ >> S.select ( k, [] ))

                            _ ->
                                m
                                    |> M.updateScene (S.select ( key, subKey ))
                                    |> M.withState (Connecting Nothing)
                                    |> return

                Connecting Nothing ->
                    let
                        target =
                            S.get key (M.scene m)
                                |> Maybe.withDefault cfg.config.defaultTarget

                        ( k, scene_ ) =
                            S.insert target (M.scene m)

                        scene =
                            S.select ( k, [] ) scene_

                        msg =
                            OnSelectFigure key subKey

                        model =
                            m
                                |> M.pushScene scene
                                |> M.withState (Connecting (Just k))
                    in
                    update cfg msg model

                _ ->
                    onScene <| S.select ( key, subKey )

        OnFigureCreate fig ->
            onScene <| S.insert fig >> Tuple.second

        OnFigureDiscard key ->
            onScene <| S.discard key

        OnFigureReplace fig key ->
            onScene <| S.put key fig

        OnFigureChangeOrder direction key ->
            onScene <| S.moveLayer direction key

        OnFigureUpdate _ lazy key ->
            case ( m |> M.onScene (S.get key), lazy () ) of
                ( Just _, Just fig ) ->
                    onScene <| S.put key fig

                _ ->
                    return m

        OnGroupOrderChange direction key ->
            onScene (S.moveGroup direction key)

        OnGroupInclude label key ->
            onScene (S.group key label)

        OnChangeViewBox _ fn ->
            return (m |> M.focusTo (fn m.bbox))

        OnDownloadRequest ->
            let
                data =
                    Json.Encode.encode 2 (Encode.scene cfg.config.shapeEncoder (M.scene m))
            in
            ( m, File.Download.string "data.json" "application/json" data )

        OnUploadRequest ->
            ( m, File.Select.file [ "application/json" ] OnUploadCompleted )

        OnUploadCompleted file ->
            ( m, File.toString file |> Task.perform OnUploadProcessed )

        OnUploadProcessed st ->
            case Json.Decode.decodeString (Decode.scene cfg.config.shapeDecoder) st of
                Ok scene ->
                    m
                        |> M.pushScene scene
                        |> M.clearError
                        |> return

                Err err ->
                    return { m | error = Just <| Json.Decode.errorToString err }

        OnClickAt pt ->
            return (M.notifyClick pt m)

        OnStateChange st ->
            return { m | state = st }

        OnKeyPress Delete ->
            case m |> M.onScene S.selected of
                Just ( key, _ ) ->
                    update cfg (OnFigureDiscard key) m

                Nothing ->
                    return m

        OnUndo ->
            return (M.undo m)

        OnRedo ->
            return (M.redo m)
