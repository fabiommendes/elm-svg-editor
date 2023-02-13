module Update exposing (..)

import BaseTypes exposing (Direction(..))
import BoundingBox2d
import Browser.Dom
import Config exposing (Config)
import Decode
import Draggable
import Element as E
import Encode
import Figure
import File
import File.Download
import File.Select
import Json.Decode
import Json.Encode
import Length exposing (inMeters)
import Lens exposing (scene)
import Maybe.Extra as Maybe
import Model as M exposing (Model)
import Msg exposing (KeyBoardCommands(..), Msg(..))
import Scene as S
import State exposing (State(..))
import Task
import Types exposing (..)
import Vector2d


update : Config a -> Msg a -> Model a -> ( Model a, Cmd (Msg a) )
update cfg msg_ m =
    update_ cfg msg_ m.state (M.trimHistory m)


update_ : Config a -> Msg a -> State a -> Model a -> ( Model a, Cmd (Msg a) )
update_ cfg msg_ state_ m =
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
    case ( msg_, state_ ) of
        ( NoOp, _ ) ->
            return m

        ( Batch msgs, _ ) ->
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

        ( OnWindowResize, _ ) ->
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

        ( OnErrorDetected s1 s2, _ ) ->
            return { m | error = Just (s1 ++ ": " ++ s2) }

        ( OnViewportRescaled { element }, _ ) ->
            let
                factor =
                    BoundingBox2d.dimensions m.bbox
                        |> Tuple.first
                        |> inMeters
                        |> (*) (1 / element.width)
            in
            return { m | scale = factor }

        ( OnDragMsg msg, _ ) ->
            Draggable.update cfg.config.drag msg m

        ( OnDragBy rawDelta, _ ) ->
            let
                delta =
                    rawDelta |> Vector2d.scaleBy m.scale
            in
            case m |> M.onScene S.selectedFullKey of
                Just ( key, [] ) ->
                    if key == backgroundKey && m.state == State.ReadOnlyView then
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

        ( OnSelectFigure key subKey, Connecting (Just k) ) ->
            if k == key then
                return m

            else
                case ( S.getElement k (M.scene m), S.getElement key (M.scene m) ) of
                    ( Just target, Just dest ) ->
                        case cfg.config.connection.connect target dest of
                            Just changed ->
                                onScene (S.put k changed >> S.select ( k, [] ))

                            _ ->
                                return m

                    _ ->
                        m
                            |> M.updateScene (S.select ( key, subKey ))
                            |> return

        ( OnSelectFigure key _, Connecting Nothing ) ->
            let
                scene : E.Element a -> Maybe (S.Scene a)
                scene elem =
                    let
                        target =
                            { elem | model = elem.model |> Figure.editable True }
                    in
                    if key == backgroundKey then
                        Nothing

                    else if cfg.config.connection.canConnect elem then
                        Just <| (S.insert target.model (M.scene m) |> (\( k, scn ) -> S.select ( k, [] ) scn))

                    else
                        Nothing
            in
            case S.getElement key (M.scene m) |> Maybe.andThen scene of
                Just scn ->
                    m
                        |> M.pushScene scn
                        |> M.withState (Connecting (S.selectedKey scn))
                        |> return

                _ ->
                    return m

        ( OnSelectFigure key subKey, _ ) ->
            onScene <| S.select ( key, subKey )

        ( OnFigureCreate fig, _ ) ->
            onScene <| S.insert fig >> Tuple.second

        ( OnFigureDiscard key, _ ) ->
            onScene <| S.discard key

        ( OnFigureReplace fig key, _ ) ->
            onScene <| S.put key fig

        ( OnFigureChangeOrder direction key, _ ) ->
            onScene <| S.moveLayer direction key

        ( OnFigureUpdate _ lazy key, _ ) ->
            case ( m |> M.onScene (S.get key), lazy () ) of
                ( Just _, Just fig ) ->
                    onScene <| S.put key fig

                _ ->
                    return m

        ( OnGroupOrderChange direction key, _ ) ->
            onScene (S.moveGroup direction key)

        ( OnGroupInclude label key, _ ) ->
            onScene (S.group key label)

        ( OnChangeViewBox _ fn, _ ) ->
            return (m |> M.focusTo (fn m.bbox))

        ( OnDownloadRequest, _ ) ->
            let
                data =
                    Json.Encode.encode 2 (Encode.scene cfg.config.shapeEncoder (M.scene m))
            in
            ( m, File.Download.string "data.json" "application/json" data )

        ( OnUploadRequest, _ ) ->
            ( m, File.Select.file [ "application/json" ] OnUploadCompleted )

        ( OnUploadCompleted file, _ ) ->
            ( m, File.toString file |> Task.perform OnUploadProcessed )

        ( OnUploadProcessed st, _ ) ->
            case Json.Decode.decodeString (Decode.scene cfg.config.shapeDecoder) st of
                Ok scene ->
                    m
                        |> M.pushScene scene
                        |> M.clearError
                        |> return

                Err err ->
                    return { m | error = Just <| Json.Decode.errorToString err }

        ( OnClickAt pt, _ ) ->
            return (M.notifyClick pt m)

        ( OnStateChange st, _ ) ->
            return (m |> M.changeState cfg st)

        ( OnKeyPress Delete, _ ) ->
            case m |> M.onScene S.selectedKey of
                Just key ->
                    update cfg (OnFigureDiscard key) m

                Nothing ->
                    return m

        ( OnKeyPress Undo, _ ) ->
            update cfg OnUndo m

        ( OnKeyPress Redo, _ ) ->
            update cfg OnRedo m

        ( OnUndo, _ ) ->
            return (M.undo m)

        ( OnRedo, _ ) ->
            return (M.redo m)
