module View exposing (..)

import Attributes as A
import BaseTypes exposing (Direction(..))
import Svg.Editor.Config exposing (Config)
import Element exposing (Element)
import Geometry exposing (BBox, point)
import Group exposing (GroupInfo)
import Html exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra.Pointer as Pointer
import Html.Extra as Html
import Lens exposing (editable)
import Material.Icons as I
import Material.Icons.Round as IR
import Material.Icons.Types exposing (Coloring(..))
import Maybe.Extra as Maybe
import Model exposing (Model)
import Monocle.Lens as L
import Msg exposing (Msg(..))
import Scene exposing (Scene)
import Shape
import Shape.Toolbar
import Shape.View
import State exposing (State(..))
import Svg as S
import Svg.Attributes as SA
import Toolbars
import Types exposing (..)
import Ui
import Util exposing (iff)


view : Config -> Model -> Html Msg
view cfg m =
    let
        selected =
            Scene.getSelected (Model.scene m)
    in
    div
        [ class "bg-base w-100"
        , attribute "data-theme" "lemonade"
        ]
        [ Ui.navbar
        , toolbar m
        , div [ class "max-w-xl m-auto" ] <|
            case m.error of
                Just err ->
                    [ pre [ HA.style "font-size" "0.7rem" ] [ text err ] ]

                Nothing ->
                    [ main_ [] [ viewScene cfg m.state m.bbox (Model.scene m) ]
                    , Maybe.unpack notSelectedContext (contextToolbar (Model.scene m)) selected
                    ]
        ]


viewScene : Config -> State -> BBox -> Scene -> Html Msg
viewScene cfg state bbox data =
    let
        elementsSvg =
            Scene.elements data
                |> List.filter (.figure >> .visible)
                |> List.map (Shape.View.view cfg)

        mapMsg f =
            List.map (S.map f)

        pointerEvents st panWithTouch =
            case ( st, panWithTouch ) of
                ( ClickToInsert _ _, _ ) ->
                    [ Pointer.onDown (.pointer >> .offsetPos >> point >> OnClickAt) ]

                ( _, True ) ->
                    A.touch ( backgroundKey, [] )

                _ ->
                    []

        supressDrag =
            mapMsg (Msg.changeDragMsgsTo Msg.NoOp)
    in
    div [ HA.class "container bg-slate-500", HA.class "scene", HA.id cfg.sceneId ]
        [ S.svg
            (SA.width "100%" :: SA.class "scene" :: A.viewBox bbox :: pointerEvents state cfg.panWithTouch)
            (case state of
                ReadOnlyView ->
                    supressDrag elementsSvg

                ClickToInsert _ _ ->
                    supressDrag elementsSvg

                _ ->
                    elementsSvg
            )
        , Ui.controls cfg
        ]


notSelectedContext : () -> Html msg
notSelectedContext _ =
    div [] [ text "no key selected" ]


contextToolbar : Scene -> Element -> Html Msg
contextToolbar scene elem =
    let
        updateFigureMsg msg updater =
            selectedMsg (OnFigureUpdate msg (\_ -> Just (updater elem.figure)))

        selectedMsg msg =
            onClick (msg elem.key)

        allowedGroups =
            [ ( "foo", "Foo" ), ( "bar", "Bar" ), ( "baz", "Baz" ) ]

        selectGroup : Html Msg
        selectGroup =
            select [ class "select w-full max-w-xs", onInput (\grp -> OnGroupInclude grp elem.key) ]
                (option [] [ text "Selecione um grupo" ]
                    :: (allowedGroups
                            |> List.map
                                (\( key, name ) ->
                                    option [ value key ] [ text name ]
                                )
                       )
                )

        showGroup : GroupInfo -> Html Msg
        showGroup { label, index } =
            let
                action direction =
                    onClick (OnGroupOrderChange direction elem.key)

                group =
                    Scene.getGroup label scene

                isLast =
                    List.length group == (index + 1)

                isFirst =
                    index == 0
            in
            div []
                [ h2 [ class "font-bold text-lg" ] [ text ("Grupo '" ++ label ++ "'") ]
                , div [ class "btn-group shadow-lg" ]
                    [ button [ class (iff isFirst "btn btn-disabled" "btn btn-active"), action Down ] [ text "-" ]
                    , button [ class "btn glass w-12 max-w-sm" ] [ text (String.fromInt index) ]
                    , button [ class (iff isLast "btn btn-disabled" "btn btn-active"), action Up ] [ text "+" ]
                    ]
                ]

        lockButtonAttrs =
            [ updateFigureMsg "editable.toggle" <| L.modify editable not ]
    in
    div []
        [ div [ class "shadow-lg bg-slate-900 text-white z-10" ]
            [ div [ class "p-2 flex max-w-2xl m-auto" ]
                [ div []
                    (Shape.Toolbar.toolbar elem)
                , div [ class "flex-1 text-slate-300 text-right px-2" ] [ text "" ]
                , div [ class "" ]
                    [ Ui.toolbarBtn [ selectedMsg (OnFigureChangeOrder Up) ] IR.move_up
                    , Ui.toolbarBtn [ selectedMsg (OnFigureChangeOrder Down) ] IR.move_down
                    , Ui.toolbarBtn [ selectedMsg OnFigureDiscard ] I.delete
                    , Ui.selectedToolbarBtn (not elem.figure.editable) lockButtonAttrs I.lock
                    ]
                ]
            ]
        , elem.subKey
            |> List.map String.fromInt
            |> String.join ", "
            |> (++) "selected sub: "
            |> text
            |> List.singleton
            |> div []
        , Maybe.unwrap selectGroup showGroup elem.group
        ]


toolbar : Model -> Html Msg
toolbar m =
    let
        insertFigure pt =
            Shape.point pt

        stateBtn st icon =
            Ui.selectedToolbarBtn (m.state |> State.isSimilarTo st) [ onClick (OnStateChange st) ] icon
    in
    div [ class "shadow-lg bg-slate-900 text-white z-10" ]
        [ div [ class "p-2 flex max-w-2xl m-auto" ]
            [ div [ class "btn-group" ]
                [ stateBtn State.StandardEditor I.edit
                , stateBtn (State.ClickToInsert "ref" insertFigure) I.add_circle_outline
                , stateBtn (State.Connecting Nothing) I.timeline
                , stateBtn State.ReadOnlyView I.landscape
                ]
            , div [ class "flex-1 text-slate-300 text-right px-2" ] [ text "" ]
            , div [ class "btn-group" ]
                [ Ui.toolbarBtn [ onClick OnUndo ] I.undo
                , Ui.toolbarBtn [ onClick OnRedo ] I.redo
                ]
            , div [ class "text-slate-300 text-right px-2" ] [ text "" ]
            , div [ class "btn-group" ]
                [ if State.isClickToInsert m.state then
                    Toolbars.removeLastItem (Model.scene m)

                  else
                    Html.nothing
                , Ui.toolbarBtn [ onClick OnDownloadRequest ] I.save_alt
                , Ui.toolbarBtn [ onClick OnUploadRequest ] I.file_open
                ]
            ]
        ]
