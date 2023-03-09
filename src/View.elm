module View exposing (..)

import Attributes as A
import BaseTypes exposing (Direction(..))
import Element exposing (Element)
import Geometry exposing (BBox, point)
import Group exposing (GroupInfo)
import Html exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Events.Extra.Pointer as Pointer
import Html.Extra as Html
import Internal.Types exposing (Config(..))
import Material.Icons as I
import Material.Icons.Round as IR
import Material.Icons.Types exposing (Coloring(..))
import Maybe.Extra as Maybe
import Model exposing (Model)
import Msg exposing (Msg(..))
import Scene exposing (Scene)
import Shape
import Shape.Toolbar
import Shape.View
import State exposing (State(..))
import String.Interpolate
import Svg as S
import Svg.Attributes as SA
import Toolbars
import Types exposing (..)
import Ui
import Util exposing (iff)


sceneStyleVariables : Config -> String
sceneStyleVariables (Cfg cfg) =
    String.Interpolate.interpolate """
:scope .scene {
    --font-size: {0};
    --figure-color: {1};
    --figure-color-contrast: {2};
    --figure-color-selected: {3};
    --line-width: {4};
}
:scope .selected {
    --line-width: {5};
}
"""
        [ cfg.styleFontSize |> String.fromFloat
        , cfg.stylePrimaryColor
        , cfg.styleConstrastColor
        , cfg.styleSelectedColor
        , cfg.styleLineWidth |> String.fromFloat
        , cfg.styleLineWidthSelected |> String.fromFloat
        ]


sceneStyleFixed : () -> String
sceneStyleFixed _ =
    """
/* GENERIC AND SEMANTIC STYLES */
:scope .scene {
    font-size: var(--font-size);
}
:scope .scene path {
    stroke-linecap: round;
    stroke-linejoin: round;
}
:scope .selected {
    --figure-color: var(--figure-color-selected);
}
:scope .closed {
    fill: var(--figure-color);
}


/* LINES */
:scope .line {
    fill: none;
    stroke: var(--figure-color);
    stroke-width: var(--line-width);
}
:scope .line path.background,
:scope .line.background {
    stroke: var(--figure-color-contrast);
    filter: blur(0.05px);
    stroke-width: calc(var(--line-width) * 1.75);
}


/* POINTS */
:scope .point {
    fill: var(--figure-color);
    stroke: var(--figure-color-contrast);
    stroke-width: calc(var(--line-width) / 3);
    transition: fill stroke-width stroke-opacity 250ms;
}


/* POINTS INSIDE LINES */
:scope .line .point {
    fill-opacity: 0.6;
    filter: brightness(125%)
}
:scope .line:not(.selected) .point {
    opacity: 0.0;
}
:scope .line.selected .point {
    transition: opacity 250ms, filter 250ms;
    transition-delay: 400ms, 0ms;
}
:scope .line.selected .point.selected-child {
    fill-opacity: 1 !important;
    transition-delay: 0s;
    filter: brightness(2) blur(0);
}


/* POINT SELECTION ANIMATION */
.line.selected .point:nth-of-type(1) { transition-delay: 0ms, 0ms; }
.line.selected .point:nth-of-type(2) { transition-delay: 100ms, 0ms; }
.line.selected .point:nth-of-type(3) { transition-delay: 200ms, 0ms; }
.line.selected .point:nth-of-type(4) { transition-delay: 300ms, 0ms; }

/* LABELS */
:scope .group-label,
:scope .group-index,
:scope .label {
    fill: var(--figure-color-contrast);
    stroke: none;
    font-weight: bold;
}
:scope .group-index {
    text-anchor: middle;
    transform: translate(0, 0.15px);
}
:scope .label {
    transform: translate(0.0px, 0.55px);
    fill: var(--figure-color) !important;
}
"""


view : Config -> Model -> Html Msg
view (Cfg cfg) m =
    let
        selected =
            Scene.getSelected (Model.scene m)

        ( topBar, bottomBar ) =
            if cfg.edit then
                ( toolbar m
                , Maybe.unpack notSelectedContext (contextToolbar (Model.scene m)) selected
                )

            else
                ( Html.nothing, Html.nothing )
    in
    div [] <|
        case m.error of
            Just err ->
                [ topBar
                , pre [ HA.style "font-size" "0.7rem" ] [ text err ]
                ]

            Nothing ->
                [ topBar
                , main_ [] [ viewScene (Cfg cfg) m.state m.bbox (Model.scene m) ]
                , bottomBar
                ]


viewScene : Config -> State -> BBox -> Scene -> Html Msg
viewScene (Cfg cfg) state bbox data =
    let
        elementsSvg =
            Scene.elements data
                |> List.map (Shape.View.view (Cfg cfg))

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
        [ node "style"
            []
            [ text (sceneStyleVariables (Cfg cfg))
            , text (sceneStyleFixed ())
            ]
        , S.svg
            (SA.width "100%" :: SA.class "scene" :: A.viewBox bbox :: pointerEvents state cfg.panWithTouch)
            (case state of
                ReadOnlyView ->
                    supressDrag elementsSvg

                ClickToInsert _ _ ->
                    supressDrag elementsSvg

                _ ->
                    elementsSvg
            )
        , Ui.controls (Cfg cfg)
        ]


notSelectedContext : () -> Html msg
notSelectedContext _ =
    div [] [ text "no key selected" ]


contextToolbar : Scene -> Element -> Html Msg
contextToolbar scene elem =
    let
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
