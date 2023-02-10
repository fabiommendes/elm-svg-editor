module View exposing (..)

import Attributes as A
import BaseTypes exposing (Direction(..))
import Config exposing (Config, withState)
import Element exposing (Element)
import Group exposing (GroupInfo)
import Html exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events exposing (onClick, onInput)
import Lens exposing (editable)
import Material.Icons as I
import Material.Icons.Round as IR
import Material.Icons.Types exposing (Coloring(..))
import Maybe.Extra as Maybe
import Model exposing (Model)
import Monocle.Lens as L
import Msg exposing (Msg(..))
import Scene exposing (Scene)
import State exposing (State(..))
import Svg exposing (svg)
import Svg.Attributes as SA
import Types exposing (..)
import Ui
import Util exposing (iff)


view : Config a -> Model a -> Html (Msg a)
view cfg m =
    let
        config =
            cfg |> withState m.state

        selected =
            Scene.getSelected m.scene
    in
    div
        [ class "bg-base w-100"
        , attribute "data-theme" "lemonade"
        ]
        [ navbar
        , toolbar config m
        , div [ class "max-w-xl m-auto" ] <|
            case m.error of
                Just err ->
                    [ pre [ HA.style "font-size" "0.7rem" ] [ text err ] ]

                Nothing ->
                    [ main_ [] [ viewScene config m ]
                    , Maybe.unpack notSelectedContext (contextToolbar cfg m.scene) selected
                    ]
        ]


viewScene : Config a -> Model a -> Html (Msg a)
viewScene cfg m =
    let
        onDrag =
            if cfg.params.panWithTouch then
                A.touch ( backgroundKey, [] )

            else
                []

        sceneSvg =
            Scene.view cfg m.scene
    in
    div [ class "container bg-slate-100", HA.class "scene" ]
        [ svg
            (SA.width "100%" :: A.viewBox m.scene.bbox :: onDrag)
            [ case m.state of
                ReadOnlyView ->
                    Svg.map (Msg.onDragMsgs NoOp) sceneSvg

                _ ->
                    sceneSvg
            ]
        , Ui.controls cfg
        ]


notSelectedContext : () -> Html msg
notSelectedContext _ =
    div [] [ text "no key selected" ]


contextToolbar : Config a -> Scene a -> Element a -> Html (Msg a)
contextToolbar cfg scene elem =
    let
        allowedGroups =
            [ ( "foo", "Foo" ), ( "bar", "Bar" ), ( "baz", "Baz" ) ]

        selectGroup : Html (Msg a)
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

        showGroup : GroupInfo -> Html (Msg a)
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
        [ text <| "selected key: " ++ showKey elem.key
        , elem.subKey
            |> List.map String.fromInt
            |> String.join ", "
            |> (++) "selected sub: "
            |> text
            |> List.singleton
            |> div []
        , div [] (cfg.config.actionButtons elem)
        , Maybe.unwrap selectGroup showGroup elem.group
        ]


navbar : Html msg
navbar =
    let
        icon i =
            i 20 Inherit
    in
    div
        [ class "navbar shadow-lg bg-primary primary-content"
        ]
        [ div [ class "flex-none pl-3" ] [ icon I.menu ]
        , div
            [ class "flex-1" ]
            [ a
                [ class "btn btn-ghost normal-case text-xl"
                , class "hover:text-white"
                , href "/"
                ]
                [ text "croq.app" ]
            ]
        , div [ class "flex-none px-3" ] [ icon I.more_vert ]
        ]


toolbar : Config a -> Model a -> Html (Msg a)
toolbar _ m =
    let
        genericBtn attrs i =
            button (class "btn-sm px-2" :: attrs) [ i 20 Inherit ]

        selectedFigure =
            Scene.getSelected m.scene

        selectedMsg msg =
            Maybe.unwrap (disabled True) (onClickMsg msg) selectedFigure

        updateFigureMsg msg updater =
            selectedMsg (OnFigureUpdate msg <| \_ -> selectedFigure |> Maybe.map (.model >> updater))

        onClickMsg msg { key } =
            onClick (msg key)
    in
    div [ class "shadow-lg bg-slate-900 text-white z-10" ]
        [ div [ class "p-2 flex max-w-2xl m-auto" ]
            [ div []
                [ genericBtn [] I.add_circle
                , genericBtn [ onClick OnDownloadRequest ] I.save_alt
                , genericBtn [ onClick OnUploadRequest ] I.file_open
                ]
            , div [ class "flex-1 text-slate-300 text-right px-2" ] [ text "|" ]
            , div [ class "" ]
                [ genericBtn [ selectedMsg (OnFigureChangeOrder Up) ] IR.move_up
                , genericBtn [ selectedMsg (OnFigureChangeOrder Down) ] IR.move_down
                , genericBtn [ selectedMsg OnFigureDiscard ] I.delete
                , genericBtn [ updateFigureMsg "editable.toggle" <| L.modify editable not ] I.lock
                ]
            ]
        ]
