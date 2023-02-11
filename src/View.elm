module View exposing (..)

import BaseTypes exposing (Direction(..))
import Config exposing (Config, withState)
import Element exposing (Element)
import Figure
import Geometry exposing (fromPoint, vector)
import Group exposing (GroupInfo)
import Html exposing (..)
import Html.Attributes as HA exposing (..)
import Html.Events exposing (onClick, onInput)
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
import State exposing (State(..))
import Toolbars
import Types exposing (..)
import Ui
import Util exposing (iff)


view : Config a -> Model a -> Html (Msg a)
view cfg m =
    let
        config =
            cfg |> withState m.state

        selected =
            Scene.getSelected (Model.scene m)
    in
    div
        [ class "bg-base w-100"
        , attribute "data-theme" "lemonade"
        ]
        [ Ui.navbar
        , toolbar config m
        , div [ class "max-w-xl m-auto" ] <|
            case m.error of
                Just err ->
                    [ pre [ HA.style "font-size" "0.7rem" ] [ text err ] ]

                Nothing ->
                    [ main_ [] [ Scene.view config m.bbox (Model.scene m) ]
                    , Maybe.unpack notSelectedContext (contextToolbar cfg (Model.scene m)) selected
                    ]
        ]


notSelectedContext : () -> Html msg
notSelectedContext _ =
    div [] [ text "no key selected" ]


contextToolbar : Config a -> Scene a -> Element a -> Html (Msg a)
contextToolbar cfg scene elem =
    let
        updateFigureMsg msg updater =
            selectedMsg (OnFigureUpdate msg (\_ -> Just (updater elem.model)))

        selectedMsg msg =
            onClick (msg elem.key)

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

        lockButtonAttrs =
            (updateFigureMsg "editable.toggle" <| L.modify editable not) :: iff elem.model.editable [] [ HA.class "btn-active" ]
    in
    div []
        [ div [ class "shadow-lg bg-slate-900 text-white z-10" ]
            [ div [ class "p-2 flex max-w-2xl m-auto" ]
                [ div []
                    [ text <| "selected key: " ++ showKey elem.key
                    ]
                , div [ class "flex-1 text-slate-300 text-right px-2" ] [ text "" ]
                , div [ class "" ]
                    [ Ui.toolbarBtn [ selectedMsg (OnFigureChangeOrder Up) ] IR.move_up
                    , Ui.toolbarBtn [ selectedMsg (OnFigureChangeOrder Down) ] IR.move_down
                    , Ui.toolbarBtn [ selectedMsg OnFigureDiscard ] I.delete
                    , Ui.toolbarBtn lockButtonAttrs I.lock
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
        , div [] (cfg.config.actionButtons elem)
        , Maybe.unwrap selectGroup showGroup elem.group
        ]


toolbar : Config a -> Model a -> Html (Msg a)
toolbar cfg m =
    let
        insertFigure pt =
            cfg.config.defaultFigure |> Figure.move (vector (fromPoint pt))

        stateMsg msg =
            onClick (OnStateChange msg)
                :: (if m.state |> State.isSimilarTo msg then
                        [ class "glass" ]

                    else
                        [ class "text-info" ]
                   )
    in
    div [ class "shadow-lg bg-slate-900 text-white z-10" ]
        [ div [ class "p-2 flex max-w-2xl m-auto" ]
            [ div []
                [ Ui.toolbarBtn (stateMsg <| State.StandardEditor) I.edit
                , Ui.toolbarBtn (stateMsg <| State.ClickToInsert "ref" insertFigure) I.add_circle_outline
                , Ui.toolbarBtn (stateMsg <| State.Connecting Nothing) I.timeline
                , Ui.toolbarBtn (stateMsg <| State.ReadOnlyView) I.landscape
                ]
            , div [ class "flex-1 text-slate-300 text-right px-2" ] [ text "" ]
            , div []
                [ Ui.toolbarBtn [ onClick OnUndo ] I.undo
                , Ui.toolbarBtn [ onClick OnRedo ] I.redo
                ]
            , div [ class "text-slate-300 text-right px-2" ] [ text "" ]
            , div []
                [ if State.isClickToInsert m.state then
                    Toolbars.removeLastItem (Model.scene m)

                  else
                    Html.nothing
                , Ui.toolbarBtn [ onClick OnDownloadRequest ] I.save_alt
                , Ui.toolbarBtn [ onClick OnUploadRequest ] I.file_open
                ]
            ]
        ]
