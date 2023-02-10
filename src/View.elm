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
        [ Ui.navbar
        , toolbar config m
        , div [ class "max-w-xl m-auto" ] <|
            case m.error of
                Just err ->
                    [ pre [ HA.style "font-size" "0.7rem" ] [ text err ] ]

                Nothing ->
                    [ main_ [] [ Scene.view config m.scene ]
                    , Maybe.unpack notSelectedContext (contextToolbar cfg m.scene) selected
                    ]
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


toolbar : Config a -> Model a -> Html (Msg a)
toolbar cfg m =
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

        insertFigure pt =
            cfg.config.defaultFigure |> Figure.move (vector (fromPoint pt))
    in
    div [ class "shadow-lg bg-slate-900 text-white z-10" ]
        [ div [ class "p-2 flex max-w-2xl m-auto" ]
            [ div []
                [ genericBtn [ onClick OnDownloadRequest ] I.save_alt
                , genericBtn [ onClick OnUploadRequest ] I.file_open
                ]
            , div [ class "flex-1 text-slate-300 text-right px-2" ] [ text "|" ]
            , div []
                [ genericBtn [ onClick (OnStateChange <| State.StandardEditor) ] I.edit
                , genericBtn [ onClick (OnStateChange <| State.ClickToInsert "ref" insertFigure) ] I.add_circle_outline
                , genericBtn [ onClick (OnStateChange <| State.ConnectingLines ("obj", 5)) ] I.timeline
                , genericBtn [ onClick (OnStateChange <| State.ReadOnlyView) ] I.landscape
                ]
            , div [ class "text-slate-300 text-right px-2" ] [ text "|" ]
            , div [ class "" ]
                [ genericBtn [ selectedMsg (OnFigureChangeOrder Up) ] IR.move_up
                , genericBtn [ selectedMsg (OnFigureChangeOrder Down) ] IR.move_down
                , genericBtn [ selectedMsg OnFigureDiscard ] I.delete
                , genericBtn [ updateFigureMsg "editable.toggle" <| L.modify editable not ] I.lock
                ]
            ]
        ]
