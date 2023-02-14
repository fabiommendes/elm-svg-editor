module Ui exposing (..)

import BoundingBox2d as BBox
import Svg.Editor.Config exposing (Config)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Extra as Html
import Material.Icons as I
import Material.Icons.Types exposing (Coloring(..))
import Msg exposing (Msg(..))
import Types exposing (..)


{-| Zoom and pan controls
-}
controls : Config -> Html Msg
controls cfg =
    div [ class "relative" ]
        [ if cfg.zoomControls then
            zoomButtons

          else
            Html.nothing
        , if cfg.panControls then
            panButtons

          else
            Html.nothing
        ]


{-| Create buttons for zoom in and zoom out.

Must be placed inside a container with `position: relative` (or class=relative, in tailwindcss)

-}
zoomButtons : Html Msg
zoomButtons =
    let
        btn attrs i =
            div
                (class "bg-white rounded-full w-6 h-6 m-1 p1 shadow-lg text-lg text-center font-bold" :: attrs)
                [ div [ class "m-auto" ] [ i 24 Inherit ] ]

        zoomBy factor bb =
            BBox.scaleAbout (BBox.centerPoint bb) (1 / factor) bb

        zoomStep =
            1.25
    in
    div [ class "absolute left-1 bottom-2" ]
        [ btn [ onClick <| OnChangeViewBox "zoom.in" (zoomBy zoomStep) ] I.add
        , btn [ onClick <| OnChangeViewBox "zoom.out" (zoomBy (1 / zoomStep)) ] I.remove
        ]


{-| Display arrow keys to control panning of scene.

Must be placed inside a container with `position: relative` (or class=relative, in tailwindcss)

-}
panButtons : Html Msg
panButtons =
    let
        btn attrs i =
            div
                (class "bg-white rounded-lg w-6 h-6 m-1 p1 shadow-lg text-lg text-center font-bold" :: attrs)
                [ div [ class "m-auto" ] [ i 24 Inherit ] ]
    in
    div [ class "absolute right-0 bottom-0" ]
        [ div [ class "relative w-24 h-24" ]
            [ btn [ onClick Msg.panUp, class "absolute top-1 right-8" ] I.keyboard_arrow_up
            , btn [ onClick Msg.panDown, class "absolute bottom-1 right-8" ] I.keyboard_arrow_down
            , btn [ onClick Msg.panRight, class "absolute right-1 top-8" ] I.keyboard_arrow_right
            , btn [ onClick Msg.panLeft, class "absolute left-1 top-8" ] I.keyboard_arrow_left
            ]
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


toolbarBtn : List (Attribute msg) -> (number -> Coloring -> Html msg) -> Html msg
toolbarBtn attrs i =
    button (class "btn-sm px-2 w-9 hover:btn-accent focus:btn-ghost no-animation" :: attrs) [ i 20 Inherit ]


selectedToolbarBtn : Bool -> List (Attribute msg) -> (number -> Coloring -> Html msg) -> Html msg
selectedToolbarBtn selected attrs i =
    if selected then
        toolbarBtn (class "glass focus:border" :: attrs) i

    else
        toolbarBtn attrs i
