module Main exposing (main)

import Browser
import Html exposing (Html, a, div)
import Html.Attributes exposing (attribute, class, href)
import Material.Icons as I
import Material.Icons.Types exposing (Coloring(..))
import Svg.Editor exposing (..)
import Svg.Editor.Config


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( init config |> withExample, cmd )
        , update = update config
        , view = viewApp config
        , subscriptions = subscriptions config
        }


withExample : Model -> Model
withExample m =
    m
        |> withFigures "obj"
            [ image 15 "https://i.imgur.com/RyBdN56.jpeg"
                |> move ( 1, 1 )
                |> editable False
            , line [ ( 3, 3 ), ( 5, 5 ), ( 10, 2 ), ( 5, 15 ) ]
            ]


config : Config
config =
    defaulConfig
        |> Svg.Editor.Config.controls { pan = True, zoom = True, drag = False }
        |> Svg.Editor.Config.pointRadius 0.25
        |> Svg.Editor.Config.editable True
        |> Svg.Editor.Config.shape ( 20, 12 )


viewApp : Config -> Model -> Html Msg
viewApp cfg m =
    div
        [ class "bg-base w-100"
        , attribute "data-theme" "lemonade"
        ]
        [ navbar, div [ class "max-w-xl m-auto" ] [ view cfg m ] ]


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
                [ Html.text "croq.app" ]
            ]
        , div [ class "flex-none px-3" ] [ icon I.more_vert ]
        ]
