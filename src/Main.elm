module Main exposing (main)

import Browser
import Figure as F
import Geometry exposing (vector)
import Model
import Shape as F
import State exposing (State(..))
import Svg.Editor exposing (..)
import Svg.Editor.Config


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( init |> withExample, cmd )
        , update = update config
        , view = view config
        , subscriptions = subscriptions config
        }


withExample : Model -> Model
withExample m =
    m
        |> Model.withFigures "obj"
            [ F.image 20 "https://i.imgur.com/RyBdN56.jpeg"
                |> F.move (vector ( 0, 0 ))
                |> F.editable False
            , F.line [ ( 3, 3 ), ( 5, 5 ), ( 10, 2 ), ( 5, 15 ) ]
            ]


config : Config
config =
    defaulConfig
        |> Svg.Editor.Config.controls { pan = True, zoom = True, drag = True }
        |> Svg.Editor.Config.pointRadius 0.3
