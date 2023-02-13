module Main exposing (main)

import Browser
import Config
import Editor exposing (..)
import Figure as F
import Geometry exposing (vector)
import Model
import Shape.Any as F
import State exposing (State(..))


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( init |> withExample, cmd )
        , update = update config
        , view = view config
        , subscriptions = subscriptions
        }


withExample : Model -> Model
withExample m =
    m
        |> Model.withFigures "obj"
            [ F.image 20 "https://i.imgur.com/RyBdN56.jpeg"
                |> F.move (vector ( 0, 0 ))
                |> F.editable False

                , F.line [(3, 3), (5, 5), (10, 2), (5, 15)]
            ]


config : Config
config =
    defaulConfig
        |> Config.withControls { pan = True, zoom = True, drag = True }
        |> Config.withPointRadius 0.3
