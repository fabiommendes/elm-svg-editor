module Main exposing (main)

import Browser
import Config
import Editor exposing (..)
import Figure as F
import Geometry exposing (vector)
import Lens exposing (scene)
import Scene
import Shape.Any as F


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
    scene.set
        (m.scene
            |> Scene.insertMany
                [ F.image 20 "https://i.imgur.com/RyBdN56.jpeg"
                    |> F.move (vector ( 0, 0 ))
                    |> F.editable False
                    |> F.draggable False
                , F.point (vector ( 0, 0 ))
                , F.point (vector ( 5, 0 ))
                , F.point (vector ( 0, 5 ))
                , F.point (vector ( 5, 5 ))
                , F.line [ ( 1, 2 ), ( 3, 4 ), ( 5, 2 ) ]
                    |> F.setLabel "bem-vindo"
                , F.text "foo bar"
                    |> F.move (vector ( 15, 10 ))
                ]
        )
        m


config : Config
config =
    defaulConfig
        |> Config.withControls { pan = True, zoom = True, drag = True }
        |> Config.withPointRadius 0.3
