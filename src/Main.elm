module Main exposing (main)

import Browser
import Editor exposing (..)
import Figure as F
import Geometry exposing (line, vector)
import Lens exposing (scene)
import Scene
import Shape.Any as F


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( init |> withExample, cmd )
        , update = update defaulConfig
        , view = view defaulConfig
        , subscriptions = subscriptions
        }


withExample : Model -> Model
withExample m =
    scene.set
        (m.scene
            |> Scene.insertMany
                [ F.point (vector ( 0, 0 )) |> F.grow 2
                , F.point (vector ( 5, 0 ))
                , F.point (vector ( 0, 5 ))
                , F.point (vector ( 5, 5 )) |> F.grow 3
                , F.line (line [ ( 1, 2 ), ( 3, 4 ), ( 5, 2 ) ])

                -- , newModel (TextModel "foo bar")
                --     |> move (vector ( 15, 10 ))
                --     |> grow 0.75
                -- , newModel (ImageModel { href = "img" })
                --     |> move (vector ( 10, 15 ))
                --     |> grow 0.75
                ]
        )
        m
