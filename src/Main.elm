module Main exposing (main)

import Browser
import Config
import Editor exposing (..)
import Figure as F
import Geometry exposing (point, vector)
import Lens exposing (scene)
import Model exposing (withState)
import Scene
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
        |> scene.set
            (m.scene
                |> Scene.insertManyAs "obj"
                    [ F.image 20 "https://i.imgur.com/RyBdN56.jpeg"
                        |> F.move (vector ( 0, 0 ))

                    -- |> F.editable False
                    -- |> F.draggable False
                    , F.point (point ( 0, 0 ))
                    , F.point (point ( 5, 0 ))
                    ]
                |> Scene.insertManyAs "obj"
                    [ F.point (point ( 0, 5 ))
                    , F.point (point ( 5, 5 ))
                    , F.line [ ( 1, 2 ), ( 3, 4 ), ( 5, 2 ) ]
                        |> F.setLabel "bem-vindo"
                    , F.line [ ( 1, 2 ), ( 2, 2 ), ( 1, 2 ), ( 1, 5 ) ]
                        |> F.setLabel "bar"
                        |> F.move (vector ( 10, 2 ))

                    -- , F.text "foo bar"
                    --     |> F.move (vector ( 15, 10 ))
                    ]
            )
        |> withState (ClickToInsert "point" F.point)


config : Config
config =
    defaulConfig
        |> Config.withControls { pan = True, zoom = True, drag = True }
        |> Config.withPointRadius 0.3
