module Svg.Editor exposing
    ( Config, Model, Msg
    , defaulConfig, init, subscriptions, update, view, cmd
    )

{-| Basic types

@docs Config, Model, Msg


## Elm Archtecture

@docs defaulConfig, init, subscriptions, update, view, cmd

-}

import Config
import Decode
import Encode
import Geometry exposing (point)
import Html exposing (Html)
import Lens as L
import Model
import Msg
import Shape.Any
import Shape.Types as Shape
import Subscriptions
import Task
import Types exposing (..)
import Update
import View


{-| Editor model
-}
type alias Model =
    Model.Model Shape.Any


{-| Message
-}
type alias Msg =
    Msg.Msg Shape.Any


{-| Editor config
-}
type alias Config =
    Config.Config Shape.Any


{-| Init function in TEA
-}
init : Model
init =
    Model.init


{-| Configuration object
-}
defaulConfig : Config
defaulConfig =
    Config.init
        (Shape.Any.point (point ( 0, 0 ))
            |> L.editable.set False
        )
        |> Config.withViewFunction Shape.Any.view
        |> Config.withInnerMove Shape.Any.moveInside
        |> Config.withInnerRemove Shape.Any.removeInside
        |> Config.withActionButtons Shape.Any.actionButtons
        |> Config.withJson { encoder = Encode.shape, decoder = Decode.shape }
        |> Config.withConnector
            { connect = Shape.Any.connect
            , canConnect =
                .shape
                    >> Shape.Any.unwrap
                        { line = \_ -> False
                        , text = \_ -> False
                        , point = \_ -> True
                        , image = \_ -> False
                        }
            , end = Shape.Any.endConnection
            }


{-| Update function in TEA
-}
update : Config -> Msg -> Model -> ( Model, Cmd Msg )
update =
    Update.update


{-| View function in TEA
-}
view : Config -> Model -> Html Msg
view =
    View.view


{-| Subscription function in TEA
-}
subscriptions : Config -> Model -> Sub Msg
subscriptions =
    Subscriptions.subscriptions


{-| Command to properly initialize the editor
-}
cmd : Cmd Msg
cmd =
    Task.perform (\() -> Msg.OnWindowResize) (Task.succeed ())
