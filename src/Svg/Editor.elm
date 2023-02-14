module Svg.Editor exposing
    ( Config, Model, Msg
    , defaulConfig, init, subscriptions, update, view, cmd
    )

{-| Basic types

@docs Config, Model, Msg


## Elm Archtecture

@docs defaulConfig, init, subscriptions, update, view, cmd

-}

import Html exposing (Html)
import Model
import Msg
import Subscriptions
import Svg.Editor.Config as Config
import Task
import Types exposing (..)
import Update
import View


{-| Editor model
-}
type alias Model =
    Model.Model


{-| Message
-}
type alias Msg =
    Msg.Msg


{-| Editor config
-}
type alias Config =
    Config.Config


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
