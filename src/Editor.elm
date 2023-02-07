module Editor exposing (..)

import Config
import Html exposing (Html)
import Model
import Msg
import Shape.Any exposing (Any)
import Subscriptions
import Task
import Types exposing (..)
import Update
import View


type alias Model =
    Model.Model Any


type alias Msg =
    Msg.Msg Any


type alias Config =
    Config.Config Any


init : Model
init =
    Model.init


defaulConfig : Config
defaulConfig =
    Config.init Shape.Any.view
        |> Config.withInnerMove Shape.Any.moveInside
        |> Config.withActionButtons Shape.Any.actionButtons


update : Config -> Msg -> Model -> ( Model, Cmd Msg )
update =
    Update.update


view : Config -> Model -> Html Msg
view =
    View.view


subscriptions : Model -> Sub Msg
subscriptions =
    Subscriptions.subscriptions


cmd : Cmd Msg
cmd =
    Task.perform (\() -> Msg.OnWindowResize) (Task.succeed ())
