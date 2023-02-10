module Toolbars exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List.Extra as List
import Material.Icons as I
import Material.Icons.Types exposing (Coloring(..))
import Maybe.Extra as Maybe
import Msg exposing (Msg(..))
import Scene exposing (Scene)
import Types exposing (..)
import Ui
import Util exposing (flip)


removeLastItem : Scene fig -> Html (Msg a)
removeLastItem scene =
    Scene.keys scene
        |> List.last
        |> Maybe.unwrap [] (\key -> [ onClick (OnFigureDiscard key) ])
        |> flip Ui.toolbarBtn I.delete_outline
