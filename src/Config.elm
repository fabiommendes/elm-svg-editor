module Config exposing (..)

import Draggable
import Draggable.Events as DE
import Element exposing (Element)
import Figure exposing (Figure)
import Geometry exposing (Vector, vector)
import Html exposing (Html)
import Lens as L
import Msg exposing (Msg(..))
import Svg exposing (Svg)
import Types exposing (Key, SubKey)


type alias Config a =
    { pointRadius : Float
    , dragConfig : Draggable.Config ( Key, SubKey ) (Msg a)
    , view : Element a -> Svg (Msg a)
    , innerMove : SubKey -> Vector -> Figure a -> Figure a
    , actionButtons : Element a -> List (Html (Msg a))
    , groups : List Key
    }


init : (Element a -> Svg (Msg a)) -> Config a
init view =
    { pointRadius = 0.5
    , dragConfig = makeDragConfig OnDragBy OnSelectFigure
    , view = view
    , innerMove = \_ _ fig -> fig
    , actionButtons = \_ -> []
    , groups = []
    }


withInnerMove : (SubKey -> Vector -> Figure a -> Figure a) -> Config a -> Config a
withInnerMove move cfg =
    { cfg | innerMove = move }


withActionButtons : (Element a -> List (Html (Msg a))) -> Config a -> Config a
withActionButtons buttons cfg =
    { cfg | actionButtons = buttons }


withGroups : List Key -> Config a -> Config a
withGroups =
    L.groups.set


makeDragConfig : (Vector -> msg) -> (Key -> SubKey -> msg) -> Draggable.Config ( Key, SubKey ) msg
makeDragConfig dragMsg selectMsg =
    Draggable.customConfig
        [ DE.onDragBy (dragMsg << vector)
        , DE.onDragStart <| \( key, subKey ) -> selectMsg key subKey
        , DE.onClick <| \( key, subKey ) -> selectMsg key subKey
        ]
