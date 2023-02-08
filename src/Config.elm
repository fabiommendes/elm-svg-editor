module Config exposing (..)

import Draggable
import Draggable.Events as DE
import Element exposing (Element)
import Figure exposing (Figure)
import Geometry exposing (Vector, vector)
import Html exposing (Html)
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import Lens as L
import Monocle.Lens as L
import Msg exposing (Msg(..))
import Svg exposing (Svg)
import Types exposing (Key, SubKey)


type alias ViewFunction a =
    ConfigParams -> Element a -> Svg (Msg a)


type alias Config a =
    { dragConfig : Draggable.Config ( Key, SubKey ) (Msg a)
    , view : ViewFunction a
    , innerMove : SubKey -> Vector -> Figure a -> Figure a
    , actionButtons : Element a -> List (Html (Msg a))
    , shapeEncoder : a -> Value
    , shapeDecoder : Decoder a
    , groups : List Key
    , params : ConfigParams
    }


type alias ConfigParams =
    { pointRadius : Float
    , zoomControls : Bool
    , panControls : Bool
    }


init : ViewFunction a -> (a -> Value) -> Config a
init view enc =
    { dragConfig = makeDragConfig OnDragBy OnSelectFigure
    , shapeEncoder = enc
    , shapeDecoder = Json.Decode.fail "no shape decoder provided"
    , view = view
    , innerMove = \_ _ fig -> fig
    , actionButtons = \_ -> []
    , groups = []
    , params = initParams
    }


initParams : ConfigParams
initParams =
    { pointRadius = 0.5
    , zoomControls = True
    , panControls = True
    }


withInnerMove : (SubKey -> Vector -> Figure a -> Figure a) -> Config a -> Config a
withInnerMove move cfg =
    { cfg | innerMove = move }


withActionButtons : (Element a -> List (Html (Msg a))) -> Config a -> Config a
withActionButtons buttons cfg =
    { cfg | actionButtons = buttons }


withDecoder : Decoder a -> Config a -> Config a
withDecoder dec cfg =
    { cfg | shapeDecoder = dec }


withGroups : List Key -> Config a -> Config a
withGroups =
    L.groups.set


withPointRadius : Float -> Config a -> Config a
withPointRadius r =
    L.modify L.params <| \p -> { p | pointRadius = r }


withZoomControls : Bool -> Config a -> Config a
withZoomControls value =
    L.modify L.params <| \p -> { p | zoomControls = value }


withPanControls : Bool -> Config a -> Config a
withPanControls value =
    L.modify L.params <| \p -> { p | panControls = value }


makeDragConfig : (Vector -> msg) -> (Key -> SubKey -> msg) -> Draggable.Config ( Key, SubKey ) msg
makeDragConfig dragMsg selectMsg =
    Draggable.customConfig
        [ DE.onDragBy (dragMsg << vector)
        , DE.onDragStart <| \( key, subKey ) -> selectMsg key subKey
        , DE.onClick <| \( key, subKey ) -> selectMsg key subKey
        ]
