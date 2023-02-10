module Config exposing
    ( Config
    , Params
    , ViewFunction
    , init
    , makeDragConfig
    , withActionButtons
    , withControls
    , withGroups
    , withInnerMove
    , withJson
    , withPointRadius
    , withState
    , withViewFunction
    )

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
import State exposing (State(..))
import Svg exposing (Svg)
import Types exposing (Key, SubKey)


type alias ViewFunction fig =
    Params fig -> Element fig -> Svg (Msg fig)


type alias InnerMoveFunction fig =
    SubKey -> Vector -> Figure fig -> Figure fig


type alias Config fig =
    { config : Cfg fig
    , params : Params fig
    }


{-| Configurations for internal use

It is necessary to make Model and Msg generic on figure type in order to avoid circular dependencies.

Most users will probably not care about these.

-}
type alias Cfg fig =
    { drag : Draggable.Config ( Key, SubKey ) (Msg fig)
    , view : ViewFunction fig
    , innerMove : InnerMoveFunction fig
    , actionButtons : Element fig -> List (Html (Msg fig))
    , shapeEncoder : fig -> Value
    , shapeDecoder : Decoder fig
    }


{-| Internal configuration parameters that might change during the lifetime of the editor

The state is controlled by the main model and is copied when passed to scene.

-}
type alias Params fig =
    { pointRadius : Float
    , zoomControls : Bool
    , panControls : Bool
    , panWithTouch : Bool
    , groups : List Key
    , state : State fig
    , sceneId : String
    }


init : Config fig
init =
    { params = initParams
    , config = initConfig
    }


initParams : Params fig
initParams =
    { pointRadius = 0.5
    , zoomControls = True
    , panControls = True
    , panWithTouch = True
    , groups = []
    , state = StandardEditor
    , sceneId = "SCENE-EDITOR"
    }


initConfig : Cfg fig
initConfig =
    { drag = makeDragConfig OnDragBy OnSelectFigure
    , shapeEncoder = \_ -> Json.Encode.null
    , shapeDecoder = Json.Decode.fail "no shape decoder provided"
    , view = \_ _ -> Svg.text_ [] [ Svg.text "no view function was provided" ]
    , innerMove = \_ _ fig -> fig
    , actionButtons = \_ -> []
    }


withViewFunction : ViewFunction fig -> Config fig -> Config fig
withViewFunction move ({ config } as cfg) =
    { cfg | config = { config | view = move } }


withInnerMove : InnerMoveFunction fig -> Config fig -> Config fig
withInnerMove move ({ config } as cfg) =
    { cfg | config = { config | innerMove = move } }


withActionButtons : (Element fig -> List (Html (Msg fig))) -> Config fig -> Config fig
withActionButtons buttons ({ config } as cfg) =
    { cfg | config = { config | actionButtons = buttons } }


withJson : { decoder : Decoder fig, encoder : fig -> Json.Encode.Value } -> Config fig -> Config fig
withJson data ({ config } as cfg) =
    { cfg | config = { config | shapeDecoder = data.decoder, shapeEncoder = data.encoder } }


withGroups : List Key -> Config fig -> Config fig
withGroups =
    (L.compose L.params L.groups).set


withPointRadius : Float -> Config fig -> Config fig
withPointRadius r =
    L.modify L.params <| \p -> { p | pointRadius = r }


withControls : { zoom : Bool, pan : Bool, drag : Bool } -> Config fig -> Config fig
withControls value =
    L.modify L.params <| \p -> { p | zoomControls = value.zoom, panControls = value.pan, panWithTouch = value.drag }


withState : State fig -> Config fig -> Config fig
withState value =
    L.modify L.params <| \p -> { p | state = value }


makeDragConfig : (Vector -> msg) -> (Key -> SubKey -> msg) -> Draggable.Config ( Key, SubKey ) msg
makeDragConfig dragMsg selectMsg =
    Draggable.customConfig
        [ DE.onDragBy (dragMsg << vector)
        , DE.onDragStart <| \( key, subKey ) -> selectMsg key subKey
        , DE.onClick <| \( key, subKey ) -> selectMsg key subKey
        ]
