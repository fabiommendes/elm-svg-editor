module Config exposing
    ( Config
    , Params
    , ViewFunction
    , init
    , makeDragConfig
    , withActionButtons
    , withConnector
    , withControls
    , withDefaultFigure
    , withDefaultTarget
    , withGroups
    , withInnerMove
    , withJson
    , withPointRadius
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
import Scene exposing (Scene)


type alias ViewFunction fig =
    Params -> Element fig -> Svg (Msg fig)


type alias InnerMoveFunction fig =
    SubKey -> Vector -> Figure fig -> Figure fig


type alias FigureConnector fig =
    Element fig -> Element fig -> Maybe (Figure fig)


type alias FigureEndConnection fig =
    Element fig -> Scene fig -> Scene fig


type alias Config fig =
    { config : Cfg fig
    , params : Params
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
    , defaultFigure : Figure fig
    , defaultTarget : Figure fig
    , connectFigures : FigureConnector fig
    , endConnection : FigureEndConnection fig
    }


{-| Internal configuration parameters that might change during the lifetime of the editor

The state is controlled by the main model and is copied when passed to scene.

-}
type alias Params =
    { pointRadius : Float
    , zoomControls : Bool
    , panControls : Bool
    , panWithTouch : Bool
    , groups : List Key
    , sceneId : String
    }


init : Figure fig -> Config fig
init fig =
    { params = initParams
    , config = initConfig fig
    }


initParams : Params
initParams =
    { pointRadius = 0.5
    , zoomControls = True
    , panControls = True
    , panWithTouch = True
    , groups = []
    , sceneId = "SCENE-EDITOR"
    }


initConfig : Figure fig -> Cfg fig
initConfig fig =
    { drag = makeDragConfig OnDragBy OnSelectFigure
    , shapeEncoder = \_ -> Json.Encode.null
    , shapeDecoder = Json.Decode.fail "no shape decoder provided"
    , view = \_ _ -> Svg.text_ [] [ Svg.text "no view function was provided" ]
    , innerMove = \_ _ x -> x
    , actionButtons = \_ -> []
    , defaultFigure = fig
    , defaultTarget = fig
    , connectFigures = \_ _ -> Nothing
    , endConnection = \ _ -> identity 
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


withDefaultFigure : Figure fig -> Config fig -> Config fig
withDefaultFigure fig ({ config } as cfg) =
    { cfg | config = { config | defaultFigure = fig } }


withDefaultTarget : Figure fig -> Config fig -> Config fig
withDefaultTarget fig ({ config } as cfg) =
    { cfg | config = { config | defaultTarget = fig } }


withConnector : { connect : FigureConnector fig, end : FigureEndConnection fig } -> Config fig -> Config fig
withConnector { connect, end } ({ config } as cfg) =
    { cfg | config = { config | connectFigures = connect, endConnection = end } }


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


makeDragConfig : (Vector -> msg) -> (Key -> SubKey -> msg) -> Draggable.Config ( Key, SubKey ) msg
makeDragConfig dragMsg selectMsg =
    Draggable.customConfig
        [ DE.onDragBy (dragMsg << vector)
        , DE.onDragStart <| \( key, subKey ) -> selectMsg key subKey
        , DE.onClick <| \( key, subKey ) -> selectMsg key subKey
        ]
