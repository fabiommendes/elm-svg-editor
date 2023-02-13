module Config exposing
    ( Config
    , Params
    , init
    , makeDragConfig
    , withActionButtons
    , withConnector
    , withControls
    , withDefaultFigure
    , withGroups
    , withJson
    , withPointRadius
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
import Scene exposing (Scene)
import Shape.Type exposing (Any)
import State exposing (State(..))
import Types exposing (Key, SubKey)


type alias FigureConnection =
    { connect : Element -> Element -> Maybe Figure
    , end : Element -> Scene -> Scene
    , canConnect : Element -> Bool
    }


type alias Config =
    { config : Cfg
    , params : Params
    }


{-| Configurations for internal use

It is necessary to make Model and Msg generic on figure type in order to avoid circular dependencies.

Most users will probably not care about these.

-}
type alias Cfg =
    { drag : Draggable.Config ( Key, SubKey ) (Msg)
    , actionButtons : Element -> List (Html (Msg))
    , shapeEncoder : Any -> Value
    , shapeDecoder : Decoder Any
    , defaultFigure : Figure
    , connection : FigureConnection
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


init : Figure -> Config
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


initConfig : Figure -> Cfg
initConfig fig =
    { drag = makeDragConfig OnDragBy OnSelectFigure
    , shapeEncoder = \_ -> Json.Encode.null
    , shapeDecoder = Json.Decode.fail "no shape decoder provided"
    , actionButtons = \_ -> []
    , defaultFigure = fig
    , connection =
        { connect = \_ _ -> Nothing
        , end = \_ -> identity
        , canConnect = \_ -> False
        }
    }


withActionButtons : (Element -> List (Html (Msg))) -> Config -> Config
withActionButtons buttons ({ config } as cfg) =
    { cfg | config = { config | actionButtons = buttons } }


withDefaultFigure : Figure -> Config -> Config
withDefaultFigure fig ({ config } as cfg) =
    { cfg | config = { config | defaultFigure = fig } }


withConnector : FigureConnection -> Config -> Config
withConnector conn ({ config } as cfg) =
    { cfg | config = { config | connection = conn } }


withJson : { decoder : Decoder Any, encoder : Any -> Json.Encode.Value } -> Config -> Config
withJson data ({ config } as cfg) =
    { cfg | config = { config | shapeDecoder = data.decoder, shapeEncoder = data.encoder } }


withGroups : List Key -> Config -> Config
withGroups =
    (L.compose L.params L.groups).set


withPointRadius : Float -> Config -> Config
withPointRadius r =
    L.modify L.params <| \p -> { p | pointRadius = r }


withControls : { zoom : Bool, pan : Bool, drag : Bool } -> Config -> Config
withControls value =
    L.modify L.params <| \p -> { p | zoomControls = value.zoom, panControls = value.pan, panWithTouch = value.drag }


makeDragConfig : (Vector -> msg) -> (Key -> SubKey -> msg) -> Draggable.Config ( Key, SubKey ) msg
makeDragConfig dragMsg selectMsg =
    Draggable.customConfig
        [ DE.onDragBy (dragMsg << vector)
        , DE.onDragStart <| \( key, subKey ) -> selectMsg key subKey
        , DE.onClick <| \( key, subKey ) -> selectMsg key subKey
        ]
