module Svg.Editor.Config exposing
    ( Config, init
    , config, controls, groups, pointRadius, sceneId, editable, shape, readOnly
    )

{-| Global application configuration

@docs Config, init


## Custom config

@docs config, controls, groups, pointRadius, sceneId, editable, shape, readOnly

-}

import Draggable
import Draggable.Events as DE
import Geometry exposing (Vector, vector)
import Internal.Types exposing (Config(..))
import Msg exposing (Msg(..))
import State exposing (State(..))
import Types exposing (Key, Label, SubKey)
import Util exposing (iff)


{-| The config object. Usually should be created using init or the config utilitiy function
-}
type alias Config =
    Internal.Types.Config


{-| Default config
-}
init : Config
init =
    Cfg
        { pointRadius = 0.5
        , zoomControls = True
        , panControls = True
        , panWithTouch = True
        , groups = []
        , sceneId = "SCENE-EDITOR"
        , shape = { width = 20, height = 20 }
        , edit = True
        , drag = makeDragConfig OnDragBy OnSelectFigure
        , initialState = StandardEditor
        , styleFontSize = 0.4
        , styleLineWidth = 0.12
        , styleLineWidthSelected = 0.2
        , stylePrimaryColor = "white"
        , styleConstrastColor = "black"
        , styleSelectedColor = "rgb(255, 174, 0)"
        }


{-| Create a custom config from a list of properties,
e.g.

    config
        [ groups [ "routes", "rock-faces", "elements" ]
        , pointRadius 2
        , controls { zoom = False, pan = True, drag = False }
        ]

-}
config : List (Config -> Config) -> Config
config =
    List.foldl (\t acc -> t acc) init


{-| List of allowed groups in scene.

Each element can be in at most a single group and have a unique index in that group.

This is useful, for instance, to mark routes or other highlights on the topo map and
have numbers identifying each element.

-}
groups : List Label -> Config -> Config
groups value (Cfg cfg) =
    Cfg { cfg | groups = value }


{-| Change the default scene id.

This is necessary if two scenes are present in the same screen. Each scene must have a unique id.

-}
sceneId : String -> Config -> Config
sceneId id (Cfg cfg) =
    Cfg { cfg | sceneId = id }


{-| Radius of points, when rendered as circles.
-}
pointRadius : Float -> Config -> Config
pointRadius value (Cfg cfg) =
    Cfg { cfg | pointRadius = value }


{-| Enable/disable zoom and pan controls in the UI
-}
controls : { zoom : Bool, pan : Bool, drag : Bool } -> Config -> Config
controls value (Cfg cfg) =
    Cfg { cfg | zoomControls = value.zoom, panControls = value.pan, panWithTouch = value.drag }


{-| Control if editor is editable or not.
-}
editable : Bool -> Config -> Config
editable value (Cfg cfg) =
    if value then
        Cfg { cfg | edit = True }

    else
        Cfg { cfg | edit = False, initialState = ReadOnlyView }


{-| Initial shape, (width, height), of the viewing area.

Scale is in meters.

-}
shape : ( Float, Float ) -> Config -> Config
shape ( width, height ) (Cfg cfg) =
    Cfg { cfg | shape = { width = width, height = height } }


{-| Initialize editor in a read-only state if boolean is true.
-}
readOnly : Bool -> Config -> Config
readOnly ctrl (Cfg cfg) =
    Cfg { cfg | initialState = iff ctrl ReadOnlyView StandardEditor }


makeDragConfig : (Vector -> Msg) -> (Key -> SubKey -> Msg) -> Draggable.Config ( Key, SubKey ) Msg
makeDragConfig dragMsg selectMsg =
    Draggable.customConfig
        [ DE.onDragBy (dragMsg << vector)
        , DE.onDragStart <| \( key, subKey ) -> selectMsg key subKey
        , DE.onClick <| \( key, subKey ) -> selectMsg key subKey
        ]
