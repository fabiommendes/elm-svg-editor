module Svg.Editor.Config exposing
    ( Config
    , config, controls, groups, init, pointRadius, sceneId
    )

{-| Global application configuration

@docs Config

-}

import Draggable
import Draggable.Events as DE
import Geometry exposing (Vector, vector)
import Lens as L
import Monocle.Lens as L
import Msg exposing (Msg(..))
import State exposing (State(..))
import Types exposing (Key, Label, SubKey)


{-| The config object. Usually should be created using init or the config utilitiy function
-}
type alias Config =
    { pointRadius : Float
    , zoomControls : Bool
    , panControls : Bool
    , panWithTouch : Bool
    , groups : List Label
    , sceneId : String
    , drag : Draggable.Config ( Key, SubKey ) Msg
    }


{-| Default config
-}
init : Config
init =
    { pointRadius = 0.5
    , zoomControls = True
    , panControls = True
    , panWithTouch = True
    , groups = []
    , sceneId = "SCENE-EDITOR"
    , drag = makeDragConfig OnDragBy OnSelectFigure
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
groups =
    L.groups.set


{-| Change the default scene id.

This is necessary if two scenes are present in the same screen. Each scene must have a unique id.

-}
sceneId : String -> Config -> Config
sceneId id cfg =
    { cfg | sceneId = id }


{-| Radius of points, when rendered as circles.
-}
pointRadius : Float -> Config -> Config
pointRadius r cfg =
    { cfg | pointRadius = r }


{-| Enable/disable zoom and pan controls in the UI
-}
controls : { zoom : Bool, pan : Bool, drag : Bool } -> Config -> Config
controls value cfg =
    { cfg | zoomControls = value.zoom, panControls = value.pan, panWithTouch = value.drag }


makeDragConfig : (Vector -> Msg) -> (Key -> SubKey -> Msg) -> Draggable.Config ( Key, SubKey ) Msg
makeDragConfig dragMsg selectMsg =
    Draggable.customConfig
        [ DE.onDragBy (dragMsg << vector)
        , DE.onDragStart <| \( key, subKey ) -> selectMsg key subKey
        , DE.onClick <| \( key, subKey ) -> selectMsg key subKey
        ]
