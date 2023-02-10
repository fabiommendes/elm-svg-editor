module Figure exposing (Figure, addStyle, grow, map, move, new, rotate, setLabel, visible, editable, draggable)

{-| Generic figure utilities
-}

import Geometry exposing (Angle, Vector, angle, vector)
import Lens as L exposing (..)
import Monocle.Lens as L
import Quantity as Q
import Types exposing (..)
import Vector2d


type alias Figure shape =
    { label : String
    , scale : Float
    , translation : Vector
    , rotation : Angle
    , editable : Bool
    , draggable : Bool
    , visible : Bool
    , style : List { attr : String, value : String }
    , shape : shape
    }


new : a -> Figure a
new data =
    { label = ""
    , scale = 1.0
    , translation = vector ( 0, 0 )
    , rotation = angle 0
    , editable = True
    , draggable = True
    , visible = True
    , style = []
    , shape = data
    }


map : (a -> b) -> Figure a -> Figure b
map f fig =
    { label = fig.label
    , scale = fig.scale
    , translation = fig.translation
    , rotation = fig.rotation
    , editable = fig.editable
    , draggable = fig.draggable
    , visible = fig.visible
    , style = fig.style
    , shape = f fig.shape
    }


move : Vector -> Figure shape -> Figure shape
move by =
    L.modify translation (\pos -> Vector2d.sum [ by, pos ])


grow : Float -> Figure shape -> Figure shape
grow by =
    L.modify scale ((*) by)


rotate : Angle -> Figure shape -> Figure shape
rotate by fig =
    fig |> rotation.set (Q.sum [ fig.rotation, by ])


visible : Bool -> Figure shape -> Figure shape
visible =
    L.visible.set


draggable : Bool -> Figure shape -> Figure shape
draggable =
    L.draggable.set


editable : Bool -> Figure shape -> Figure shape
editable =
    L.editable.set


setLabel : Label -> Figure shape -> Figure shape
setLabel text =
    label.set text


addStyle : String -> String -> Figure shape -> Figure shape
addStyle attr value =
    L.modify style ((::) { attr = attr, value = value })
