module Figure exposing (Figure, addStyle, grow, map, move, new, rotate, setLabel)

{-| Generic figure utilities
-}

import Geometry exposing (Angle, Vector, angle, vector)
import Lens exposing (..)
import Monocle.Lens as L
import Quantity as Q
import Types exposing (..)
import Vector2d


type alias Figure a =
    { label : String
    , scale : Float
    , translation : Vector
    , rotation : Angle
    , editable : Bool
    , draggable : Bool
    , visible : Bool
    , style : List { attr : String, value : String }
    , data : a
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
    , data = data
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
    , data = f fig.data
    }


move : Vector -> Figure a -> Figure a
move by =
    L.modify translation (\pos -> Vector2d.sum [ by, pos ])


grow : Float -> Figure a -> Figure a
grow by =
    L.modify scale ((*) by)


rotate : Angle -> Figure a -> Figure a
rotate by fig =
    fig |> rotation.set (Q.sum [ fig.rotation, by ])


setLabel : Label -> Figure a -> Figure a
setLabel text =
    label.set text


addStyle : String -> String -> Figure a -> Figure a
addStyle attr value =
    L.modify style ((::) { attr = attr, value = value })
