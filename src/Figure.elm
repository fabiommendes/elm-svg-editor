module Figure exposing
    ( Figure
    , andThen
    , grow
    , map
    , move
    , new
    , replace
    , rotate
    , setLabel
    )

{-| Generic figure utilities
-}

import Geometry exposing (Angle, Vector, angle, vector)
import Lens as L exposing (..)
import Monocle.Lens as L
import Quantity as Q
import Shape.Type exposing (Any)
import Types exposing (..)
import Vector2d


type alias Figure =
    { label : String
    , scale : Float
    , translation : Vector
    , rotation : Angle
    , shape : Any
    }


new : Any -> Figure
new data =
    { label = ""
    , scale = 1.0
    , translation = vector ( 0, 0 )
    , rotation = angle 0
    , shape = data
    }


map : (Any -> Any) -> Figure -> Figure
map f fig =
    { label = fig.label
    , scale = fig.scale
    , translation = fig.translation
    , rotation = fig.rotation
    , shape = f fig.shape
    }


replace : Any -> Figure -> Figure
replace x =
    map (\_ -> x)


andThen : (Any -> Figure) -> Figure -> Figure
andThen f fig =
    map (f >> .shape) fig


move : Vector -> Figure -> Figure
move by =
    L.modify translation (\pos -> Vector2d.sum [ by, pos ])


grow : Float -> Figure -> Figure
grow by =
    L.modify scale ((*) by)


rotate : Angle -> Figure -> Figure
rotate by fig =
    fig |> rotation.set (Q.sum [ fig.rotation, by ])


setLabel : Label -> Figure -> Figure
setLabel text =
    label.set text
