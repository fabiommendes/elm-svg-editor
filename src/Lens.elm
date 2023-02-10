module Lens exposing (..)

import List.Extra as List
import Monocle.Lens as L
import Monocle.Optional exposing (Optional)


type alias Lens a b =
    L.Lens a b


lens : (a -> b) -> (b -> a -> a) -> Lens a b
lens =
    L.Lens


runState : Lens a b -> (b -> ( c, b )) -> a -> ( c, a )
runState lens_ func obj =
    let
        ( value, new ) =
            func (lens_.get obj)
    in
    ( value, obj |> lens_.set new )



---
--- Attribute accessors
---


bbox : Lens { a | bbox : b } b
bbox =
    lens .bbox (\x m -> { m | bbox = x })


config : Lens { a | config : b } b
config =
    lens .config (\x m -> { m | config = x })


data : Lens { a | data : b } b
data =
    lens .data (\x m -> { m | data = x })


draggable : Lens { a | draggable : b } b
draggable =
    lens .draggable (\x m -> { m | draggable = x })


editable : Lens { a | editable : b } b
editable =
    lens .editable (\x m -> { m | editable = x })


figure : Lens { a | figure : b } b
figure =
    lens .figure (\x m -> { m | figure = x })


groups : Lens { a | groups : b } b
groups =
    lens .groups (\x m -> { m | groups = x })


ith : Int -> Optional (List a) a
ith i =
    { getOption = List.getAt i
    , set = List.setAt i
    }


model : Lens { a | model : b } b
model =
    lens .model (\x m -> { m | model = x })


label : Lens { a | label : b } b
label =
    lens .label (\x m -> { m | label = x })


objects : Lens { a | objects : b } b
objects =
    lens .objects (\x m -> { m | objects = x })


order : Lens { a | order : b } b
order =
    lens .order (\x m -> { m | order = x })


params : Lens { a | params : b } b
params =
    lens .params (\x m -> { m | params = x })


rotation : Lens { a | rotation : b } b
rotation =
    lens .rotation (\x m -> { m | rotation = x })


scale : Lens { a | scale : b } b
scale =
    lens .scale (\x m -> { m | scale = x })


scene : Lens { a | scene : b } b
scene =
    lens .scene (\x m -> { m | scene = x })


selected : Lens { a | selected : b } b
selected =
    lens .selected (\x m -> { m | selected = x })


shape : Lens { a | shape : b } b
shape =
    lens .shape (\x m -> { m | shape = x })


visible : Lens { a | visible : b } b
visible =
    lens .visible (\x m -> { m | visible = x })


style : Lens { a | style : b } b
style =
    lens .style (\x m -> { m | style = x })


translation : Lens { a | translation : b } b
translation =
    lens .translation (\x m -> { m | translation = x })


vertices : Lens { a | vertices : b } b
vertices =
    lens .vertices (\x m -> { m | vertices = x })
