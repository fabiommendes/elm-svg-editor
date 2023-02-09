module Encode exposing (..)

import BoundingBox2d
import Dict
import Figure exposing (Figure)
import Geometry exposing (..)
import Group
import Json.Decode as D
import Json.Encode exposing (..)
import Length exposing (inMeters)
import List.Extra as List
import Scene exposing (Scene)
import Shape.Any
import Shape.Image
import Shape.Line
import Shape.Point
import Shape.Text
import Types exposing (..)


pair : ( Float, Float ) -> Value
pair ( x, y ) =
    object
        [ ( "x", float x )
        , ( "y", float y )
        ]


key : Key -> Value
key =
    int


label : Label -> Value
label =
    string



---
--- Shapes
---


shape : Shape.Any.Any -> Value
shape =
    Shape.Any.replace { line = line, text = text, point = point, image = image }


image : Shape.Image.Image -> Value
image obj =
    object
        [ ( "type", string "image" )
        , ( "href", string obj.href )
        , ( "width", float obj.width )
        ]


line : Shape.Line.Line -> Value
line obj =
    object
        [ ( "type", string "line" )
        , ( "vertices", list pair (obj.vertices |> List.map fromPoint) )
        , ( "duplicate_last", bool obj.duplicateLast  )
        ]


point : Shape.Point.Point -> Value
point _ =
    object
        [ ( "type", string "point" ) ]


text : Shape.Text.Text -> Value
text obj =
    object
        [ ( "type", string "text" )
        , ( "content", string obj.content )
        ]



---
--- Figures
---


figure : (a -> Value) -> Figure a -> Value
figure shapeEnc obj =
    object
        [ ( "label", string obj.label )
        , ( "scale", float obj.scale )
        , ( "translation", pair (fromVector obj.translation) )
        , ( "rotation", float (fromAngle obj.rotation) )
        , ( "editable", bool obj.editable )
        , ( "draggable", bool obj.draggable )
        , ( "visible", bool obj.visible )
        , ( "style", list (list string) (obj.style |> List.map (\{ attr, value } -> [ attr, value ])) )
        , ( "data", shapeEnc obj.data )
        ]


scene : (a -> Value) -> Scene a -> Value
scene shapeEnc obj =
    let
        elements =
            Scene.elements obj

        groups : List ( String, Value )
        groups =
            elements
                |> List.filterMap
                    (\e -> e.group |> Maybe.map (Tuple.pair e.key))
                |> Dict.fromList
                |> Group.fromGroupInfoMap
                |> Dict.toList
                |> List.map (\( grp, lst ) -> ( grp, list key lst ))

        objects : List Value
        objects =
            elements
                |> List.map (\e -> figure shapeEnc e.model |> withKey e.key)

        withKey k value =
            D.decodeValue (D.keyValuePairs D.value) value
                |> Result.withDefault []
                |> (::) ( "key", int k )
                |> object

        bbox =
            BoundingBox2d.extrema obj.bbox
                |> (\{ minX, minY, maxX, maxY } -> [ minX, minY, maxX, maxY ])
                |> List.map inMeters
    in
    object
        [ ( "scale", float obj.scale )
        , ( "translation", pair (fromVector obj.translation) )
        , ( "bbox", list float bbox )
        , ( "groups", object groups )
        , ( "objects", list identity objects )
        ]
