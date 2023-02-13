module Encode exposing (..)

import Dict
import Figure exposing (Figure)
import Geometry exposing (..)
import Geometry.CtxPoint exposing (CtxPoint)
import Group
import Json.Decode as D
import Json.Encode exposing (..)
import List.Extra as List
import Maybe.Extra as Maybe
import Scene exposing (Scene)
import Shape.Any
import Shape.Image
import Shape.Line
import Shape.Point
import Shape.Text
import Shape.Types as Shape
import Types exposing (..)


extendObject : List ( String, Value ) -> Value -> Value
extendObject pairs obj =
    case D.decodeValue (D.keyValuePairs D.value) obj of
        Ok lst ->
            object (lst ++ pairs)

        Err _ ->
            object pairs


pair : ( Float, Float ) -> Value
pair ( x, y ) =
    object
        [ ( "x", float x )
        , ( "y", float y )
        ]


key : Key -> Value
key =
    showKey >> string


label : Label -> Value
label =
    string



---
--- Shapes
---


shape : Shape.Any -> Value
shape =
    Shape.Any.unwrap { line = line, text = text, point = point, image = image }


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
        , ( "vertices", list pointExt obj.vertices )
        , ( "duplicate_last", bool obj.duplicateLast )
        ]


pointExt : CtxPoint -> Value
pointExt pt =
    pair (fromPoint pt.point)
        |> extendObject
            [ ( "back", bool pt.ctx.back )
            , ( "breakLine", bool pt.ctx.breakLine )
            , ( "from", pt.ctx.from |> Maybe.unwrap null key )
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
        , ( "visible", bool obj.visible )
        , ( "style", list (list string) (obj.style |> List.map (\{ attr, value } -> [ attr, value ])) )
        , ( "data", shapeEnc obj.shape )
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
                |> (::) ( "key", key k )
                |> object
    in
    object
        [ ( "groups", object groups )
        , ( "objects", list identity objects )
        ]
