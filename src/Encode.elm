module Encode exposing (..)

import Dict
import Figure exposing (Figure)
import Geometry exposing (..)
import Group
import Json.Decode as D
import Json.Encode exposing (..)
import List.Extra as List
import List.GraphList as GE
import List.NonEmpty as NE
import Maybe.Extra as Maybe
import Scene exposing (Scene)
import Shape
import Shape.Type as Shape
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


nonEmpty : (a -> Value) -> NE.NonEmpty a -> Value
nonEmpty dec xs =
    NE.toList xs |> list dec


graphList : (a -> Value) -> GE.GraphList () a -> Value
graphList dec xs =
    GE.toList xs |> list dec


pt : Point -> Value
pt =
    fromPoint >> pair


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
    Shape.unwrap { line = line, text = text, point = point, image = image }


image : Shape.Image -> Value
image obj =
    object
        [ ( "type", string "image" )
        , ( "href", string obj.href )
        , ( "width", float obj.width )
        ]


line : Shape.Line -> Value
line obj =
    object
        [ ( "type", string "line" )
        , ( "vertices", graphList pt obj.vertices )
        ]


point : Shape.Point -> Value
point _ =
    object
        [ ( "type", string "point" ) ]


text : Shape.Text -> Value
text obj =
    object
        [ ( "type", string "text" )
        , ( "content", string obj.content )
        ]



---
--- Figures
---


figure : Figure -> Value
figure obj =
    object
        [ ( "label", string obj.label )
        , ( "scale", float obj.scale )
        , ( "translation", pair (fromVector obj.translation) )
        , ( "rotation", float (fromAngle obj.rotation) )
        , ( "data", shape obj.shape )
        ]


scene : Scene -> Value
scene obj =
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
                |> List.map (\e -> figure e.figure |> withKey e.key)

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
