module Decode exposing (..)

import BoundingBox2d
import Dict
import Figure exposing (Figure)
import Geometry exposing (BBox, angle, vector)
import Group exposing (GroupData)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Length exposing (meters)
import Scene exposing (Scene)
import Shape.Any exposing (Any(..))
import Shape.Image
import Shape.Line
import Shape.Point
import Shape.Text
import Types exposing (..)
import Util exposing (flip)


pair : Decoder ( Float, Float )
pair =
    succeed Tuple.pair
        |> required "x" float
        |> required "y" float


key : Decoder Key
key =
    int


label : Decoder Label
label =
    string



---
--- SHAPES
---


shape : Decoder Any
shape =
    field "type" string
        |> andThen
            (\s ->
                case s of
                    "point" ->
                        map PointModel point

                    "text" ->
                        map TextModel text

                    "image" ->
                        map ImageModel image

                    "line" ->
                        map LineModel line

                    _ ->
                        fail ("invalid type: " ++ s)
            )


image : Decoder Shape.Image.Image
image =
    withType "image" <|
        (succeed Shape.Image.Image
            |> required "href" string
            |> required "width" float
        )


line : Decoder Shape.Line.Line
line =
    withType "line" <|
        (succeed Shape.Line.Line
            |> required "vertices" (list pair |> map (List.map Geometry.point))
        )


point : Decoder Shape.Point.Point
point =
    withType "point" <|
        succeed ()


text : Decoder Shape.Text.Text
text =
    withType "text" <|
        (succeed Shape.Text.Text
            |> required "content" string
        )



---
--- Figures
---


figure : Decoder a -> Decoder (Figure a)
figure shapeDec =
    let
        style =
            list string
                |> andThen
                    (\xs ->
                        case xs of
                            [ a, b ] ->
                                succeed { attr = a, value = b }

                            _ ->
                                fail "expect 2 elements"
                    )
    in
    succeed Figure
        |> required "label" string
        |> required "scale" float
        |> required "translation" (map vector pair)
        |> required "rotation" (map angle float)
        |> required "editable" bool
        |> required "draggable" bool
        |> required "visible" bool
        |> required "style" (list style)
        |> required "data" shapeDec


scene : Decoder a -> Decoder (Scene a)
scene shapeDec =
    let
        keyfig : Decoder ( Key, Figure a )
        keyfig =
            field "key" key
                |> andThen (\k -> map (Tuple.pair k) (figure shapeDec))

        groups : Decoder (GroupData Key)
        groups =
            field "groups" (keyValuePairs (list key))
                |> map Dict.fromList

        initScene s scale translation bb =
            { s | scale = scale, translation = translation, bbox = bb }

        updateScene : List ( Key, Figure a ) -> GroupData Key -> Scene a -> Scene a
        updateScene figs grps scene_ =
            let
                expand lst =
                    lst
                        |> Dict.toList
                        |> List.concatMap
                            (\( k, vs ) ->
                                List.map (flip Tuple.pair k) vs
                            )
                        |> Dict.fromList

                reducer ( k, fig ) =
                    Scene.put k fig
            in
            List.foldl reducer scene_ (List.reverse figs)
                |> Scene.groupMany (expand grps)
    in
    map3 updateScene (field "objects" <| list keyfig) groups <|
        (succeed (initScene <| Scene.init 0 0)
            |> required "scale" float
            |> required "translation" (map vector pair)
            |> required "bbox" bbox
        )


bbox : Decoder BBox
bbox =
    list float
        |> andThen
            (\xs ->
                case xs |> List.map meters of
                    [ minX, minY, maxX, maxY ] ->
                        succeed <|
                            BoundingBox2d.fromExtrema
                                { minX = minX
                                , minY = minY
                                , maxX = maxX
                                , maxY = maxY
                                }

                    _ ->
                        fail "expect 4 elements"
            )


withType : String -> Decoder a -> Decoder a
withType name dec =
    field "type" string
        |> andThen
            (\s ->
                if s == name then
                    dec

                else
                    fail ("not a " ++ name)
            )
