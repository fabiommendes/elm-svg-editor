module Decode exposing (..)

import BoundingBox2d
import Dict
import Figure exposing (Figure)
import Geometry as G exposing (BBox, angle, point, vector)
import Geometry.CtxPoint exposing (CtxPoint, Props)
import Group exposing (GroupData)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Length exposing (meters)
import Msg exposing (KeyBoardCommands(..), Msg(..))
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
    let
        keyAsString =
            string
                |> andThen
                    (\st ->
                        case String.split "-" st |> List.reverse of
                            x :: y :: rest ->
                                case String.toInt x of
                                    Just n ->
                                        succeed
                                            ( (y :: rest)
                                                |> List.reverse
                                                |> String.join "-"
                                            , n
                                            )

                                    _ ->
                                        fail ("invalid key: " ++ st)

                            _ ->
                                fail ("invalid key: " ++ st)
                    )

        keyAsInt =
            int |> map (flip nextKeyBy anonymousKey)
    in
    oneOf [ keyAsString, keyAsInt ]


label : Decoder Label
label =
    string


keyPress : Decoder (Msg a)
keyPress =
    map3 (\x y z -> ( x, y, z ))
        (field "key" string)
        (field "ctrlKey" bool)
        (field "shiftKey" bool)
        |> andThen
            (\p ->
                case p of
                    ( "Delete", _, _ ) ->
                        succeed (OnKeyPress Delete)

                    ( "z", True, False ) ->
                        succeed (OnKeyPress Undo)

                    ( "z", True, True ) ->
                        succeed (OnKeyPress Redo)

                    -- shift may capitalize Z
                    ( "Z", True, True ) ->
                        succeed (OnKeyPress Redo)

                    ( k, _, _ ) ->
                        fail k
            )



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
            |> required "vertices" (list pointExt)
            |> optional "duplicate_last" bool False
            |> optional "fill" lineFill Shape.Line.Open
        )


pointExt : Decoder CtxPoint
pointExt =
    map2 CtxPoint
        (succeed Props
            |> optional "back" bool False
            |> optional "breakLine" bool False
            |> optional "from" (maybe key) Nothing
        )
        (map G.point pair)


lineFill : Decoder Shape.Line.Fill
lineFill =
    string
        |> andThen
            (\s ->
                case s of
                    "open" ->
                        succeed Shape.Line.Open

                    "closed" ->
                        succeed Shape.Line.Closed

                    "left" ->
                        succeed Shape.Line.Left

                    "right" ->
                        succeed Shape.Line.Right

                    _ ->
                        fail "invalid fill type"
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

        makeScene : List ( Key, Figure a ) -> GroupData Key -> Scene a
        makeScene figs grps =
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
            List.foldl reducer Scene.init (List.reverse figs)
                |> Scene.groupMany (expand grps)
    in
    succeed makeScene
        |> required "objects" (list keyfig)
        |> required "groups" (keyValuePairs (list key) |> map Dict.fromList)


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
