module Decode exposing (..)

import BoundingBox2d
import Dict
import Figure exposing (Figure)
import Geometry exposing (BBox, angle, point, vector)
import Group exposing (GroupData)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Length exposing (meters)
import Msg exposing (KeyBoardCommands(..), Msg(..))
import Scene exposing (Scene)
import Shape.Type as Shape
import Types exposing (..)
import Util exposing (flip)


pair : Decoder ( Float, Float )
pair =
    succeed Tuple.pair
        |> required "x" float
        |> required "y" float


pt : Decoder Geometry.Point
pt =
    pair |> map Geometry.point


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


keyPress : Decoder Msg
keyPress =
    map3 (\x y z -> ( x, y, z ))
        (field "ctrlKey" bool)
        (field "shiftKey" bool)
        (field "key" string)
        |> andThen
            (\p ->
                case p of
                    ( False, _, "Delete" ) ->
                        succeed (OnKeyPress Delete)

                    ( True, _, "Delete" ) ->
                        succeed (OnKeyPress DeletePart)

                    ( True, False, "z" ) ->
                        succeed (OnKeyPress Undo)

                    -- shift capitalize Z
                    ( True, True, "Z" ) ->
                        succeed (OnKeyPress Redo)

                    ( _, _, "ArrowUp" ) ->
                        succeed Msg.panUp

                    ( _, _, "ArrowDown" ) ->
                        succeed Msg.panDown

                    ( _, _, "ArrowLeft" ) ->
                        succeed Msg.panLeft

                    ( _, _, "ArrowRight" ) ->
                        succeed Msg.panRight

                    ( _, _, k ) ->
                        fail (Debug.log "k" k)
            )



---
--- SHAPES
---


shape : Decoder Shape.Any
shape =
    field "type" string
        |> andThen
            (\s ->
                case s of
                    "point" ->
                        map Shape.PointModel point

                    "text" ->
                        map Shape.TextModel text

                    "image" ->
                        map Shape.ImageModel image

                    "line" ->
                        map Shape.LineModel line

                    _ ->
                        fail ("invalid type: " ++ s)
            )


image : Decoder Shape.Image
image =
    withType "image" <|
        (succeed Shape.Image
            |> required "href" string
            |> required "width" float
        )


line : Decoder Shape.Line
line =
    withType "line" <|
        (succeed Shape.Line
            |> required "vertices" (list pt)
            |> optional "duplicate_last" bool False
            |> optional "fill" lineFill Shape.Open
        )


lineFill : Decoder Shape.Fill
lineFill =
    string
        |> andThen
            (\s ->
                case s of
                    "open" ->
                        succeed Shape.Open

                    "closed" ->
                        succeed Shape.Closed

                    "left" ->
                        succeed Shape.Left

                    "right" ->
                        succeed Shape.Right

                    _ ->
                        fail "invalid fill type"
            )


point : Decoder Shape.Point
point =
    withType "point" <|
        succeed ()


text : Decoder Shape.Text
text =
    withType "text" <|
        (succeed Shape.Text
            |> required "content" string
        )



---
--- Figures
---


figure : Decoder Figure
figure =
    succeed Figure
        |> required "label" string
        |> required "scale" float
        |> required "translation" (map vector pair)
        |> required "rotation" (map angle float)
        |> required "data" shape


scene : Decoder Scene
scene =
    let
        keyfig : Decoder ( Key, Figure )
        keyfig =
            field "key" key
                |> andThen (\k -> map (Tuple.pair k) figure)

        makeScene : List ( Key, Figure ) -> GroupData Key -> Scene
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
