module Shape.Any exposing
    ( Any(..), Element, Figure, Map, Replace
    , line, point, text, image
    , andThen, map, replace, moveInside
    , actionButtons, view
    , mapId
    )

{-|

@docs Any, Element, Figure, Map, Replace


## Constructors

@docs line, point, text, image


## Transforms

@docs andThen, map, replace, moveInside


## UI

@docs actionButtons, view

-}

import Config exposing (Params)
import Element
import Figure
import Geometry as G exposing (Vector)
import Html exposing (Html)
import Lens exposing (data)
import Msg exposing (Msg)
import Shape.Image exposing (Image)
import Shape.Line exposing (Fill(..), Line)
import Shape.Point exposing (Point)
import Shape.Text exposing (Text)
import Svg exposing (Svg)
import Types exposing (..)


type alias Figure =
    Figure.Figure Any


type alias Element =
    Element.Element Any


type Any
    = PointModel Point
    | LineModel Line
    | TextModel Text
    | ImageModel Image


type alias Map =
    { line : Line -> Line
    , text : Text -> Text
    , image : Image -> Image
    }


type alias Replace a =
    { line : Line -> a
    , text : Text -> a
    , point : Point -> a
    , image : Image -> a
    }


mapId : Map
mapId =
    { line = identity
    , text = identity
    , image = identity
    }


point : Vector -> Figure
point by =
    Figure.new (PointModel ()) |> Figure.move by


line : List ( Float, Float ) -> Figure
line data =
    Figure.new (LineModel { vertices = List.map G.point data, duplicateLast = True, fill = Open })


image : Float -> String -> Figure
image size href =
    Figure.new (ImageModel { href = href, width = size })


text : String -> Figure
text src =
    Figure.new (TextModel { content = src })


andThen : Replace Any -> Any -> Any
andThen =
    replace


map : Map -> Any -> Any
map mapper fig =
    case fig of
        PointModel _ ->
            PointModel ()

        LineModel ln ->
            LineModel (mapper.line ln)

        TextModel str ->
            TextModel (mapper.text str)

        ImageModel href ->
            ImageModel (mapper.image href)


replace : Replace a -> Any -> a
replace mapper fig =
    case fig of
        PointModel _ ->
            mapper.point ()

        LineModel ln ->
            mapper.line ln

        TextModel str ->
            mapper.text str

        ImageModel href ->
            mapper.image href


moveInside : SubKey -> Vector -> Figure -> Figure
moveInside sub by fig =
    case ( fig.data, sub ) of
        ( LineModel obj, [ i ] ) ->
            fig
                |> data.set (LineModel <| Shape.Line.movePoint i by obj)

        _ ->
            fig


view : Params -> Element -> Svg (Msg Any)
view cfg fig =
    let
        render viewFn msg data =
            viewFn cfg (Element.map (\_ -> data) fig)
                |> Svg.map (Msg.map msg)
    in
    case fig.model.data of
        PointModel pt ->
            render Shape.Point.view PointModel pt

        LineModel ln ->
            render Shape.Line.view LineModel ln

        TextModel str ->
            render Shape.Text.view TextModel str

        ImageModel href ->
            render Shape.Image.view ImageModel href


actionButtons : Element -> List (Html (Msg Any))
actionButtons fig =
    case fig.model.data of
        LineModel obj ->
            Shape.Line.actionButtons obj (Element.map (\_ -> obj) fig)
                |> List.map (Html.map (Msg.map LineModel))

        _ ->
            []
