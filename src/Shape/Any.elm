module Shape.Any exposing
    ( Any
    , Element
    , Figure
    , Map
    , Replace
    , actionButtons
    , andThen
    , line
    , map
    , moveInside
    , point
    , view
    )

import Element
import Figure
import Geometry exposing (Vector)
import Html exposing (Html)
import Lens exposing (data)
import Msg exposing (Msg)
import Shape.Image exposing (Image)
import Shape.Line exposing (Line)
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
    | TextModel String
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


point : Vector -> Figure
point by =
    Figure.new (PointModel ()) |> Figure.move by


line : Line -> Figure
line data =
    Figure.new (LineModel data)


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


view : Element -> Svg (Msg Any)
view fig =
    let
        render : (Element.Element a -> Svg (Msg a)) -> (a -> Any) -> a -> Svg (Msg Any)
        render viewFn msg data =
            viewFn (Element.map (\_ -> data) fig)
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
