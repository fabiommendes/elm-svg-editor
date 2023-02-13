module Shape.Any exposing
    ( Any(..), Element, Figure, Map, Replace
    , line, point, text, image
    , andThen, map, moveInside
    , actionButtons, view
    , connect, endConnection, mapShapeId, mapShape, unwrap, replaceConst
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

import BaseTypes exposing (Direction(..))
import Config exposing (Params)
import Element
import Figure
import Geometry as G exposing (Point, Vector)
import Geometry.CtxPoint as CtxPoint exposing (distanceFrom, origin, purePoint)
import Html exposing (Html, a)
import Length exposing (inMeters)
import Lens as L
import Monocle.Lens as L
import Msg exposing (Msg)
import Scene exposing (Scene)
import Shape.Image exposing (Image)
import Shape.Line exposing (Fill(..), Line)
import Shape.Point exposing (Point)
import Shape.Text exposing (Text)
import Svg exposing (Svg)
import Types exposing (..)
import Vector2d


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


mapShapeId : Map
mapShapeId =
    { line = identity
    , text = identity
    , image = identity
    }


replaceConst : a -> Replace a
replaceConst a =
    let
        const =
            \_ -> a
    in
    { line = const
    , text = const
    , image = const
    , point = const
    }


point : G.Point -> Figure
point pt =
    Figure.new (PointModel ()) |> Figure.move (G.vector (G.fromPoint pt))


line : List ( Float, Float ) -> Figure
line data =
    Figure.new
        (LineModel
            { vertices = List.map (G.point >> purePoint) data
            , duplicateLast = True
            , fill = Open
            }
        )


image : Float -> String -> Figure
image size href =
    Figure.new (ImageModel { href = href, width = size })


text : String -> Figure
text src =
    Figure.new (TextModel { content = src })


map : Map -> Figure -> Figure
map mapper fig =
    { fig | shape = mapShape mapper fig.shape }


andThen : Replace Figure -> Figure -> Figure
andThen mapper fig =
    { fig | shape = (unwrap mapper fig.shape).shape }


mapShape : Map -> Any -> Any
mapShape mapper fig =
    case fig of
        PointModel _ ->
            PointModel ()

        LineModel ln ->
            LineModel (mapper.line ln)

        TextModel str ->
            TextModel (mapper.text str)

        ImageModel href ->
            ImageModel (mapper.image href)


unwrap : Replace a -> Any -> a
unwrap mapper fig =
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
    case ( fig.shape, sub ) of
        ( LineModel obj, [ i ] ) ->
            fig
                |> L.shape.set (LineModel <| Shape.Line.movePoint i by obj)

        _ ->
            fig


connect : Element -> Element -> Maybe Figure
connect ({ model } as target) src =
    let
        shift =
            Vector2d.minus model.translation src.model.translation
    in
    case ( target.shape, src.shape ) of
        ( LineModel ln, PointModel _ ) ->
            let
                pointLocal =
                    purePoint (G.pointVec shift)
                        |> CtxPoint.from src.key

                distance =
                    pointLocal |> distanceFrom origin |> inMeters
            in
            if distance <= 1.0e-3 then
                Just target.model

            else
                Just { model | shape = LineModel { ln | vertices = ln.vertices ++ [ pointLocal ] } }

        ( PointModel _, _ ) ->
            let
                targetAsLine =
                    Element.map (\_ -> (line []).shape) target
            in
            connect targetAsLine src

        -- ( LineModel ln, LineModel ln2 ) ->
        --     let
        --         vertices =
        --             ln2.vertices |> List.map (Point2d.translateBy shift)
        --     in
        --     ( { target | shape = LineModel { ln | vertices = ln.vertices ++ vertices } }
        --     , src
        --     )
        _ ->
            Nothing


endConnection : Element -> Scene Any -> Scene Any
endConnection elem scene =
    case elem.shape of
        LineModel ln ->
            let
                keys =
                    ln.vertices
                        |> List.filterMap (.ctx >> .from)
            in
            scene
                |> Scene.moveFrom keys Down elem.key

        _ ->
            scene


view : Params -> Element -> Svg (Msg Any)
view cfg fig =
    let
        render viewFn msg data =
            viewFn cfg (Element.map (\_ -> data) fig)
                |> Svg.map (Msg.map msg)
    in
    case fig.model.shape of
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
    case fig.model.shape of
        LineModel obj ->
            Shape.Line.actionButtons obj (Element.map (\_ -> obj) fig)
                |> List.map (Html.map (Msg.map LineModel))

        _ ->
            []
