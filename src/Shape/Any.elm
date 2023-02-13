module Shape.Any exposing
    ( line, point, text, image
    , andThen, map, moveInside
    , connect, endConnection, mapShape, mapShapeId, removeInside, replaceConst, unwrap
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
import Element exposing (Element)
import Figure exposing (Figure)
import Geometry as G exposing (Vector)
import Geometry.CtxPoint as CtxPoint exposing (distanceFrom, origin, purePoint)
import Length exposing (inMeters)
import List.Extra as List
import Point2d
import Scene exposing (Scene)
import Shape.Line
import Shape.Type exposing (Any(..), Fill(..), Map, Replace)
import Types exposing (..)
import Vector2d


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
    let
        ( head, tail ) =
            List.uncons data
                |> Maybe.withDefault ( ( 0, 0 ), [] )
                |> Tuple.mapBoth G.vector (List.map G.point)
    in
    Figure.new
        (LineModel
            { vertices = List.map (Point2d.translateBy (Vector2d.reverse head) >> purePoint) tail
            , duplicateLast = True
            , fill = Closed
            }
        )
        |> Figure.move head


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
        ( LineModel shape, [ i ] ) ->
            (fig |> Figure.replace (LineModel shape))
                |> Shape.Line.movePoint i by shape

        _ ->
            fig


removeInside : SubKey -> Figure -> Maybe Figure
removeInside sub fig =
    case ( fig.shape, sub ) of
        ( LineModel shape, [ i ] ) ->
            Just { fig | shape = LineModel (Shape.Line.removePoint i shape) }

        _ ->
            Just fig


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


endConnection : Element -> Scene -> Scene
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
