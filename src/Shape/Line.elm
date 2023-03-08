module Shape.Line exposing
    ( insertMiddlePoint
    , insertPointAt
    , movePoint
    , removePoint
    , vertices
    , withValidSubkey
    )

import Figure exposing (Figure)
import Geometry exposing (Point, Vector)
import Geometry.CtxPoint exposing (CtxPoint, midpoint, pointCtx, translateBy)
import Geometry.Paths exposing (pairs)
import Lens as L
import List.Extra as List
import Monocle.Lens as L
import Msg exposing (Msg(..))
import Point2d
import Shape.Type exposing (Any(..), Fill(..), Line)
import Types exposing (..)
import Vector2d


{-| Extract vertices as points
-}
vertices : Line -> List Point
vertices pt =
    pt.vertices |> List.map .point


{-| Move the i-th internal point by the given displacement
-}
movePoint : Int -> Vector -> Line -> Figure -> Figure
movePoint i delta shape fig =
    if i == 0 then
        { fig
            | translation = Vector2d.sum [ fig.translation, delta ]
            , shape = LineModel { shape | vertices = shape.vertices |> List.map (translateBy (Vector2d.reverse delta)) }
        }

    else
        { fig | shape = LineModel { shape | vertices = shape.vertices |> List.updateAt (i - 1) (translateBy delta) } }


{-| Add new point in the i-th position
-}
insertMiddlePoint : Int -> Line -> Line
insertMiddlePoint i line =
    let
        ( before, after ) =
            (pointCtx ( 0, 0 ) :: line.vertices)
                |> pairs line.fill
                |> List.splitAt i

        before_ =
            before |> List.drop 1 |> List.map Tuple.first

        after_ =
            case after of
                ( pt1, pt2 ) :: rest ->
                    pt1 :: midpoint pt1 pt2 :: List.map Tuple.first rest

                _ ->
                    []
    in
    L.vertices.set (before_ ++ after_) line


{-| Add new point in the i-th position
-}
insertPointAt : Int -> CtxPoint -> Line -> Figure -> Figure
insertPointAt i pt line fig =
    if i == 0 then
        let
            delta =
                Vector2d.from Point2d.origin pt.point
        in
        movePoint 0 delta { line | vertices = pointCtx ( 0, 0 ) :: line.vertices } fig

    else
        let
            ( before, after ) =
                (pointCtx ( 0, 0 ) :: line.vertices)
                    |> List.splitAt i

            line_ =
                L.vertices.set (List.drop 1 before ++ (pt :: after)) line
        in
        { fig | shape = LineModel line_ }


{-| Remove the i-th point from line
-}
removePoint : Int -> Line -> Line
removePoint i =
    L.modify L.vertices (List.removeAt (i - 1))


{-| Compute transformation with point, if subkey is valid
-}
withValidSubkey : SubKey -> (Int -> Line -> Line) -> Line -> Line
withValidSubkey subKey func line =
    case subKey of
        [ i ] ->
            func i line

        _ ->
            line
