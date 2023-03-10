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
import Geometry.Paths exposing (pairs)
import Lens as L
import List.Extra as List
import List.NonEmpty as NE
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
    pt.vertices |> NE.toList


{-| Move the i-th internal point by the given displacement
-}
movePoint : Int -> Vector -> Line -> Figure -> Figure
movePoint i delta shape fig =
    if i == 0 then
        { fig
            | translation = Vector2d.sum [ fig.translation, delta ]
            , shape = LineModel { shape | vertices = shape.vertices |> NE.map (Point2d.translateBy (Vector2d.reverse delta)) }
        }

    else
        { fig | shape = LineModel { shape | vertices = shape.vertices |> NE.updateAt (i - 1) (Point2d.translateBy delta) } }


{-| Add new point in the i-th position
-}
insertMiddlePoint : Int -> Line -> Line
insertMiddlePoint i line =
    let
        ( before, after ) =
            vertices line
                |> pairs line.fill
                |> List.splitAt i

        before_ =
            before |> List.drop 1 |> List.map Tuple.first

        after_ =
            case after of
                ( pt1, pt2 ) :: rest ->
                    pt1 :: Point2d.midpoint pt1 pt2 :: List.map Tuple.first rest

                _ ->
                    []

        data =
            before_
                ++ after_
                |> NE.fromList
                |> Maybe.withDefault line.vertices
    in
    L.vertices.set data line


{-| Add new point in the i-th position
-}
insertPointAt : Int -> Point -> Line -> Figure -> Figure
insertPointAt i pt line fig =
    if i == 0 then
        let
            delta =
                Vector2d.from Point2d.origin pt
        in
        movePoint 0 delta line fig

    else
        let
            ( before, after ) =
                vertices line |> List.splitAt i

            pts =
                (List.drop 1 before ++ (pt :: after))
                    |> NE.fromList
                    |> Maybe.withDefault line.vertices

            line_ =
                line |> L.vertices.set pts
        in
        { fig | shape = LineModel line_ }


{-| Remove the i-th point from line, if possible
-}
removePoint : Int -> Line -> Line
removePoint i =
    L.modify L.vertices (NE.removeAt i)


{-| Compute transformation with point, if subkey is valid
-}
withValidSubkey : SubKey -> (Int -> Line -> Line) -> Line -> Line
withValidSubkey subKey func line =
    case subKey of
        [ i ] ->
            func i line

        _ ->
            line
