module Shape.Line exposing
    ( insertPointAt
    , movePoint
    , removePoint
    , vertices
    , withValidSubkey
    )

import Figure exposing (Figure)
import Geometry exposing (Point, Vector, point)
import Lens as L
import List.Extra as List
import List.GraphList as GE
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
    pt.vertices |> GE.toList


{-| Move the i-th internal point by the given displacement
-}
movePoint : Int -> Vector -> Line -> Figure -> Figure
movePoint i delta line fig =
    let
        newLine =
            { line | vertices = line.vertices |> GE.updateAt i (Point2d.translateBy delta) }
    in
    { fig | shape = LineModel newLine } |> normalizeFigure newLine


normalizeFigure : Line -> Figure -> Figure
normalizeFigure line fig =
    let
        head =
            GE.head line.vertices
    in
    if head == Point2d.origin then
        fig

    else
        let
            delta =
                Vector2d.from Point2d.origin head

            translation =
                Vector2d.sum [ fig.translation, delta ]

            points =
                line.vertices
                    |> GE.map (Point2d.translateBy (Vector2d.reverse delta))
                    |> GE.updateAt 0 (\_ -> Point2d.origin)

            newLine =
                { line | vertices = points }
        in
        { fig | translation = translation, shape = LineModel newLine }


{-| Add new point in the i-th position
-}
insertPointAt : Int -> Point -> Line -> Figure -> Figure
insertPointAt i pt line fig =
    { fig | shape = LineModel { line | vertices = GE.insertAt i () pt line.vertices } }


{-| Remove the i-th point from line, if possible
-}
removePoint : Int -> Line -> Line
removePoint i line =
    L.modify L.vertices (GE.removeAt i) line


{-| Compute transformation with point, if subkey is valid
-}
withValidSubkey : SubKey -> (Int -> Line -> Line) -> Line -> Line
withValidSubkey subKey func line =
    case subKey of
        [ i ] ->
            func i line

        _ ->
            line
