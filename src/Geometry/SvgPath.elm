module Geometry.SvgPath exposing (..)

import Geometry exposing (fromPoint)
import Geometry.Paths exposing (ghostLine)
import Geometry.PointEx exposing (PointEx, pointEx)
import List.Extra as List
import Svg.PathD as D
import Util exposing (iff)


pathD : Bool -> List PointEx -> String
pathD fill line =
    case line of
        start :: rest ->
            rest
                |> List.map (.point >> fromPoint >> D.L)
                |> (::) (D.M (fromPoint start.point))
                |> iff fill closePath identity
                |> D.pathD

        _ ->
            pathD fill [ pointEx ( 0, 0 ) ]


closePath : List D.Segment -> List D.Segment
closePath lst =
    case lst of
        (D.M pt) :: _ ->
            lst ++ [ D.T pt, D.z ]

        _ ->
            lst ++ [ D.z ]


ghostLinePath : Float -> List PointEx -> String
ghostLinePath factor pts =
    let
        ( origin, vertices ) =
            ghostLine factor pts
                |> List.uncons
                |> Maybe.withDefault ( pointEx ( 0, 0 ), [] )
    in
    D.pathD <|
        (D.M (fromPoint origin.point) :: List.map (.point >> fromPoint >> D.L) vertices)
