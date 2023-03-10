module Geometry.SvgPath exposing (..)

import Geometry exposing (Point, fromPoint, point)
import Geometry.Paths exposing (ghostLine)
import List.Extra as List
import Svg.PathD as D
import Util exposing (iff)


pathD : Bool -> List Point -> String
pathD fill line =
    case line of
        start :: rest ->
            rest
                |> List.map (fromPoint >> D.L)
                |> (::) (D.M (fromPoint start))
                |> iff fill closePath identity
                |> D.pathD

        _ ->
            pathD fill [ point ( 0, 0 ) ]


closePath : List D.Segment -> List D.Segment
closePath lst =
    case lst of
        (D.M pt) :: _ ->
            lst ++ [ D.T pt, D.z ]

        _ ->
            lst ++ [ D.z ]


ghostLinePath : Float -> List Point -> String
ghostLinePath factor pts =
    let
        ( origin, vertices ) =
            ghostLine factor pts
                |> List.uncons
                |> Maybe.withDefault ( point ( 0, 0 ), [] )
    in
    D.pathD <|
        (D.M (fromPoint origin) :: List.map (fromPoint >> D.L) vertices)
