module Geometry.Paths exposing (..)

import Direction2d
import Geometry exposing (vector)
import Geometry.PointExt as PointExt exposing (PointExt, pointExt)
import Length exposing (meters)
import Vector2d


smooth : List PointExt -> List PointExt
smooth lst =
    let
        run prev pts =
            case ( prev, pts ) of
                ( Nothing, pt :: rest ) ->
                    pt :: run (Just pt) rest

                ( Just ptBefore, pt :: ptAfter :: rest ) ->
                    let
                        directionBefore =
                            Vector2d.from pt.point ptBefore.point

                        directionAfter =
                            Vector2d.from pt.point ptAfter.point

                        smoothingFactor =
                            0.15

                        before =
                            pt |> PointExt.translateBy (Vector2d.scaleBy smoothingFactor directionBefore)

                        after =
                            pt |> PointExt.translateBy (Vector2d.scaleBy smoothingFactor directionAfter)
                    in
                    before :: after :: run (Just pt) (ptAfter :: rest)

                _ ->
                    pts
    in
    run Nothing lst


ghostLine : Float -> List PointExt -> List PointExt
ghostLine factor line =
    let
        direction f x y =
            Direction2d.from x.point y.point
                |> Maybe.map
                    (Direction2d.perpendicularTo
                        >> Direction2d.toVector
                        >> Vector2d.scaleTo (meters f)
                    )
                |> Maybe.withDefault Vector2d.zero

        fromSegment2 f x y =
            x |> PointExt.translateBy (direction f x y)

        fromSegment3 f x y z =
            let
                d1 =
                    direction -f y x

                d2 =
                    direction f y z
            in
            y |> PointExt.translateBy (Vector2d.sum [ d1, d2 ] |> Vector2d.scaleTo (meters <| abs f))

        do : Maybe PointExt -> List PointExt -> List PointExt
        do start pts =
            case ( start, pts ) of
                ( Nothing, x :: y :: rest ) ->
                    fromSegment2 factor x y :: do (Just x) (y :: rest)

                ( Just prev, x :: y :: rest ) ->
                    fromSegment3 factor prev x y :: do (Just x) (y :: rest)

                ( Just prev, [ x ] ) ->
                    [ fromSegment2 -factor x prev ]

                _ ->
                    []
    in
    do Nothing (pointExt ( 0, 0 ) :: line)


pairsWithExtrapolation : List PointExt -> List ( PointExt, PointExt )
pairsWithExtrapolation vertices =
    case vertices of
        [ pt1, pt2 ] ->
            [ ( pt1, pt2 ), ( pt2, pt2 |> PointExt.translateBy (Vector2d.from pt1.point pt2.point) ) ]

        pt1 :: pt2 :: rest ->
            ( pt1, pt2 ) :: pairsWithExtrapolation (pt2 :: rest)

        [ pt1 ] ->
            [ ( pt1, pt1 |> PointExt.translateBy (vector ( 0, 0.5 )) ) ]

        [] ->
            []
