module Geometry.Paths exposing (..)

import Direction2d
import Geometry exposing (vector)
import Geometry.CtxPoint as PointExt exposing (CtxPoint, pointCtx)
import Length exposing (inMeters, meters)
import Lens exposing (vertices)
import Shape.Types exposing (Fill(..))
import Vector2d


smooth2 : Int -> List CtxPoint -> List CtxPoint
smooth2 nIter lst =
    if nIter <= 0 then
        lst

    else
        smooth2 (nIter - 1) (smoothOnce2 lst)


smoothOnce2 : List CtxPoint -> List CtxPoint
smoothOnce2 lst =
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


smooth : Int -> List CtxPoint -> List CtxPoint
smooth nIter lst =
    if nIter <= 0 then
        lst

    else
        smooth (nIter - 1) (smoothOnce lst)


smoothOnce : List CtxPoint -> List CtxPoint
smoothOnce lst =
    let
        run prev pts =
            case ( prev, pts ) of
                ( Nothing, pt :: rest ) ->
                    pt :: run (Just pt) rest

                ( Just ptBefore, pt :: ptAfter :: rest ) ->
                    let
                        smoothingFactor =
                            0.45

                        d0 =
                            Vector2d.from pt.point ptBefore.point

                        d1 =
                            Vector2d.from pt.point ptAfter.point

                        length =
                            min (Vector2d.length d0 |> inMeters) (Vector2d.length d1 |> inMeters) |> meters

                        d0_ =
                            d0 |> Vector2d.scaleTo length

                        d1_ =
                            d1 |> Vector2d.scaleTo length

                        middle =
                            Vector2d.interpolateFrom d0_ d1_ 0.5 |> Vector2d.scaleBy (smoothingFactor / 2)

                        before =
                            pt |> PointExt.translateBy (Vector2d.scaleBy smoothingFactor d0_ |> Vector2d.minus middle)

                        after =
                            pt |> PointExt.translateBy (Vector2d.scaleBy smoothingFactor d1_ |> Vector2d.minus middle)
                    in
                    before :: pt :: after :: run (Just pt) (ptAfter :: rest)

                _ ->
                    pts
    in
    run Nothing lst


ghostLine : Float -> List CtxPoint -> List CtxPoint
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

        do : Maybe CtxPoint -> List CtxPoint -> List CtxPoint
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
    do Nothing (pointCtx ( 0, 0 ) :: line)


pairs : Fill -> List CtxPoint -> List ( CtxPoint, CtxPoint )
pairs fill =
    case fill of
        Closed ->
            pairsClosing

        _ ->
            pairsWithExtrapolation


pairsWithExtrapolation : List CtxPoint -> List ( CtxPoint, CtxPoint )
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


pairsClosing : List CtxPoint -> List ( CtxPoint, CtxPoint )
pairsClosing pts =
    let
        do first vertices =
            case ( first, vertices ) of
                ( Just _, pt1 :: pt2 :: rest ) ->
                    ( pt1, pt2 ) :: do first (pt2 :: rest)

                ( Just start, [ pt1 ] ) ->
                    [ ( pt1, start ) ]

                _ ->
                    pairsWithExtrapolation vertices
    in
    do (List.head pts) pts
