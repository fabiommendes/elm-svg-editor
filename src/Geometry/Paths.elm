module Geometry.Paths exposing (..)

import Direction2d
import Geometry exposing (Point, point, vector)
import Length exposing (inMeters, meters)
import Lens exposing (vertices)
import List.Extra as List
import Point2d
import Shape.Type exposing (Fill(..))
import Vector2d


smooth2 : Int -> List Point -> List Point
smooth2 nIter lst =
    if nIter <= 0 then
        lst

    else
        smooth2 (nIter - 1) (smoothOnce2 lst)


smoothOnce2 : List Point -> List Point
smoothOnce2 lst =
    let
        run prev pts =
            case ( prev, pts ) of
                ( Nothing, pt :: rest ) ->
                    pt :: run (Just pt) rest

                ( Just ptBefore, pt :: ptAfter :: rest ) ->
                    let
                        directionBefore =
                            Vector2d.from pt ptBefore

                        directionAfter =
                            Vector2d.from pt ptAfter

                        smoothingFactor =
                            0.15

                        before =
                            pt |> Point2d.translateBy (Vector2d.scaleBy smoothingFactor directionBefore)

                        after =
                            pt |> Point2d.translateBy (Vector2d.scaleBy smoothingFactor directionAfter)
                    in
                    before :: after :: run (Just pt) (ptAfter :: rest)

                _ ->
                    pts
    in
    run Nothing lst


smooth : Int -> List Point -> List Point
smooth nIter lst =
    if nIter <= 0 then
        lst

    else
        smooth (nIter - 1) (smoothOnce lst)


smoothOnce : List Point -> List Point
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
                            Vector2d.from pt ptBefore

                        d1 =
                            Vector2d.from pt ptAfter

                        length =
                            min (Vector2d.length d0 |> inMeters) (Vector2d.length d1 |> inMeters) |> meters

                        d0_ =
                            d0 |> Vector2d.scaleTo length

                        d1_ =
                            d1 |> Vector2d.scaleTo length

                        middle =
                            Vector2d.interpolateFrom d0_ d1_ 0.5 |> Vector2d.scaleBy (smoothingFactor / 2)

                        before =
                            pt |> Point2d.translateBy (Vector2d.scaleBy smoothingFactor d0_ |> Vector2d.minus middle)

                        after =
                            pt |> Point2d.translateBy (Vector2d.scaleBy smoothingFactor d1_ |> Vector2d.minus middle)
                    in
                    before :: pt :: after :: run (Just pt) (ptAfter :: rest)

                _ ->
                    pts
    in
    run Nothing lst


ghostLine : Float -> List Point -> List Point
ghostLine factor line =
    let
        direction f x y =
            Direction2d.from x y
                |> Maybe.map
                    (Direction2d.perpendicularTo
                        >> Direction2d.toVector
                        >> Vector2d.scaleTo (meters f)
                    )
                |> Maybe.withDefault Vector2d.zero

        fromSegment2 f x y =
            x |> Point2d.translateBy (direction f x y)

        fromSegment3 f x y z =
            let
                d1 =
                    direction -f y x

                d2 =
                    direction f y z
            in
            y |> Point2d.translateBy (Vector2d.sum [ d1, d2 ] |> Vector2d.scaleTo (meters <| abs f))

        do : Maybe Point -> List Point -> List Point
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
    do Nothing (point ( 0, 0 ) :: line)


pairs : Fill -> List Point -> List ( Point, Point )
pairs fill =
    case fill of
        Closed ->
            pairsClosing

        _ ->
            pairsWithExtrapolation


pairsWithExtrapolation : List Point -> List ( Point, Point )
pairsWithExtrapolation vertices =
    case vertices of
        [ pt1, pt2 ] ->
            [ ( pt1, pt2 ), ( pt2, pt2 |> Point2d.translateBy (Vector2d.from pt1 pt2) ) ]

        pt1 :: pt2 :: rest ->
            ( pt1, pt2 ) :: pairsWithExtrapolation (pt2 :: rest)

        [ pt1 ] ->
            [ ( pt1, pt1 |> Point2d.translateBy (vector ( 0, 0.5 )) ) ]

        [] ->
            []


pairsClosing : List Point -> List ( Point, Point )
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


triple : Int -> Fill -> List Point -> Maybe ( Point, Point, Point )
triple idx fill points =
    case ( idx, fill, List.drop (idx - 1) points ) of
        ( 0, Closed, pt :: post :: rest ) ->
            List.last rest |> Maybe.map (\pre -> ( pre, pt, post ))

        ( 0, _, pt :: post :: _ ) ->
            let
                pre =
                    pt |> Point2d.translateBy (Vector2d.from post pt)
            in
            Just ( pre, pt, post )

        ( _, _, pre :: pt :: post :: _ ) ->
            Just ( pre, pt, post )

        ( _, Closed, [ pre, pt ] ) ->
            List.head points |> Maybe.map (\post -> ( pre, pt, post ))

        ( _, Open, [ pre, pt ] ) ->
            let
                post =
                    pt |> Point2d.translateBy (Vector2d.from pre pt)
            in
            Just ( pre, pt, post )

        _ ->
            Nothing
