module Shape.Line exposing
    ( Fill(..)
    , Line
    , actionButtons
    , movePoint
    , view
    )

import Attributes as A
import Config exposing (Params)
import Direction2d
import Element exposing (Element)
import Figure exposing (move)
import Geometry exposing (Point, Vector, angle, fromPoint, point, vector)
import Geometry.Svg as S
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Length exposing (meters)
import Lens exposing (..)
import LineSegment2d exposing (LineSegment2d)
import List.Extra as List
import Monocle.Lens as L
import Msg exposing (Msg(..))
import Point2d
import Shape.Point exposing (circle)
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Svg.PathD as D
import Types exposing (..)
import Util exposing (flip, iff)
import Vector2d


type alias Line =
    { vertices : List Point
    , duplicateLast : Bool
    , fill : Fill
    }


type Fill
    = Open
    | Closed
    | Left
    | Right


{-| Move the i-th internal point by the given displacement
-}
movePoint : Int -> Vector -> Line -> Line
movePoint i displacement =
    L.modify vertices <| List.updateAt i (Point2d.translateBy displacement)


{-| Add new point in the i-th position
-}
addPoint : Int -> Line -> Line
addPoint i line =
    let
        ( before, after ) =
            line.vertices
                |> pairsWithExtrapolation
                |> List.splitAt i

        before_ =
            before |> List.map Tuple.first

        after_ =
            case after of
                ( pt1, pt2 ) :: rest ->
                    pt1 :: Point2d.midpoint pt1 pt2 :: List.map Tuple.first rest

                _ ->
                    []
    in
    vertices.set (before_ ++ after_) line


{-| Remove the i-th point from line
-}
removePoint : Int -> Line -> Line
removePoint i =
    L.modify vertices (List.removeAt i)


{-| Render line
-}
view : Params -> Element Line -> Svg (Msg Line)
view cfg elem =
    let
        subKey =
            elem.subKey |> List.getAt 0 |> Maybe.withDefault -1

        makePoint i pt =
            let
                isLast =
                    i == nPoints - 1 && elem.model.data.duplicateLast

                elem_ =
                    if isLast then
                        L.modify model (move (vector (fromPoint pt))) elem

                    else
                        elem

                attrs =
                    SA.class (iff (i == subKey) "point selected-point" "point")
                        :: SA.class ("key-" ++ String.fromInt i)
                        :: A.childPart [ i ] "line" elem_
            in
            if isLast then
                Shape.Point.viewAsPoint cfg elem.group elem.model.label elem.isSelected (SA.class "last-point" :: attrs) (point ( 0, 0 ))

            else
                circle cfg.pointRadius pt attrs []

        firstPoint =
            Shape.Point.view cfg elem

        nPoints =
            elem.model.data.vertices |> List.length

        points =
            firstPoint :: List.indexedMap makePoint elem.model.data.vertices

        labels =
            [ elem.group
                |> Maybe.map (\{ index } -> S.text_ [ SA.fontSize "1.5" ] [ S.text (String.fromInt index) ])
                |> Maybe.withDefault (S.text "")
            ]

        dAttribute =
            SA.d (pathD elem.model.data.fill line)

        transforms =
            A.transformElement elem

        line =
            (point ( 0, 0 ) :: elem.model.data.vertices)
                |> smooth
                |> smooth
                |> smooth
                |> smooth

        lines =
            [ S.path [ dAttribute, transforms, SA.class "background" ] []
            , S.path [ SA.d (ghostLine 0.1 line), SA.filter "blur(0.05px) opacity(0.9)", transforms, SA.class "foreground" ] []
            , S.path [ SA.d (ghostLine 0.25 line), SA.filter "blur(0.125px) opacity(0.7)", transforms, SA.class "foreground" ] []
            , S.path [ SA.d (ghostLine 0.5 line), SA.filter "blur(0.25px) opacity(0.5)", transforms, SA.class "foreground" ] []
            , S.path [ SA.d (ghostLine 0.8 line), SA.filter "blur(0.4px) opacity(0.3)", transforms, SA.class "foreground" ] []
            , S.path [ SA.d (ghostLine 1.2 line), SA.filter "blur(0.6px) opacity(0.1)", transforms, SA.class "foreground" ] []
            , S.path [ dAttribute, transforms, SA.class "foreground" ] []
            ]
    in
    S.g (A.classes "line" elem ++ A.style elem) <|
        List.concat [ lines, labels, points ]


ghostLine : Float -> List Point -> String
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

        ( origin, vertices ) =
            do Nothing (point ( 0, 0 ) :: line)
                |> List.uncons
                |> Maybe.withDefault ( point ( 0, 0 ), [] )
    in
    D.pathD <|
        (D.M (fromPoint origin) :: List.map (fromPoint >> D.L) vertices)


actionButtons : Line -> Element Line -> List (Html (Msg Line))
actionButtons line fig =
    let
        action name func =
            OnFigureUpdate name
                (\_ ->
                    let
                        i =
                            fig.subKey |> List.getAt 0 |> Maybe.withDefault 0
                    in
                    Just
                        (fig.model
                            |> data.set (func i line)
                        )
                )
                fig.key
    in
    [ H.button [ HA.class "btn", HE.onClick (action "add-point" addPoint) ] [ H.text "add point" ]
    , H.button [ HA.class "btn", HE.onClick (action "remove-point" removePoint) ] [ H.text "remove point" ]
    , H.text <| "numpoints: " ++ String.fromInt (List.length line.vertices + 1)
    ]


pathD : Fill -> List Point -> String
pathD fill line =
    case line of
        start :: rest ->
            rest
                |> List.map (fromPoint >> D.L)
                |> (::) (D.M (fromPoint start))
                |> iff (fill == Closed) closePath identity
                |> D.pathD

        _ ->
            pathD fill [ point ( 0, 0 ) ]


smooth : List Point -> List Point
smooth lst =
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
                        
                        smoothingFactor = 0.15

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


closePath : List D.Segment -> List D.Segment
closePath lst =
    case lst of
        (D.M pt) :: _ ->
            lst ++ [ D.T pt, D.z ]

        _ ->
            lst ++ [ D.z ]


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
