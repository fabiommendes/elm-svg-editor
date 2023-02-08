module Shape.Line exposing (Line, actionButtons, movePoint, view)

import Attributes as A
import Config exposing (ConfigParams)
import Element exposing (Element)
import Geometry exposing (Point, Vector, fromPoint, point, vector)
import Geometry.Svg as S
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Lens exposing (..)
import List.Extra as List
import Monocle.Lens as L
import Msg exposing (Msg(..))
import Point2d
import Shape.Point exposing (circle)
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Types exposing (..)
import Util exposing (iff)
import Vector2d


type alias Line =
    { vertices : List Point }


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


{-| Render line
-}
view : ConfigParams -> Element Line -> Svg (Msg Line)
view cfg elem =
    let
        subKey =
            elem.subKey |> List.getAt 0 |> Maybe.withDefault -1

        makePoint i pt =
            let
                classes =
                    SA.class (iff (i == subKey) "point selected-point" "point")
                        :: SA.class ("key-" ++ String.fromInt i)
                        :: A.transformElement elem
                        :: A.dragChild [ i ] elem
            in
            circle cfg.pointRadius (fromPoint pt) (classes ++ A.dragChild [ i ] elem) []

        firstPoint =
            Shape.Point.view cfg elem

        points =
            firstPoint :: List.indexedMap makePoint elem.model.data.vertices

        groupLabel =
            elem.group
                |> Maybe.map (\{ index } -> S.text_ [ SA.fontSize "1.5" ] [ S.text (String.fromInt index) ])
                |> Maybe.withDefault (S.text "")
    in
    S.g
        (A.classes "line" elem ++ A.style elem)
        (smoothLine [ A.transformElement elem, SA.class "background" ] elem.model.data
            :: smoothLine [ A.transformElement elem ] elem.model.data
            :: groupLabel
            :: points
        )


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


smoothLine : List (S.Attribute msg) -> Line -> Svg msg
smoothLine attrs line =
    S.path (pathAttribute (point ( 0, 0 )) line.vertices :: attrs) []


pathAttribute : Point -> List Point -> S.Attribute msg
pathAttribute origin points =
    let
        parts =
            List.map (part "T") points

        part cmd pt =
            let
                ( x, y ) =
                    fromPoint pt
            in
            cmd ++ String.fromFloat x ++ "," ++ String.fromFloat y
    in
    SA.d (String.join " " (part "M" origin :: parts))
