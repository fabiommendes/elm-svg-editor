module Shape.Line exposing (Line, actionButtons, movePoint, view)

import Attributes as A
import Element exposing (Element)
import Geometry as G exposing (Point, Vector, fromLine, fromPoint, vector)
import Geometry.Svg as S
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Lens exposing (data)
import List.Extra as List
import Msg exposing (Msg(..))
import Point2d
import Polyline2d
import Shape.Point exposing (viewPoint)
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Types exposing (..)
import Util exposing (iff)
import Vector2d


type alias Line =
    G.Line


{-| Move the i-th internal point by the given displacement
-}
movePoint : Int -> Vector -> Line -> Line
movePoint i displacement line =
    Polyline2d.vertices line
        |> List.updateAt i (Point2d.translateBy displacement)
        |> Polyline2d.fromVertices


{-| Add new point in the i-th position
-}
addPoint : Int -> Line -> Line
addPoint i line =
    let
        ( before, after ) =
            Polyline2d.vertices line
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
    (before_ ++ after_)
        |> Polyline2d.fromVertices


{-| Remove the i-th point from line
-}
removePoint : Int -> Line -> Line
removePoint i =
    onVertices (List.removeAt i)


onVertices : (List Point -> List Point) -> Line -> Line
onVertices func line =
    Polyline2d.vertices line
        |> func
        |> Polyline2d.fromVertices


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
view : Element Line -> Svg (Msg Line)
view fig =
    let
        subKey =
            fig.subKey |> List.getAt 0 |> Maybe.withDefault 0

        makePoint i pt =
            let
                radius =
                    iff (i == subKey) 0.75 (iff (i == 0) 0.5 0.25)

                classes =
                    [ SA.class <| iff (i == subKey) "point selected-point" "point"
                    , SA.class <| iff (i == 0) "handle" ""
                    , SA.class <| "key-" ++ String.fromInt i
                    ]
            in
            if i == 0 then
                viewPoint radius pt (classes ++ A.dragRoot fig) []

            else
                viewPoint radius pt (classes ++ A.dragChild [ i ] fig) []

        points =
            fig.model.data
                |> fromLine
                |> List.indexedMap makePoint

        groupLabel =
            fig.group
                |> Maybe.map (\{ index } -> S.text_ [ SA.fontSize "1.5" ] [ S.text (String.fromInt index) ])
                |> Maybe.withDefault (S.text "")
    in
    S.g
        (List.concat
            [ A.classes "line" fig
            , A.transform fig
            ]
        )
        (smoothLine (A.style fig) fig.model.data :: groupLabel :: points)


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
    , H.text <| "numpoints: " ++ String.fromInt (Polyline2d.vertices line |> List.length)
    ]


smoothLine : List (S.Attribute msg) -> Line -> Svg msg
smoothLine attrs line =
    case Polyline2d.vertices line of
        [] ->
            S.text ""

        origin :: rest ->
            S.path (pathAttribute origin rest :: attrs) []


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
