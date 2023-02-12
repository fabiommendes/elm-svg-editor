module Shape.Line exposing
    ( Fill(..)
    , Line
    , actionButtons
    , movePoint
    , vertices
    , view
    )

import Attributes as SA
import Config exposing (Params)
import Element exposing (Element)
import Figure exposing (move)
import Geometry exposing (Point, Vector, fromPoint, point, vector)
import Geometry.CtxPoint as PointExt exposing (CtxPoint)
import Geometry.Paths exposing (pairsWithExtrapolation, smooth, smooth2)
import Geometry.Svg as S
import Geometry.SvgPath exposing (ghostLinePath, pathD)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Lens as L
import List.Extra as List
import Monocle.Lens as L
import Msg exposing (Msg(..))
import Point2d
import Shape.Point exposing (circle)
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Types exposing (..)
import Util exposing (iff)


type alias Line =
    { vertices : List CtxPoint
    , duplicateLast : Bool
    , fill : Fill
    }


type Fill
    = Open
    | Closed
    | Left
    | Right


vertices : Line -> List Point
vertices pt =
    pt.vertices |> List.map .point


{-| Move the i-th internal point by the given displacement
-}
movePoint : Int -> Vector -> Line -> Line
movePoint i displacement =
    L.modify L.vertices <| List.updateAt i (L.modify L.point (Point2d.translateBy displacement))


{-| Add new point in the i-th position
-}
insertPoint : Int -> Line -> Line
insertPoint i line =
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
                    pt1 :: PointExt.midpoint pt1 pt2 :: List.map Tuple.first rest

                _ ->
                    []
    in
    L.vertices.set (before_ ++ after_) line


{-| Remove the i-th point from line
-}
removePoint : Int -> Line -> Line
removePoint i =
    L.modify L.vertices (List.removeAt i)


{-| Render line
-}
view : Params -> Element Line -> Svg (Msg Line)
view cfg ({ model, shape } as elem) =
    let
        subKey =
            elem.subKey |> List.getAt 0 |> Maybe.withDefault -1

        makePoint i pte =
            let
                pt =
                    pte.point

                isLast =
                    i == nPoints - 1 && model.shape.duplicateLast

                elem_ =
                    if isLast then
                        L.modify L.model (move (vector (fromPoint pt))) elem

                    else
                        elem

                attrs =
                    SA.class (iff (i == subKey) "point selected-point" "point")
                        :: SA.class ("key-" ++ String.fromInt i)
                        :: SA.childPart [ i ] "line" elem_
            in
            if isLast then
                Shape.Point.viewAsPoint cfg elem.group model.label elem.isSelected (SA.class "last-point" :: attrs) (point ( 0, 0 ))

            else
                circle cfg.pointRadius pt attrs []

        firstPoint =
            Shape.Point.view cfg elem

        nPoints =
            elem.model.shape.vertices |> List.length

        points =
            firstPoint :: List.indexedMap makePoint elem.model.shape.vertices

        labels =
            [ elem.group
                |> Maybe.map (\{ index } -> S.text_ [ SA.fontSize "1.5" ] [ S.text (String.fromInt index) ])
                |> Maybe.withDefault (S.text "")
            ]

        dAttribute =
            SA.d (pathD (shape.fill == Closed) line)

        transforms =
            SA.transformElement elem

        line =
            (PointExt.pointEx ( 0, 0 ) :: shape.vertices)
                |> smooth 1
                |> smooth2 1

        -- |> smooth 1
        lines =
            [ S.path [ dAttribute, transforms, SA.class "background" ] []
            , S.path [ SA.d (ghostLinePath 0.1 line), SA.filter "blur(0.05px) opacity(0.9)", transforms, SA.class "foreground" ] []
            , S.path [ SA.d (ghostLinePath 0.25 line), SA.filter "blur(0.125px) opacity(0.7)", transforms, SA.class "foreground" ] []
            , S.path [ SA.d (ghostLinePath 0.5 line), SA.filter "blur(0.25px) opacity(0.5)", transforms, SA.class "foreground" ] []
            , S.path [ SA.d (ghostLinePath 0.8 line), SA.filter "blur(0.4px) opacity(0.3)", transforms, SA.class "foreground" ] []
            , S.path [ SA.d (ghostLinePath 1.2 line), SA.filter "blur(0.6px) opacity(0.1)", transforms, SA.class "foreground" ] []
            , S.path (dAttribute :: transforms :: SA.class "foreground" :: SA.dragRoot elem) []
            ]
    in
    S.g (SA.classes "line" elem ++ SA.styles elem) <|
        List.concat [ lines, labels, points ]


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
                            |> L.shape.set (func i line)
                        )
                )
                fig.key
    in
    [ H.button [ HA.class "btn", HE.onClick (action "add-point" insertPoint) ] [ H.text "add point" ]
    , H.button [ HA.class "btn", HE.onClick (action "remove-point" removePoint) ] [ H.text "remove point" ]
    , H.text <| "numpoints: " ++ String.fromInt (List.length line.vertices + 1)
    ]
