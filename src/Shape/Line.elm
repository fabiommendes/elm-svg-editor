module Shape.Line exposing
    ( Line
    , actionButtons
    , movePoint
    , vertices
    , view
    , withValidSubkey
    )

import Attributes as SA
import Config exposing (Params)
import Element exposing (Element)
import Figure exposing (Figure)
import Geometry exposing (Point, Vector, point)
import Geometry.CtxPoint exposing (midpoint, pointCtx, translateBy)
import Geometry.Paths exposing (pairsClosing, pairsWithExtrapolation)
import Geometry.Svg as S
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Lens as L
import List.Extra as List
import Material.Icons as I
import Monocle.Lens as L
import Msg exposing (Msg(..))
import Shape.Types exposing (Fill(..), Line)
import Shape.View exposing (circle)
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Types exposing (..)
import Ui
import Util exposing (flip, iff)
import Vector2d


type alias Line =
    Shape.Types.Line


vertices : Line -> List Point
vertices pt =
    pt.vertices |> List.map .point


{-| Move the i-th internal point by the given displacement
-}
movePoint : Int -> Vector -> Figure Line -> Figure Line
movePoint i delta ({ shape } as fig) =
    if i == -1 then
        { fig
            | translation = Vector2d.sum [ fig.translation, delta ]
            , shape = { shape | vertices = shape.vertices |> List.map (translateBy (Vector2d.reverse delta)) }
        }

    else
        fig |> L.shape.set { shape | vertices = shape.vertices |> List.updateAt i (translateBy delta) }


{-| Add new point in the i-th position
-}
insertMiddlePoint : Int -> Line -> Line
insertMiddlePoint i line =
    let
        ( before, after ) =
            (pointCtx ( 0, 0 ) :: line.vertices)
                |> iff (line.fill == Closed) pairsClosing pairsWithExtrapolation
                |> List.splitAt (i + 1)

        before_ =
            before |> List.drop 1 |> List.map Tuple.first

        after_ =
            case after of
                ( pt1, pt2 ) :: rest ->
                    pt1 :: midpoint pt1 pt2 :: List.map Tuple.first rest

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
view cfg elem =
    let
        subKey =
            elem.subKey |> List.getAt 0 |> Maybe.withDefault -1

        points =
            flip List.indexedMap (pointCtx ( 0, 0 ) :: elem.model.shape.vertices) <|
                \j { point } ->
                    let
                        i =
                            j - 1

                        attrs =
                            SA.class (iff (i == subKey) "point selected-point" "point")
                                :: SA.class ("key-" ++ String.fromInt i)
                                :: SA.dragChild [ i ] elem
                    in
                    circle cfg.pointRadius point attrs []
    in
    S.g (SA.classes "line" elem :: SA.transformElement elem :: SA.styles elem) <|
        List.concat
            [ Shape.View.linePaths elem
            , points
            , Shape.View.labels [] elem
            ]


actionButtons : Element Line -> List (Html (Msg Line))
actionButtons elem =
    let
        action : Description -> (Line -> Line) -> H.Attribute (Msg Line)
        action name f =
            HE.onClick <| OnFigureUpdate name (\_ -> Just <| Figure.map f elem.model) elem.key
    in
    [ Ui.toolbarBtn [ action "add-point" (withValidSubkey elem.subKey insertMiddlePoint) ] I.add
    , Ui.toolbarBtn [ action "remove-point" (withValidSubkey elem.subKey removePoint) ] I.remove
    , H.span [ HA.class "px-2" ] [ H.text "|" ]
    , Ui.toolbarBtn [ action "close-line" (L.fill.set Closed) ] I.pentagon
    , Ui.toolbarBtn [ action "open-line" (L.fill.set Open) ] I.show_chart
    , Ui.toolbarBtn [ action "left-line" (L.fill.set Left) ] I.stacked_line_chart
    , Ui.toolbarBtn [ action "right-line" (L.fill.set Right), HA.style "transform" "scaleX(-1)" ] I.stacked_line_chart
    ]


withValidSubkey : SubKey -> (Int -> Line -> Line) -> Line -> Line
withValidSubkey subKey func line =
    case subKey of
        [ i ] ->
            func i line

        _ ->
            line
