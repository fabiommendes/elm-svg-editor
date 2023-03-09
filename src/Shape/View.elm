module Shape.View exposing (..)

import Attributes as SA
import Element
import Geometry exposing (fromPoint, point)
import Geometry.CtxPoint exposing (CtxPoint, midpoint, pointCtx)
import Geometry.Paths exposing (smooth, smooth2, triple)
import Geometry.SvgPath exposing (ghostLinePath, pathD)
import Group exposing (GroupInfo)
import Html as H
import Html.Events exposing (onClick)
import Html.Extra as H
import Internal.Types exposing (Config(..))
import List.Extra as List
import Msg exposing (Msg(..))
import Shape.Line exposing (insertPointAt)
import Shape.Type exposing (..)
import Svg as S exposing (Attribute, Svg)
import Svg.Attributes as SA
import Types exposing (..)
import Util exposing (flip, iff)


type alias Element =
    Element.Element


type alias Msg =
    Msg.Msg


view : Config -> Element -> Svg Msg
view (Cfg cfg) elem =
    case elem.shape of
        PointModel _ ->
            viewAsPoint (Cfg cfg) elem.group elem.figure.label elem.isSelected (SA.rootElement "point" elem) (point ( 0, 0 ))

        LineModel shape ->
            let
                subKey =
                    elem.subKey |> List.getAt 0 |> Maybe.withDefault 0

                points =
                    flip List.indexedMap (pointCtx ( 0, 0 ) :: shape.vertices) <|
                        \i { point } ->
                            let
                                attrs =
                                    SA.class (iff (i == subKey) "point selected-child" "point")
                                        :: SA.class ("child::" ++ String.fromInt i)
                                        :: SA.dragChild [ i ] elem
                            in
                            circle cfg.pointRadius point attrs []

                newPointHandles =
                    if elem.isSelected && elem.subKey /= [] then
                        newPoints (Cfg cfg) subKey shape elem

                    else
                        []
            in
            S.g [ SA.classes "line" elem, SA.transformElement elem ] <|
                List.concat
                    [ linePaths (SA.dragRoot elem) shape
                    , newPointHandles
                    , points
                    , labels [] elem
                    ]

        TextModel shape ->
            S.g (SA.rootElement "text" elem)
                [ S.text_ [ SA.class "background" ] [ S.text shape.content ]
                , S.text_ [] [ S.text shape.content ]
                ]

        ImageModel shape ->
            let
                attrs =
                    SA.width (String.fromFloat shape.width)
                        :: SA.xlinkHref shape.href
                        :: SA.rootElement "image" elem
            in
            S.image attrs []


viewAsPoint : Config -> Maybe GroupInfo -> String -> Bool -> List (Attribute Msg) -> Geometry.Point -> Svg Msg
viewAsPoint (Cfg cfg) group name isSelected attrs pt =
    case ( group, name ) of
        ( Just { index, label }, _ ) ->
            S.g attrs
                [ circle cfg.pointRadius pt [] []
                , S.text_ [ SA.class "group-index" ] [ S.text (String.fromInt (index + 1)) ]
                , if isSelected && name == "" then
                    S.text_ [ SA.class "group-label" ] [ S.text ("(" ++ label ++ ")") ]

                  else if isSelected then
                    S.text_ [ SA.class "group-label" ] [ S.text ("(" ++ label ++ " / " ++ name ++ ")") ]

                  else
                    H.nothing
                ]

        ( _, "" ) ->
            circle cfg.pointRadius pt attrs []

        ( _, _ ) ->
            S.g attrs
                [ circle cfg.pointRadius pt [] []
                , if isSelected then
                    S.text_ [ SA.class "group-label" ] [ S.text ("(" ++ name ++ ")") ]

                  else
                    H.nothing
                ]


newPoints : Config -> Int -> Line -> Element -> List (Svg Msg)
newPoints (Cfg cfg) idx line elem =
    case triple idx line.fill (pointCtx ( 0, 0 ) :: line.vertices) of
        Just ( p1, p2, p3 ) ->
            let
                pre =
                    midpoint p1 p2

                post =
                    midpoint p2 p3

                update pt i _ =
                    Just (insertPointAt i pt line elem.figure)

                attrs pt i =
                    [ SA.class "point"
                    , SA.style "fill-opacity: 0.25"
                    , onClick <|
                        Batch
                            [ OnFigureUpdate "new-point" (update pt i) elem.key
                            , OnSelectFigure elem.key [ i + 1 ]
                            ]
                    ]
            in
            [ circle (cfg.pointRadius * 1.5) pre.point (attrs pre idx) []
            , circle (cfg.pointRadius * 1.5) post.point (attrs post (idx + 1)) []
            ]

        Nothing ->
            []


circle : Float -> Geometry.Point -> List (Attribute msg) -> List (Svg msg) -> Svg msg
circle r pt attrs =
    let
        ( x, y ) =
            fromPoint pt

        num =
            String.fromFloat
    in
    S.circle ([ SA.cx (num x), SA.cy (num y), SA.r (num r) ] ++ attrs)


{-| Create a label, if element has set a label or is part of a group

Return a list of svg.text\_ elements rendering the existing labels.

Usually, those will be placed alongside a circle sibling inside a g [][...] node with the
necessary transforms.

-}
labels : List (H.Attribute msg) -> Element -> List (Svg msg)
labels attrs elem =
    let
        indexLabel idx =
            S.text_ (SA.class "group-index" :: attrs) [ S.text (String.fromInt (idx + 1)) ]
    in
    case ( elem.group, elem.figure.label, elem.isSelected ) of
        ( Just { index, label }, "", True ) ->
            [ indexLabel index, S.text_ (SA.class "label" :: attrs) [ S.text ("(" ++ label ++ ")") ] ]

        ( Just { index, label }, name, True ) ->
            [ indexLabel index, S.text_ (SA.class "label" :: attrs) [ S.text ("(" ++ label ++ " / " ++ name ++ ")") ] ]

        ( Just { index }, _, False ) ->
            [ indexLabel index ]

        ( _, name, True ) ->
            [ S.text_ (SA.class "label" :: attrs) [ S.text ("(" ++ name ++ ")") ] ]

        _ ->
            []


linePaths : List (H.Attribute msg) -> Line -> List (Svg msg)
linePaths attrs shape =
    let
        dAttribute =
            SA.d (pathD (shape.fill == Closed) line)

        line =
            (pointCtx ( 0, 0 ) :: shape.vertices)
                |> smooth 1
                |> smooth2 1

        foregroundClass =
            if shape.fill == Closed then
                "foreground closed"

            else
                "foreground"
    in
    (S.path [ dAttribute, SA.class "background" ] [] :: ghostLinePaths shape.fill line)
        ++ [ S.path (dAttribute :: SA.class foregroundClass :: attrs) [] ]


ghostLinePaths : Fill -> List CtxPoint -> List (Svg msg)
ghostLinePaths fill line =
    let
        ghostPath delta =
            let
                blur =
                    String.fromFloat (abs delta / 2)

                opacity =
                    String.fromFloat (1 - abs delta / 4)

                filter =
                    "blur(" ++ blur ++ "px) opacity(" ++ opacity ++ ")"
            in
            S.path [ SA.d (ghostLinePath delta line), SA.filter filter, SA.class "foreground" ] []

        levels =
            [ 0.1, 0.25, 0.5, 0.8, 1.2 ]
    in
    case fill of
        Right ->
            levels |> List.map ghostPath

        Left ->
            levels |> List.map (negate >> ghostPath)

        _ ->
            []
