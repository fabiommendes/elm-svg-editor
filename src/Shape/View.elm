module Shape.View exposing (..)

import Attributes as SA
import Element exposing (Element)
import Geometry exposing (fromPoint)
import Geometry.CtxPoint exposing (CtxPoint, pointCtx)
import Geometry.Paths exposing (smooth, smooth2)
import Geometry.SvgPath exposing (ghostLinePath, pathD)
import Html
import Msg exposing (Msg)
import Shape.Types exposing (Fill(..), Line)
import Svg as S exposing (Attribute, Svg)
import Svg.Attributes as SA
import Types exposing (..)


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
labels : List (Html.Attribute msg) -> Element a -> List (Svg msg)
labels attrs elem =
    let
        indexLabel idx =
            S.text_ (SA.class "group-index" :: attrs) [ S.text (String.fromInt (idx + 1)) ]
    in
    case ( elem.group, elem.model.label, elem.isSelected ) of
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


linePaths : Element Line -> List (Svg (Msg Line))
linePaths ({ shape } as elem) =
    let
        dAttribute =
            SA.d (pathD (shape.fill == Closed) line)

        line =
            (pointCtx ( 0, 0 ) :: shape.vertices)
                |> smooth 1
                |> smooth2 1

        foregroundClass =
            if shape.fill == Closed then
                "foreground line-closed"

            else
                "foreground"
    in
    (S.path [ dAttribute, SA.class "background" ] [] :: ghostLinePaths shape.fill line)
        ++ [ S.path (dAttribute :: SA.class foregroundClass :: SA.dragRoot elem) [] ]


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
