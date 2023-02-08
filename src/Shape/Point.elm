module Shape.Point exposing (Point, circle, view, viewAsPoint)

import Attributes as SA
import Config exposing (ConfigParams)
import Element exposing (Element)
import Html.Extra as H
import Msg exposing (Msg)
import Svg as S exposing (Attribute, Svg)
import Svg.Attributes as SA
import Types exposing (..)


type alias Point =
    ()


view : ConfigParams -> Element a -> Svg (Msg a)
view cfg elem =
    viewAsPoint cfg (SA.rootFigure "point" elem) elem


viewAsPoint : ConfigParams -> List (Attribute (Msg a)) -> Element a -> Svg (Msg a)
viewAsPoint cfg attrs elem =
    case ( elem.group, elem.model.label ) of
        ( Just { index, label }, name ) ->
            S.g attrs
                [ circle cfg.pointRadius ( 0, 0 ) [] []
                , S.text_ [ SA.class "group-index" ] [ S.text (String.fromInt (index + 1)) ]
                , if elem.isSelected && name == "" then
                    S.text_ [ SA.class "group-label" ] [ S.text ("(" ++ label ++ ")") ]

                  else if elem.isSelected then
                    S.text_ [ SA.class "group-label" ] [ S.text ("(" ++ label ++ " / " ++ name ++ ")") ]

                  else
                    H.nothing
                ]

        ( _, "" ) ->
            circle cfg.pointRadius ( 0, 0 ) attrs []

        ( _, name ) ->
            S.g attrs
                [ circle cfg.pointRadius ( 0, 0 ) [] []
                , if elem.isSelected then
                    S.text_ [ SA.class "group-label" ] [ S.text ("(" ++ name ++ ")") ]

                  else
                    H.nothing
                ]


circle : Float -> ( Float, Float ) -> List (Attribute msg) -> List (Svg msg) -> Svg msg
circle r ( x, y ) attrs =
    let
        num =
            String.fromFloat
    in
    S.circle ([ SA.cx (num x), SA.cy (num y), SA.r (num r) ] ++ attrs)
