module Shape.Point exposing (Point, circle, view, viewAsPoint)

import Attributes as SA
import Config exposing (Params)
import Element exposing (Element)
import Geometry exposing (fromPoint, point)
import Group exposing (GroupInfo)
import Html.Extra as H
import Msg exposing (Msg)
import Svg as S exposing (Attribute, Svg)
import Svg.Attributes as SA
import Types exposing (..)


type alias Point =
    ()


view : Params fig -> Element a -> Svg (Msg a)
view cfg elem =
    viewAsPoint cfg elem.group elem.model.label elem.isSelected (SA.rootElement "point" elem) (point ( 0, 0 ))


viewAsPoint : Params fig -> Maybe GroupInfo -> String -> Bool -> List (Attribute (Msg a)) -> Geometry.Point -> Svg (Msg a)
viewAsPoint cfg group name isSelected attrs pt =
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


circle : Float -> Geometry.Point -> List (Attribute msg) -> List (Svg msg) -> Svg msg
circle r pt attrs =
    let
        ( x, y ) =
            fromPoint pt

        num =
            String.fromFloat
    in
    S.circle ([ SA.cx (num x), SA.cy (num y), SA.r (num r) ] ++ attrs)
