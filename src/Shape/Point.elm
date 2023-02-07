module Shape.Point exposing (Point, view, viewPoint)

import Attributes as SA
import Element exposing (Element)
import Msg exposing (Msg)
import Svg as S exposing (Attribute, Svg)
import Svg.Attributes as SA
import Types exposing (..)


type alias Point =
    ()


view : Element Point -> Svg (Msg Point)
view fig =
    viewPoint 0.5 ( 0, 0 ) (SA.rootFigure "point" fig) []


viewPoint : Float -> ( Float, Float ) -> List (Attribute msg) -> List (Svg msg) -> Svg msg
viewPoint r ( x, y ) attrs =
    let
        num =
            String.fromFloat
    in
    S.circle ([ SA.cx (num x), SA.cy (num y), SA.r (num r) ] ++ attrs)
