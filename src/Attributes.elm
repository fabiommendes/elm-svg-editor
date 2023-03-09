module Attributes exposing (..)

import BoundingBox2d
import Draggable
import Element exposing (Element)
import Geometry exposing (Angle, BBox, Vector, fromAngle, fromVector)
import Html exposing (Attribute)
import Html.Events exposing (onClick)
import Length exposing (inMeters)
import Msg exposing (Msg(..))
import Quantity as Q
import Svg.Attributes as SA
import Types exposing (..)


viewBox : BBox -> Attribute msg
viewBox bb =
    let
        { minX, minY, maxX, maxY } =
            BoundingBox2d.extrema bb
    in
    SA.viewBox
        ([ minX, minY, Q.difference maxX minX, Q.difference maxY minY ]
            |> List.map (inMeters >> String.fromFloat)
            |> String.join " "
        )


dragRoot : Element -> List (Attribute Msg)
dragRoot { key, isEditable, isVisible } =
    if isEditable && isVisible then
        SA.class "drag-root" :: touch ( key, [] )

    else
        [ SA.class "drag-root", onClick (OnSelectFigure key []) ]


dragChild : SubKey -> Element -> List (Attribute Msg)
dragChild subKey parent =
    if parent.isEditable && parent.isEditable then
        SA.class "drag-child" :: touch ( parent.key, subKey )

    else
        [ SA.class "drag-child" ]


touch : ( Key, SubKey ) -> List (Attribute Msg)
touch id =
    Draggable.mouseTrigger id OnDragMsg :: Draggable.touchTriggers id OnDragMsg


click : ( Key, SubKey ) -> List (Attribute Msg)
click id =
    [ Draggable.mouseTrigger id OnDragMsg ]


classes : String -> Element -> Attribute msg
classes name elem =
    SA.class
        << String.join " "
    <|
        ("key_" ++ showKey elem.key)
            :: name
            :: (if elem.isSelected then
                    if elem.subKey == [] then
                        [ "selected" ]

                    else
                        [ "selected selected-child" ]

                else
                    []
               )


transformFrom : Float -> Angle -> Vector -> Attribute msg
transformFrom scale angle translation =
    let
        ( x, y ) =
            fromVector translation

        factor =
            1.0 / scale
    in
    SA.transform
        (trans "rotate" [ 0, 0, fromAngle angle ]
            ++ trans "scale" [ scale ]
            ++ trans "translate" [ factor * x, factor * y ]
        )


transformElement : Element -> Attribute msg
transformElement fig =
    transformFrom fig.figure.scale fig.figure.rotation fig.figure.translation


trans : String -> List Float -> String
trans st nums =
    st ++ "(" ++ String.join " " (List.map String.fromFloat nums) ++ ") "


rootElement : String -> Element -> List (Attribute Msg)
rootElement name elem =
    classes name elem
        :: transformElement elem
        :: dragRoot elem


childPart : SubKey -> String -> Element -> List (Attribute Msg)
childPart sub name fig =
    classes name fig
        :: transformElement fig
        :: SA.class "child"
        :: dragChild sub fig
