module Attributes exposing (..)

import BoundingBox2d
import Draggable
import Element exposing (Element)
import Figure.Style exposing (toAttribute)
import Geometry exposing (Angle, BBox, Vector, fromAngle, fromVector)
import Html exposing (Attribute)
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


dragRoot : Element a -> List (Attribute (Msg a))
dragRoot { key, model } =
    if model.draggable && model.editable then
        SA.class "drag-root" :: touch ( key, [] )

    else
        SA.class "drag-root" :: click ( key, [] )


dragChild : SubKey -> Element a -> List (Attribute (Msg a))
dragChild subKey parent =
    if parent.model.draggable && parent.model.editable then
        SA.class "drag-child" :: touch ( parent.key, subKey )

    else
        SA.class "drag-child" :: click ( parent.key, subKey )


touch : ( Key, SubKey ) -> List (Attribute (Msg a))
touch id =
    Draggable.mouseTrigger id OnDragMsg :: Draggable.touchTriggers id OnDragMsg


click : ( Key, SubKey ) -> List (Attribute (Msg a))
click id =
    [ Draggable.mouseTrigger id OnDragMsg ]


style : Element a -> List (Attribute msg)
style { model } =
    model.style |> List.map (\{ attr, value } -> toAttribute attr value)


classes : String -> Element a -> List (Attribute msg)
classes name elem =
    SA.class ("key-" ++ String.fromInt elem.key)
        :: SA.class (name ++ "-figure")
        :: (if elem.isSelected then
                if elem.subKey == [] then
                    [ SA.class "selected-figure selected-root" ]

                else
                    [ SA.class "selected-figure selected-child" ]

            else
                []
           )


transform_ : Float -> Angle -> Vector -> Attribute msg
transform_ scale angle translation =
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


transform : Element a -> List (Attribute msg)
transform fig =
    [ transform_ fig.model.scale fig.model.rotation fig.model.translation ]


trans : String -> List Float -> String
trans st nums =
    st ++ "(" ++ String.join " " (List.map String.fromFloat nums) ++ ") "


rootFigure : String -> Element a -> List (Attribute (Msg a))
rootFigure name fig =
    List.concat
        [ dragRoot fig
        , classes name fig
        , style fig
        , transform fig
        ]
