module Shape.Toolbar exposing (..)

{-| Renders toolbar buttons
-}

import Element exposing (Element)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Lens as L
import Material.Icons as I
import Monocle.Lens as L
import Msg exposing (Msg(..))
import Shape.Line as Line
import Shape.Type exposing (Any(..), Fill(..), Line)
import Types exposing (..)
import Ui


toolbar : Element -> List (Html Msg)
toolbar ({ figure } as elem) =
    case elem.shape of
        LineModel shape ->
            let
                action : Description -> (Line -> Line) -> H.Attribute Msg
                action name f =
                    HE.onClick <| OnFigureUpdate name (\_ -> Just <| { figure | shape = LineModel (f shape) }) elem.key
            in
            [ Ui.toolbarBtn [ action "add-point" (Line.withValidSubkey elem.subKey Line.insertMiddlePoint) ] I.add
            , Ui.toolbarBtn [ action "remove-point" (Line.withValidSubkey elem.subKey Line.removePoint) ] I.remove
            , H.span [ HA.class "px-2" ] [ H.text "|" ]
            , Ui.toolbarBtn [ action "close-line" (L.fill.set Closed) ] I.pentagon
            , Ui.toolbarBtn [ action "open-line" (L.fill.set Open) ] I.show_chart
            , Ui.toolbarBtn [ action "left-line" (L.fill.set Left) ] I.stacked_line_chart
            , Ui.toolbarBtn [ action "right-line" (L.fill.set Right), HA.style "transform" "scaleX(-1)" ] I.stacked_line_chart
            ]

        _ ->
            []
