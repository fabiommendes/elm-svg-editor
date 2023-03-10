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
            [ Ui.toolbarBtn [ action "remove-point" (Line.withValidSubkey elem.subKey Line.removePoint) ] I.remove
            , H.span [ HA.class "px-2" ] [ H.text "|" ]
            , lineFillToggle action shape
            ]

        _ ->
            []


lineFillToggle : (Description -> (Line -> Line) -> H.Attribute Msg) -> Line -> Html Msg
lineFillToggle action shape =
    let
        do name fill =
            Ui.toolbarBtn [ action name (L.fill.set fill) ] I.border_style
    in
    case shape.fill of
        Open ->
            do "close-line" Closed

        Closed ->
            do "left-line" Left

        Left ->
            do "right-line" Right

        Right ->
            do "open-line" Open
