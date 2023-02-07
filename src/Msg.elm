module Msg exposing (..)

import BaseTypes exposing (Direction)
import Browser.Dom exposing (Element)
import Draggable
import Figure exposing (Figure)
import Geometry exposing (BBox, Vector)
import Types exposing (..)


type Msg a
    = NoOp
    | OnDragMsg (Draggable.Msg ( Key, SubKey ))
    | OnDragBy Vector
    | OnWindowResize
    | OnRescaleViewport Element
    | OnFigureChangeOrder Direction Key
    | OnSelectFigure Key SubKey
    | OnFigureCreate (Figure a)
    | OnFigureDiscard Key
    | OnFigureReplace (Figure a) Key
    | OnFigureUpdate Description (() -> Maybe (Figure a)) Key
    | OnGroupInclude Label Key
    | OnGroupOrderChange Direction Key
    | OnChangeViewBox Description (BBox -> BBox)


map : (a -> b) -> Msg a -> Msg b
map f msg =
    case msg of
        NoOp ->
            NoOp

        OnDragMsg x ->
            OnDragMsg x

        OnDragBy vector ->
            OnDragBy vector

        OnWindowResize ->
            OnWindowResize

        OnRescaleViewport elem ->
            OnRescaleViewport elem

        OnFigureChangeOrder dir key ->
            OnFigureChangeOrder dir key

        OnSelectFigure id sub ->
            OnSelectFigure id sub

        OnFigureCreate fig ->
            OnFigureCreate (Figure.map f fig)

        OnFigureDiscard key ->
            OnFigureDiscard key

        OnFigureReplace fig key ->
            OnFigureReplace (Figure.map f fig) key

        OnFigureUpdate label lazy key ->
            OnFigureUpdate label (\_ -> lazy () |> Maybe.map (Figure.map f)) key

        OnGroupOrderChange key idx ->
            OnGroupOrderChange key idx

        OnGroupInclude grp key ->
            OnGroupInclude grp key

        OnChangeViewBox label updater ->
            OnChangeViewBox label updater
