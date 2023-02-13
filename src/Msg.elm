module Msg exposing (..)

import BaseTypes exposing (Direction)
import BoundingBox2d as BBox
import Browser.Dom exposing (Element)
import Draggable
import Figure exposing (Figure)
import File exposing (File)
import Geometry exposing (..)
import Length exposing (inMeters)
import State exposing (State)
import Types exposing (..)


type KeyBoardCommands
    = Delete
    | DeletePart
    | Undo
    | Redo
    | PanRight
    | PanLeft
    | PanUp
    | PanDown


type Msg a
    = NoOp
    | Batch (List (Msg a))
    | OnErrorDetected String String
    | OnDragMsg (Draggable.Msg ( Key, SubKey ))
    | OnDragBy Vector
    | OnWindowResize
    | OnViewportRescaled Element
    | OnClickAt Point
    | OnKeyPress KeyBoardCommands
    | OnFigureChangeOrder Direction Key
    | OnSelectFigure Key SubKey
    | OnFigureCreate (Figure a)
    | OnFigureDiscard Key
    | OnFigureReplace (Figure a) Key
    | OnFigureUpdate Description (() -> Maybe (Figure a)) Key
    | OnGroupInclude Label Key
    | OnGroupOrderChange Direction Key
    | OnChangeViewBox Description (BBox -> BBox)
    | OnDownloadRequest
    | OnUploadRequest
    | OnUploadCompleted File
    | OnUploadProcessed String
    | OnStateChange (State a)
    | OnUndo
    | OnRedo


map : (a -> b) -> Msg a -> Msg b
map f msg =
    case msg of
        NoOp ->
            NoOp

        Batch lst ->
            Batch (List.map (map f) lst)

        OnErrorDetected s1 s2 ->
            OnErrorDetected s1 s2

        OnDragMsg x ->
            OnDragMsg x

        OnDragBy vector ->
            OnDragBy vector

        OnWindowResize ->
            OnWindowResize

        OnViewportRescaled elem ->
            OnViewportRescaled elem

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

        OnDownloadRequest ->
            OnDownloadRequest

        OnUploadRequest ->
            OnUploadRequest

        OnUploadCompleted file ->
            OnUploadCompleted file

        OnUploadProcessed st ->
            OnUploadProcessed st

        OnClickAt pt ->
            OnClickAt pt

        OnStateChange st ->
            OnStateChange (State.map f st)

        OnKeyPress cmd ->
            OnKeyPress cmd

        OnUndo ->
            OnUndo

        OnRedo ->
            OnRedo


changeDragMsgsTo : Msg fig -> Msg fig -> Msg fig
changeDragMsgsTo default msg =
    case msg of
        OnDragMsg _ ->
            default

        OnDragBy _ ->
            default

        _ ->
            msg


pan : Float -> Float -> BBox -> BBox
pan x y bb =
    let
        step =
            BBox.dimensions bb
                |> Tuple.first
                |> inMeters
                |> (*) 0.15
    in
    BBox.translateBy (vector ( -step * x, -step * y )) bb


panUp : Msg a
panUp =
    OnChangeViewBox "pan.up" (pan 0 1)


panDown : Msg a
panDown =
    OnChangeViewBox "pan.down" (pan 0 -1)


panRight : Msg a
panRight =
    OnChangeViewBox "pan.right" (pan -1 0)


panLeft : Msg a
panLeft =
    OnChangeViewBox "pan.left" (pan 1 0)


panArrow : KeyBoardCommands -> Msg a
panArrow cmd =
    case cmd of
        PanRight ->
            panRight

        PanLeft ->
            panLeft

        PanUp ->
            panUp

        PanDown ->
            panDown

        _ ->
            NoOp
