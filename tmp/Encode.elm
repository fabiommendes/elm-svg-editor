module Encode exposing (..)

import Types exposing (..)
import Json.Encode exposing (..)
import Length exposing (inMeters)
import Point2d
import Polyline2d


pair : ( Float, Float ) -> Value
pair ( x, y ) =
    object
        [ ( "x", float x )
        , ( "y", float y )
        ]


point : Point -> Value
point =
    Point2d.toTuple inMeters >> pair


vector : Point -> Value
vector =
    Point2d.toTuple inMeters >> pair


line : Line -> Value
line =
    Polyline2d.vertices >> list point


text : Text -> Value
text txt =
    object
        [ ( "data", string txt.data )
        ]


mark : Mark -> Value
mark { id, label } =
    object
        [ ( "id", int id )
        , ( "label", string label )
        ]



image : Image -> E.Value
image img =
    let
        ( x, y ) =
            Point.toTuple img.location
    in
    E.object
        [ ( "href", E.float y )
        , ( "x", E.float x )
        , ( "y", E.float y )
        ]



mark : Mark -> E.Value
mark mark =
    let
        ( x, y ) =
            Point.toTuple mark.location
    in
    E.object
        [ ( "id", E.int mark.id )
        , ( "label", E.string mark.label )
        , ( "x", E.float x )
        , ( "y", E.float y )
        ]




text : Text -> E.Value
text text =
    let
        ( x, y ) =
            Point.toTuple text.location
    in
    E.object
        [ ( "data", E.string text.data )
        , ( "x", E.float x )
        , ( "y", E.float y )
        ]
