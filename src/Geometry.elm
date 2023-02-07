module Geometry exposing (..)

import Angle exposing (Angle, inDegrees)
import BoundingBox2d
import Circle2d
import Length exposing (Meters, inMeters, meters)
import Point2d
import Polyline2d
import Vector2d


type alias Angle =
    Angle.Angle


type alias LengthUnit =
    Meters


type alias Coordinates =
    ()


type alias Point =
    Point2d.Point2d LengthUnit Coordinates


type alias Vector =
    Vector2d.Vector2d LengthUnit Coordinates


type alias Line =
    Polyline2d.Polyline2d LengthUnit Coordinates


type alias Circle =
    Circle2d.Circle2d LengthUnit Coordinates


type alias BBox =
    BoundingBox2d.BoundingBox2d LengthUnit Coordinates


{-| Create a angle from float value, in degrees
-}
angle : Float -> Angle
angle =
    Angle.degrees


{-| Return angle in degrees
-}
fromAngle : Angle -> Float
fromAngle =
    inDegrees


vector : ( Float, Float ) -> Vector
vector =
    Vector2d.fromTuple meters


fromVector : Vector -> ( Float, Float )
fromVector =
    Vector2d.toTuple inMeters


point : ( Float, Float ) -> Point
point =
    Point2d.fromTuple meters


fromPoint : Point -> ( Float, Float )
fromPoint =
    Point2d.toTuple inMeters


line : List ( Float, Float ) -> Line
line =
    List.map point >> Polyline2d.fromVertices


fromLine : Line -> List ( Float, Float )
fromLine =
    Polyline2d.vertices >> List.map fromPoint
