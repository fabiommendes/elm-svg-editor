module Geometry.CtxPoint exposing (..)

import Geometry exposing (Point, Vector, point)
import Length exposing (Length)
import Lens as L
import Monocle.Lens as L
import Point2d
import Types exposing (Key)


{-| A point extended with some
-}
type alias GenericCtxPoint a =
    { ctx : a
    , point : Point
    }


type alias CtxPoint =
    { ctx : Props
    , point : Point
    }


type alias Props =
    { back : Bool
    , breakLine : Bool
    , from : Maybe Key
    }


pointEx : ( Float, Float ) -> CtxPoint
pointEx =
    point >> purePoint


pointPropDefault : Props
pointPropDefault =
    { back = False, breakLine = False, from = Nothing }


purePoint : Point -> CtxPoint
purePoint =
    CtxPoint pointPropDefault


pureVertices : List Point -> List CtxPoint
pureVertices =
    List.map (CtxPoint pointPropDefault)


translateBy : Vector -> CtxPoint -> CtxPoint
translateBy vec =
    L.modify L.point (Point2d.translateBy vec)


midpoint : CtxPoint -> CtxPoint -> CtxPoint
midpoint u v =
    purePoint (Point2d.midpoint u.point v.point)


distanceFrom : CtxPoint -> CtxPoint -> Length
distanceFrom pt1 pt2 =
    Point2d.distanceFrom pt1.point pt2.point


origin : CtxPoint
origin =
    purePoint Point2d.origin
