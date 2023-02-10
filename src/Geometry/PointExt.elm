module Geometry.PointExt exposing (..)

import Geometry exposing (Point, Vector, point)
import Lens as L
import Monocle.Lens as L
import Point2d
import Types exposing (Key)


type alias PointProp =
    { back : Bool
    , breakLine : Bool
    , from : Maybe Key
    }


type alias PointExt =
    { props : PointProp
    , point : Point
    }


pointExt : ( Float, Float ) -> PointExt
pointExt =
    point >> purePoint


pointPropDefault : PointProp
pointPropDefault =
    { back = False, breakLine = False, from = Nothing }


purePoint : Point -> PointExt
purePoint =
    PointExt pointPropDefault


pureVertices : List Point -> List PointExt
pureVertices =
    List.map (PointExt pointPropDefault)


translateBy : Vector -> PointExt -> PointExt
translateBy vec =
    L.modify L.point (Point2d.translateBy vec)


midpoint : PointExt -> PointExt -> PointExt
midpoint u v =
    purePoint (Point2d.midpoint u.point v.point)
