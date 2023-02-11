module Geometry.PointEx exposing (..)

import Geometry exposing (Point, Vector, point)
import Lens as L
import Monocle.Lens as L
import Point2d
import Types exposing (Key)


{-| A point extended with some
-}
type alias TaggedPoint a =
    { props : a
    , point : Point
    }


type alias PointEx =
    { props : Props
    , point : Point
    }


type alias Props =
    { back : Bool
    , breakLine : Bool
    , from : Maybe Key
    }


pointEx : ( Float, Float ) -> PointEx
pointEx =
    point >> purePoint


pointPropDefault : Props
pointPropDefault =
    { back = False, breakLine = False, from = Nothing }


purePoint : Point -> PointEx
purePoint =
    PointEx pointPropDefault


pureVertices : List Point -> List PointEx
pureVertices =
    List.map (PointEx pointPropDefault)


translateBy : Vector -> PointEx -> PointEx
translateBy vec =
    L.modify L.point (Point2d.translateBy vec)


midpoint : PointEx -> PointEx -> PointEx
midpoint u v =
    purePoint (Point2d.midpoint u.point v.point)
