module Shape.Types exposing (..)

import Geometry.CtxPoint exposing (CtxPoint)


type alias Point =
    ()


type Fill
    = Open
    | Closed
    | Left
    | Right


type alias Line =
    { vertices : List CtxPoint
    , duplicateLast : Bool
    , fill : Fill
    }


type alias Image =
    { href : String
    , width : Float
    }


type alias Text =
    { content : String }
