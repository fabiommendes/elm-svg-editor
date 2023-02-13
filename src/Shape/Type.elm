module Shape.Type exposing (..)

import Geometry.CtxPoint exposing (CtxPoint)


type alias Point =
    ()


type Any
    = PointModel Point
    | LineModel Line
    | TextModel Text
    | ImageModel Image


type alias Map =
    { line : Line -> Line
    , text : Text -> Text
    , image : Image -> Image
    }


type alias Replace a =
    { line : Line -> a
    , text : Text -> a
    , point : Point -> a
    , image : Image -> a
    }


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
