module Shape.Image exposing (Image, view)

import Attributes as A
import Config exposing (Params)
import Element exposing (Element)
import Msg exposing (Msg)
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Types exposing (..)


type alias Image =
    { href : String
    , width : Float
    }


view : Params fig -> Element Image -> Svg (Msg Image)
view _ ({ shape } as elem) =
    let
        attrs =
            SA.width (String.fromFloat shape.width)
                :: SA.xlinkHref shape.href
                :: A.rootElement "image" elem
    in
    S.image attrs []
