module Shape.Image exposing (Image, view)

import Attributes as A
import Config exposing (ConfigParams)
import Element exposing (Element)
import Msg exposing (Msg)
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Types exposing (..)


type alias Image =
    { href : String
    , width : Float
    }


view : ConfigParams -> Element Image -> Svg (Msg Image)
view _ elem =
    let
        attrs =
            List.concat
                [ [ SA.width (String.fromFloat elem.model.data.width)
                  , SA.xlinkHref elem.model.data.href
                  , A.transformElement elem
                  ]
                , A.dragRoot elem
                , A.classes "image" elem
                ]
    in
    S.image attrs []
