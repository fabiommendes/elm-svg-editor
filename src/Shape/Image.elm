module Shape.Image exposing (Image, view)

import Attributes
import Msg exposing (Msg)
import Element exposing (Element)
import Shape.Point exposing (viewPoint)
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Types exposing (..)


type alias Image =
    { href : String
    }


view : Element Image -> Svg (Msg Image)
view fig =
    S.g (Attributes.dragRoot fig)
        [ viewPoint 1 ( 0, 0 ) [] []
        , S.text_ [ SA.fontSize "1.25" ] [ S.text fig.model.data.href ]
        ]
