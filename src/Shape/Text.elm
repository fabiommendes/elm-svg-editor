module Shape.Text exposing (Text, view)

import Attributes
import Element exposing (Element)
import Msg exposing (Msg)
import Shape.Point exposing (viewPoint)
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Types exposing (..)


type alias Text =
    String


view : Element Text -> Svg (Msg Text)
view fig =
    S.g (Attributes.dragRoot fig)
        [ viewPoint 1 ( 0, 0 ) [] []
        , S.text_ [ SA.fontSize "1.25" ] [ S.text fig.model.data ]
        ]
