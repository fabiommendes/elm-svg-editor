module Shape.Text exposing (Text, view)

import Attributes as A
import Config exposing (Params)
import Element exposing (Element)
import Msg exposing (Msg)
import Shape.Types exposing (Text)
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Types exposing (..)


type alias Text =
    Shape.Types.Text


view : Params -> Element Text -> Svg (Msg Text)
view _ ({ shape } as elem) =
    S.g (A.rootElement "text" elem)
        [ S.text_ [ SA.class "background" ] [ S.text shape.content ]
        , S.text_ [] [ S.text shape.content ]
        ]
