module Shape.Text exposing (Text, view)

import Attributes as A
import Config exposing (ConfigParams)
import Element exposing (Element)
import Msg exposing (Msg)
import Svg as S exposing (Svg)
import Svg.Attributes as SA
import Types exposing (..)


type alias Text =
    { content : String }


view : ConfigParams -> Element Text -> Svg (Msg Text)
view _ elem =
    S.g (A.rootElement "text" elem)
        [ S.text_ [ SA.class "background" ] [ S.text elem.model.data.content ]
        , S.text_ [] [ S.text elem.model.data.content ]
        ]
