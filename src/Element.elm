module Element exposing (..)

import Figure exposing (Figure)
import Group exposing (GroupInfo)
import Types exposing (..)


{-| An shape in the scene with some context.
-}
type alias Element a =
    { key : Key
    , subKey : SubKey
    , group : Maybe GroupInfo
    , isSelected : Bool
    , model : Figure a
    }


map : (a -> b) -> Element a -> Element b
map f e =
    { key = e.key
    , subKey = e.subKey
    , group = e.group
    , isSelected = e.isSelected
    , model = Figure.map f e.model
    }
