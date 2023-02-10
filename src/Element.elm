module Element exposing (..)

import Figure exposing (Figure)
import Group exposing (GroupInfo)
import Types exposing (..)


{-| An figure in the scene with some context.

Element de-normalize data in figure/scene to avoid passing uncessary scene objects and
re-querying for Key in many contexts

-}
type alias Element fig =
    { key : Key
    , subKey : SubKey
    , group : Maybe GroupInfo
    , isSelected : Bool
    , model : Figure fig
    , shape : fig -- a copy from model.shape
    }


map : (a -> b) -> Element a -> Element b
map f ({ model } as elem) =
    let
        new =
            Figure.map f model
    in
    { key = elem.key
    , subKey = elem.subKey
    , group = elem.group
    , isSelected = elem.isSelected
    , model = new
    , shape = new.shape
    }


withShape : fig -> Element fig -> Element fig
withShape shape ({ model } as elem) =
    { elem | model = { model | shape = shape } }
