module Element exposing (..)

import Figure exposing (Figure)
import Group exposing (GroupInfo)
import Shape.Type exposing (Any)
import Types exposing (..)


{-| An figure in the scene with some context.

Element de-normalize data in figure/scene to avoid passing uncessary scene objects and
re-querying for Key in many contexts

-}
type alias Element =
    { key : Key
    , subKey : SubKey
    , group : Maybe GroupInfo
    , isSelected : Bool
    , isEditable : Bool
    , isVisible : Bool
    , figure : Figure
    , shape : Any
    }


map : (Any -> Any) -> Element -> Element
map f ({ figure } as elem) =
    let
        new =
            Figure.map f figure
    in
    { key = elem.key
    , subKey = elem.subKey
    , group = elem.group
    , isSelected = elem.isSelected
    , isEditable = elem.isEditable
    , isVisible = elem.isVisible
    , figure = new
    , shape = new.shape
    }


withShape : Any -> Element -> Element
withShape shape ({ figure } as elem) =
    { elem | figure = { figure | shape = shape } }
