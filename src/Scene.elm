module Scene exposing
    ( Scene
    , discard
    , elements
    , get
    , getElement
    , getGroup
    , getGroupOf
    , getSelected
    , group
    , groupMany
    , init
    , insert
    , insertAs
    , insertMany
    , insertManyAs
    , moveGroup
    , moveLayer
    , put
    , update
    , view
    )

import Attributes as A
import BaseTypes exposing (Direction(..))
import BoundingBox2d
import Config exposing (Config)
import Dict exposing (Dict)
import Element exposing (Element)
import Figure exposing (Figure)
import Geometry exposing (..)
import Group exposing (GroupData, GroupInfo)
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events.Extra.Pointer as Pointer
import Lens as L
import List.Extra as List
import Maybe.Extra as Maybe
import Monocle.Common exposing (second)
import Monocle.Lens as L exposing (Lens)
import Msg exposing (Msg(..))
import State exposing (State(..))
import Svg as S
import Svg.Attributes as SA
import Types exposing (..)
import Ui
import Util exposing (flip)


{-| Represent an Svg scene
-}
type alias Scene a =
    { data : Data a
    , selected : Maybe ( Key, SubKey )
    , scale : Float
    , translation : Vector
    , bbox : BBox
    }


type Data a
    = Data (SceneData a)


type alias SceneData a =
    { objects : Objects a
    , groups : GroupData Key
    , order : List Key
    }


type alias Objects a =
    Dict Key ( Figure a, Maybe GroupInfo )


init : Float -> Float -> Scene a
init width height =
    { data =
        Data
            { objects = Dict.empty
            , order = []
            , groups = Group.empty
            }
    , selected = Nothing
    , scale = 1.0
    , translation = vector ( 0, 0 )
    , bbox = BoundingBox2d.from (point ( 0, 0 )) (point ( width, height ))
    }



--
-- ELEMENTS
--


getElement : Key -> Scene a -> Maybe (Element a)
getElement key scene =
    Dict.get key (objects.get scene)
        |> Maybe.map (\( fig, group_ ) -> makeElement scene key group_ fig)


getSelected : Scene a -> Maybe (Element a)
getSelected scene =
    scene.selected
        |> Maybe.andThen
            (\( key, subKey ) ->
                Dict.get key (objects.get scene)
                    |> Maybe.map
                        (\( fig, group_ ) ->
                            { key = key
                            , subKey = subKey
                            , isSelected = True
                            , group = group_
                            , model = fig
                            , shape = fig.shape
                            }
                        )
            )


{-| Return all elements in scene in the same order as scene.order
-}
elements : Scene a -> List (Element a)
elements scene =
    order.get scene |> List.filterMap (flip getElement scene)



--
-- FIGURES
--


get : Key -> Scene a -> Maybe (Figure a)
get key =
    objects.get >> Dict.get key >> Maybe.map Tuple.first


update : Key -> (Figure a -> Figure a) -> Scene a -> Scene a
update key func =
    let
        updater ( fig, grp ) =
            ( func fig, grp )
    in
    L.modify objects <| Dict.update key (Maybe.map updater)


{-| Put figure in scene at the outmost layer.
-}
put : Key -> Figure a -> Scene a -> Scene a
put key fig =
    L.modify data
        (\inner ->
            { inner
                | objects = Dict.insert key ( fig, Nothing ) inner.objects
                , order = orderWithKey key inner.order
            }
        )


insert : Figure a -> Scene a -> ( Key, Scene a )
insert =
    insertAs "obj"


insertAs : String -> Figure a -> Scene a -> ( Key, Scene a )
insertAs key fig scene =
    let
        key_ =
            Dict.keys (objects.get scene)
                |> List.filter (Tuple.first >> (==) key)
                |> List.maximum
                |> Maybe.unwrap ( key, 0 ) nextKey
    in
    ( key_, put key_ fig scene )


insertMany : List (Figure a) -> Scene a -> Scene a
insertMany =
    insertManyAs "obj"


insertManyAs : String -> List (Figure a) -> Scene a -> Scene a
insertManyAs key figures =
    L.modify data <|
        \inner ->
            let
                firstKey =
                    Dict.keys inner.objects
                        |> List.filter (Tuple.first >> (==) key)
                        |> List.maximum
                        |> Maybe.unwrap ( key, 0 ) nextKey

                extra =
                    figures |> List.indexedMap (\i x -> ( firstKey |> nextKeyBy i, ( x, Nothing ) ))
            in
            { inner
                | objects = Dict.union inner.objects (Dict.fromList extra)
                , order = inner.order ++ List.map Tuple.first extra
            }


discard : Key -> Scene a -> Scene a
discard key =
    L.modify data <|
        \inner ->
            { inner
                | objects = Dict.remove key inner.objects
                , order = List.filter ((/=) key) inner.order
            }



--
-- GROUPS
--


{-| Get list of keys for the given group label
-}
getGroup : Label -> Scene a -> List Key
getGroup label scene =
    groups.get scene
        |> Dict.get label
        |> Maybe.withDefault []


{-| Return GroupInfo associated with key
-}
getGroupOf : Key -> Scene a -> Maybe GroupInfo
getGroupOf key =
    objects.get >> Dict.get key >> Maybe.andThen Tuple.second


moveGroup : Direction -> Key -> Scene a -> Scene a
moveGroup direction key scene =
    groups.get scene
        |> Group.move direction key
        |> flip updateGroups scene


{-| Register key to group
-}
group : Key -> Label -> Scene a -> Scene a
group key label scene =
    Group.remove key (groups.get scene)
        |> Group.insert key label
        |> flip updateGroups scene


groupMany : Dict Key Label -> Scene a -> Scene a
groupMany grps scene =
    (Group.toList (groups.get scene) ++ Dict.toList grps)
        |> Group.fromList
        |> flip updateGroups scene


{-| Update inner structure from groupings
-}
updateGroups : GroupData Key -> Scene a -> Scene a
updateGroups groups_ =
    L.modify data <|
        \inner ->
            { inner
                | groups = groups_
                , objects = Group.updateGroupInfo groups_ second.set inner.objects
            }



--
-- ORDERING
--


moveLayer : Direction -> Key -> Scene a -> Scene a
moveLayer direction key =
    let
        run objs =
            case ( direction, objs ) of
                ( Down, k1 :: k2 :: rest ) ->
                    if k2 == key then
                        k2 :: k1 :: rest

                    else
                        k1 :: (run <| k2 :: rest)

                ( Up, k1 :: k2 :: rest ) ->
                    if k1 == key then
                        k2 :: k1 :: rest

                    else
                        k1 :: (run <| k2 :: rest)

                _ ->
                    objs
    in
    L.modify order run


view : Config a -> Scene a -> Html (Msg a)
view cfg scene =
    let
        elementsSvg =
            elements scene
                |> List.filter (.model >> .visible)
                |> List.map (cfg.config.view cfg.params)

        mapMsg f =
            List.map (S.map f)

        pointerEvents state panWithTouch =
            case ( state, panWithTouch ) of
                ( ClickToInsert _ _, _ ) ->
                    Pointer.onDown (.pointer >> .offsetPos >> point >> OnClickAt) :: pointerEvents ReadOnlyView panWithTouch

                ( _, True ) ->
                    A.touch ( backgroundKey, [] )

                _ ->
                    []

        supressDrag =
            mapMsg (Msg.changeDragMsgsTo Msg.NoOp)
    in
    H.div [ HA.class "container bg-slate-100", HA.class "scene", HA.id cfg.params.sceneId ]
        [ S.svg
            (SA.width "100%" :: SA.class "scene" :: A.viewBox scene.bbox :: pointerEvents cfg.params.state cfg.params.panWithTouch)
            (case cfg.params.state of
                ReadOnlyView ->
                    supressDrag elementsSvg

                ClickToInsert _ _ ->
                    supressDrag elementsSvg

                _ ->
                    elementsSvg
            )
        , Ui.controls cfg
        ]


orderWithKey : Key -> List Key -> List Key
orderWithKey key lst =
    if List.member key lst then
        lst

    else
        lst ++ [ key ]


makeElement : Scene a -> Key -> Maybe GroupInfo -> Figure a -> Element a
makeElement scene key group_ fig =
    case scene.selected of
        Just ( key_, subKey ) ->
            if key == key_ then
                { key = key
                , subKey = subKey
                , group = group_
                , isSelected = True
                , model = fig
                , shape = fig.shape
                }

            else
                { key = key
                , subKey = []
                , group = group_
                , isSelected = False
                , model = fig
                , shape = fig.shape
                }

        _ ->
            { key = key
            , subKey = []
            , group = group_
            , isSelected = False
            , model = fig
            , shape = fig.shape
            }



---
--- Utilities
---


data : Lens (Scene a) (SceneData a)
data =
    let
        getter scene =
            case scene.data of
                Data x ->
                    x

        setter x scene =
            { scene | data = Data x }
    in
    Lens getter setter


objects : Lens (Scene a) (Objects a)
objects =
    L.compose data L.objects


order : Lens (Scene a) (List Key)
order =
    L.compose data L.order


groups : Lens (Scene a) (GroupData Key)
groups =
    L.compose data L.groups
