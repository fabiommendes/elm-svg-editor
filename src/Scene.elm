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
    , keys
    , moveGroup
    , moveLayer
    , pop
    , put
    , select
    , selected
    , update
    , view
    )

import Attributes as A
import BaseTypes exposing (Direction(..))
import Config exposing (Config)
import Dict exposing (Dict)
import Dict.Extra as Dict
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
type Scene a
    = Scene
        { objects : Objects a
        , groups : GroupData Key
        , order : List Key
        , selected : Maybe ( Key, SubKey )
        }


type alias Objects a =
    Dict Key ( Figure a, Maybe GroupInfo )


init : Scene a
init =
    Scene
        { objects = Dict.empty
        , order = []
        , groups = Group.empty
        , selected = Nothing
        }



--
-- ATTRIBUTES
--


selected : Scene a -> Maybe ( Key, SubKey )
selected (Scene s) =
    s.selected


select : ( Key, SubKey ) -> Scene a -> Scene a
select =
    Just >> (L.compose scene L.selected).set



--
-- ELEMENTS
--


getElement : Key -> Scene a -> Maybe (Element a)
getElement key data =
    objects.get data
        |> Dict.get key
        |> Maybe.map (\( fig, grp ) -> makeElement data key grp fig)


getSelected : Scene a -> Maybe (Element a)
getSelected (Scene s) =
    case s.selected of
        Just ( key, subKey ) ->
            case Dict.get key s.objects of
                Just ( fig, group_ ) ->
                    Just
                        { key = key
                        , subKey = subKey
                        , isSelected = True
                        , group = group_
                        , model = fig
                        , shape = fig.shape
                        }

                _ ->
                    Nothing

        _ ->
            Nothing


{-| Return all elements in scene in order.
-}
elements : Scene a -> List (Element a)
elements data =
    keys data |> List.filterMap (flip getElement data)



--
-- FIGURES
--


{-| Get figure in given key.

getElement wraps figure in an object with more information and is usually more useful

-}
get : Key -> Scene a -> Maybe (Figure a)
get key =
    scene.get >> .objects >> Dict.get key >> Maybe.map Tuple.first


{-| Update figure with given key.

It is a no-op if figure does not exist. If you don't want this behavior, use put instead.

-}
update : Key -> (Figure a -> Figure a) -> Scene a -> Scene a
update key func =
    let
        updater ( fig, grp ) =
            ( func fig, grp )
    in
    L.modify objects <| Dict.update key (Maybe.map updater)


{-| Put figure in scene in given key.

If figure is not present, insert at the outmost layer.

-}
put : Key -> Figure a -> Scene a -> Scene a
put key fig (Scene s) =
    Scene
        { s
            | objects = Dict.insert key ( fig, Nothing ) s.objects
            , order = orderWithKey key s.order
        }


{-| Add figure to scene and compute a unique key.

It returns the key and the updated scene

-}
insert : Figure a -> Scene a -> ( Key, Scene a )
insert =
    insertAs "obj"


{-| Add figure to scene and compute a unique key.

The key uses the given string as its named part.

-}
insertAs : String -> Figure a -> Scene a -> ( Key, Scene a )
insertAs prefix fig ((Scene s) as ss) =
    let
        key_ =
            Dict.keys s.objects
                |> List.filter (Tuple.first >> (==) prefix)
                |> List.maximum
                |> Maybe.unwrap ( prefix, 0 ) nextKey
    in
    ( key_, put key_ fig ss )


{-| Include many figures into scene.
-}
insertMany : List (Figure a) -> Scene a -> Scene a
insertMany =
    insertManyAs "obj"


{-| Include many figures into scene, using the given key prefix.
-}
insertManyAs : String -> List (Figure a) -> Scene a -> Scene a
insertManyAs prefix figures (Scene s) =
    let
        firstKey =
            Dict.keys s.objects
                |> List.filter (Tuple.first >> (==) prefix)
                |> List.maximum
                |> Maybe.unwrap ( prefix, 0 ) nextKey

        extra =
            figures |> List.indexedMap (\i x -> ( firstKey |> nextKeyBy i, ( x, Nothing ) ))
    in
    Scene
        { s
            | objects = Dict.union s.objects (Dict.fromList extra)
            , order = s.order ++ List.map Tuple.first extra
        }


{-| Discard figure with given key
-}
discard : Key -> Scene a -> Scene a
discard key (Scene s) =
    Scene
        { s
            | objects = Dict.remove key s.objects
            , order = List.filter ((/=) key) s.order
        }


{-| Remove figure with given key and return it
-}
pop : Key -> Scene a -> ( Scene a, Maybe (Figure a) )
pop key data =
    case get key data of
        Just fig ->
            ( discard key data, Just fig )

        Nothing ->
            ( data, Nothing )


{-| List of keys, from bottom layer to top
-}
keys : Scene a -> List Key
keys (Scene s) =
    s.order



--
-- GROUPS
--


{-| Get list of keys for the given group label
-}
getGroup : Label -> Scene a -> List Key
getGroup label =
    groups.get >> Dict.get label >> Maybe.withDefault []


{-| Return GroupInfo associated with key
-}
getGroupOf : Key -> Scene a -> Maybe GroupInfo
getGroupOf key =
    objects.get >> Dict.get key >> Maybe.andThen Tuple.second


moveGroup : Direction -> Key -> Scene a -> Scene a
moveGroup direction key data =
    groups.get data
        |> Group.move direction key
        |> flip updateGroups data


{-| Register key to group
-}
group : Key -> Label -> Scene a -> Scene a
group key label data =
    Group.remove key (groups.get data)
        |> Group.insert key label
        |> flip updateGroups data


groupMany : Dict Key Label -> Scene a -> Scene a
groupMany grps data =
    (Group.toList (groups.get data) ++ Dict.toList grps)
        |> Group.fromList
        |> flip updateGroups data


{-| Update inner structure from groupings
-}
updateGroups : GroupData Key -> Scene a -> Scene a
updateGroups groups_ (Scene s) =
    Scene
        { s
            | groups = groups_
            , objects = Group.updateGroupInfo groups_ second.set s.objects
        }



--
-- ORDERING
--


moveLayer : Direction -> Key -> Scene a -> Scene a
moveLayer direction key (Scene s) =
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
    s
        |> L.modify L.order run
        |> Scene


view : Config a -> State a ->  BBox -> Scene a -> Html (Msg a)
view cfg state bbox data =
    let
        elementsSvg =
            elements data
                |> List.filter (.model >> .visible)
                |> List.map (cfg.config.view cfg.params)

        mapMsg f =
            List.map (S.map f)

        pointerEvents st panWithTouch =
            case ( st, panWithTouch ) of
                ( ClickToInsert _ _, _ ) ->
                    [ Pointer.onDown (.pointer >> .offsetPos >> point >> OnClickAt) ]

                ( _, True ) ->
                    A.touch ( backgroundKey, [] )

                _ ->
                    []

        supressDrag =
            mapMsg (Msg.changeDragMsgsTo Msg.NoOp)
    in
    H.div [ HA.class "container bg-slate-100", HA.class "scene", HA.id cfg.params.sceneId ]
        [ S.svg
            (SA.width "100%" :: SA.class "scene" :: A.viewBox bbox :: pointerEvents state cfg.params.panWithTouch)
            (case state of
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
makeElement (Scene s) key group_ fig =
    case s.selected of
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


scene :
    Lens
        (Scene a)
        { objects : Objects a
        , groups : GroupData Key
        , order : List Key
        , selected : Maybe ( Key, SubKey )
        }
scene =
    Lens (\(Scene s) -> s) (\s _ -> Scene s)


objects : Lens (Scene a) (Objects a)
objects =
    L.compose scene L.objects


groups : Lens (Scene a) (GroupData Key)
groups =
    L.compose scene L.groups
