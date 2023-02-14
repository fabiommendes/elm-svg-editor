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
    , moveFrom
    , moveGroup
    , moveLayer
    , pop
    , put
    , select
    , selectedFullKey
    , selectedKey
    , selectedSubKey
    , update
    )

import BaseTypes exposing (Direction(..))
import Dict exposing (Dict)
import Dict.Extra as Dict
import Element exposing (Element)
import Figure exposing (Figure)
import Geometry exposing (..)
import Group exposing (GroupData, GroupInfo)
import Lens as L
import List.Extra as List
import Maybe.Extra as Maybe
import Monocle.Common exposing (second)
import Monocle.Lens as L exposing (Lens)
import Msg exposing (Msg(..))
import Set
import State exposing (State(..))
import Types exposing (..)
import Util exposing (flip, iff)


{-| Represent an Svg scene
-}
type Scene
    = Scene
        { objects : Objects
        , groups : GroupData Key
        , order : List Key
        , selected : Maybe ( Key, SubKey )
        }


type alias Objects =
    Dict Key ( Figure, Maybe GroupInfo )


init : Scene
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


selectedFullKey : Scene -> Maybe ( Key, SubKey )
selectedFullKey (Scene s) =
    s.selected


selectedSubKey : Scene -> Maybe SubKey
selectedSubKey (Scene s) =
    s.selected |> Maybe.map Tuple.second


selectedKey : Scene -> Maybe Key
selectedKey (Scene s) =
    s.selected |> Maybe.map Tuple.first


select : ( Key, SubKey ) -> Scene -> Scene
select =
    Just >> (L.compose scene L.selected).set



--
-- ELEMENTS
--


getElement : Key -> Scene -> Maybe Element
getElement key data =
    objects.get data
        |> Dict.get key
        |> Maybe.map (\( fig, grp ) -> makeElement data key grp fig)


getSelected : Scene -> Maybe Element
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
                        , figure = fig
                        , shape = fig.shape
                        }

                _ ->
                    Nothing

        _ ->
            Nothing


{-| Return all elements in scene in order.
-}
elements : Scene -> List Element
elements data =
    keys data |> List.filterMap (flip getElement data)



--
-- FIGURES
--


{-| Get figure in given key.

getElement wraps figure in an object with more information and is usually more useful

-}
get : Key -> Scene -> Maybe Figure
get key =
    scene.get >> .objects >> Dict.get key >> Maybe.map Tuple.first


{-| Update figure with given key.

It is a no-op if figure does not exist. If you don't want this behavior, use put instead.

-}
update : Key -> (Figure -> Figure) -> Scene -> Scene
update key func =
    let
        updater ( fig, grp ) =
            ( func fig, grp )
    in
    L.modify objects <| Dict.update key (Maybe.map updater)


{-| Put figure in scene in given key.

If figure is not present, insert at the outmost layer.

-}
put : Key -> Figure -> Scene -> Scene
put key fig (Scene s) =
    Scene
        { s
            | objects = Dict.insert key ( fig, Nothing ) s.objects
            , order = orderWithKey key s.order
        }


{-| Add figure to scene and compute a unique key.

It returns the key and the updated scene

-}
insert : Figure -> Scene -> ( Key, Scene )
insert =
    insertAs "obj"


{-| Add figure to scene and compute a unique key.

The key uses the given string as its named part.

-}
insertAs : String -> Figure -> Scene -> ( Key, Scene )
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
insertMany : List Figure -> Scene -> Scene
insertMany =
    insertManyAs "obj"


{-| Include many figures into scene, using the given key prefix.
-}
insertManyAs : String -> List Figure -> Scene -> Scene
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
discard : Key -> Scene -> Scene
discard key (Scene s) =
    Scene
        { s
            | objects = Dict.remove key s.objects
            , order = List.filter ((/=) key) s.order
        }


{-| Remove figure with given key and return it
-}
pop : Key -> Scene -> ( Scene, Maybe Figure )
pop key data =
    case get key data of
        Just fig ->
            ( discard key data, Just fig )

        Nothing ->
            ( data, Nothing )


{-| List of keys, from bottom layer to top
-}
keys : Scene -> List Key
keys (Scene s) =
    s.order



--
-- GROUPS
--


{-| Get list of keys for the given group label
-}
getGroup : Label -> Scene -> List Key
getGroup label =
    groups.get >> Dict.get label >> Maybe.withDefault []


{-| Return GroupInfo associated with key
-}
getGroupOf : Key -> Scene -> Maybe GroupInfo
getGroupOf key =
    objects.get >> Dict.get key >> Maybe.andThen Tuple.second


moveGroup : Direction -> Key -> Scene -> Scene
moveGroup direction key data =
    groups.get data
        |> Group.move direction key
        |> flip updateGroups data


{-| Register key to group
-}
group : Key -> Label -> Scene -> Scene
group key label data =
    Group.remove key (groups.get data)
        |> Group.insert key label
        |> flip updateGroups data


groupMany : Dict Key Label -> Scene -> Scene
groupMany grps data =
    (Group.toList (groups.get data) ++ Dict.toList grps)
        |> Group.fromList
        |> flip updateGroups data


{-| Update inner structure from groupings
-}
updateGroups : GroupData Key -> Scene -> Scene
updateGroups groups_ (Scene s) =
    Scene
        { s
            | groups = groups_
            , objects = Group.updateGroupInfo groups_ second.set s.objects
        }



--
-- ORDERING
--


moveLayer : Direction -> Key -> Scene -> Scene
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


moveFrom : List Key -> Direction -> Key -> Scene -> Scene
moveFrom keyList direction key ((Scene s) as scene_) =
    let
        hasKey =
            Set.fromList keyList |> flip Set.member

        order =
            s.order
                |> List.indexedMap (\i k -> ( i, k, hasKey k ))
                |> List.filterMap
                    (\( i, k, keep ) ->
                        if keep && k /= key then
                            Just i

                        else
                            Nothing
                    )
                |> iff (direction == Up) (List.maximum >> Maybe.map ((+) 1)) List.minimum

        newOrder =
            Maybe.map (flip List.splitAt s.order) order
                |> Maybe.map
                    (\( pre, post ) ->
                        List.filter ((/=) key) pre ++ key :: List.filter ((/=) key) post
                    )
    in
    if hasKey key then
        moveFrom (List.filter ((/=) key) keyList) direction key scene_

    else if List.member key s.order then
        Scene { s | order = newOrder |> Maybe.withDefault s.order }

    else
        scene_


orderWithKey : Key -> List Key -> List Key
orderWithKey key lst =
    if List.member key lst then
        lst

    else
        lst ++ [ key ]


makeElement : Scene -> Key -> Maybe GroupInfo -> Figure -> Element
makeElement (Scene s) key group_ fig =
    case s.selected of
        Just ( key_, subKey ) ->
            if key == key_ then
                { key = key
                , subKey = subKey
                , group = group_
                , isSelected = True
                , figure = fig
                , shape = fig.shape
                }

            else
                { key = key
                , subKey = []
                , group = group_
                , isSelected = False
                , figure = fig
                , shape = fig.shape
                }

        _ ->
            { key = key
            , subKey = []
            , group = group_
            , isSelected = False
            , figure = fig
            , shape = fig.shape
            }


scene :
    Lens
        Scene
        { objects : Objects
        , groups : GroupData Key
        , order : List Key
        , selected : Maybe ( Key, SubKey )
        }
scene =
    Lens (\(Scene s) -> s) (\s _ -> Scene s)


objects : Lens Scene Objects
objects =
    L.compose scene L.objects


groups : Lens Scene (GroupData Key)
groups =
    L.compose scene L.groups
