module Group exposing (GroupData, GroupInfo, empty, fromGroupInfoMap, fromList, get, getAt, insert, move, moveAt, remove, removeAt, toGroupInfoMap, toList, updateGroupInfo)

import BaseTypes exposing (Direction(..))
import Dict exposing (Dict)
import Dict.Extra as Dict
import List.Extra as List
import Maybe.Extra as Maybe
import Types exposing (..)
import Util exposing (flip, iff)
import Util.List exposing (bubbleDownAt, bubbleDownWhen, bubbleUpAt, bubbleUpWhen)


{-| Describe how elements are grouped together.

Each group has a Label and an ordered list of members identified by their respective keys

-}
type alias GroupData comparable =
    Dict Label (List comparable)


{-| This is attatched to elemements when providing information about their group membership

Each element can be in one or zero groups.

-}
type alias GroupInfo =
    { label : Label, index : Int }


empty : GroupData comparable
empty =
    Dict.empty


fromList : List ( key, Label ) -> GroupData key
fromList =
    let
        updater k mks =
            case mks of
                Just ks ->
                    Just (uniquelyAddKey k ks)

                _ ->
                    Just [ k ]

        reducer ( key, label ) acc =
            Dict.update label (updater key) acc
    in
    List.foldl reducer Dict.empty


toList : GroupData key -> List ( key, Label )
toList =
    Dict.toList >> List.concatMap (\( label, keys ) -> List.map (\k -> ( k, label )) keys)


{-| Convert Group Data into a mapping from identifiers to group info
-}
toGroupInfoMap : GroupData comparable -> Dict comparable GroupInfo
toGroupInfoMap =
    Dict.toList
        >> List.concatMap
            (\( grp, xs ) ->
                xs |> List.indexedMap (\i x -> ( x, { label = grp, index = i } ))
            )
        >> Dict.fromList


{-| Recriate Group Data from a mapping from identifiers to group info values.

Assumes data is consistent, i.e., no key share the same index in the same group and
there are no "holes" in the indexing data.

-}
fromGroupInfoMap : Dict comparable GroupInfo -> GroupData comparable
fromGroupInfoMap data =
    let
        clear _ pairs =
            pairs
                |> List.sortBy (Tuple.second >> .index)
                |> List.map Tuple.first
    in
    data
        |> Dict.toList
        |> Dict.groupBy (Tuple.second >> .label)
        |> Dict.map clear


{-| Update dictionary that stores an optional group info in some
of the values field
-}
updateGroupInfo : GroupData comparable -> (Maybe GroupInfo -> a -> a) -> Dict comparable a -> Dict comparable a
updateGroupInfo group modify data =
    let
        byKey =
            toGroupInfoMap group

        update ( key, x ) =
            ( key, modify (Dict.get key byKey) x )
    in
    data
        |> Dict.toList
        |> List.map update
        |> Dict.fromList


{-| Remove key from group data
-}
remove : comparable -> GroupData comparable -> GroupData comparable
remove key group =
    List.foldl (flip removeAt key) group (Dict.keys group)


{-| Remove key from specific group.

If group is known, this is faster than using ungroup

-}
removeAt : Label -> comparable -> GroupData comparable -> GroupData comparable
removeAt label key group =
    Dict.update label (Maybe.map (withoutKey key)) group


{-| Get group info from key in any group
-}
get : key -> GroupData key -> Maybe GroupInfo
get key groups =
    List.foldl
        (\label -> Maybe.orElseLazy (\_ -> getAt label key groups))
        Nothing
        (Dict.keys groups)


{-| Get group info from key in specific group
-}
getAt : Label -> key -> GroupData key -> Maybe GroupInfo
getAt label key groups =
    Dict.get label groups
        |> Maybe.andThen (List.elemIndex key >> Maybe.map (GroupInfo label))


{-| Register key to group
-}
insert : comparable -> Label -> GroupData comparable -> GroupData comparable
insert key label group =
    case Dict.get label group of
        Just keys ->
            Dict.insert label (key :: withoutKey key keys) group

        Nothing ->
            Dict.insert label [ key ] group


{-| Move key up or down in group
-}
move : Direction -> comparable -> GroupData comparable -> GroupData comparable
move dir key groups =
    case get key groups of
        Just { label, index } ->
            Dict.update label (Maybe.map <| iff (dir == Up) bubbleUpAt bubbleDownAt index) groups

        _ ->
            groups


moveAt : Label -> Direction -> comparable -> GroupData comparable -> GroupData comparable
moveAt label dir key groups =
    Dict.update label (Maybe.map <| iff (dir == Up) bubbleUpWhen bubbleDownWhen ((==) key)) groups


uniquelyAddKey : key -> List key -> List key
uniquelyAddKey k ks =
    k :: withoutKey k ks


withoutKey : key -> List key -> List key
withoutKey k ks =
    List.filter ((/=) k) ks
