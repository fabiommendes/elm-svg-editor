module List.NonEmpty exposing
    ( NonEmpty(..)
    , create, cons, singleton, repeat, fromList
    , map, indexedMap, foldl, foldr, reduce
    , filter, filterMap
    , append, concat, concatMap, map2, map3, map4, map5
    , sort, sortBy, sortWith
    , isEmpty, head, tail, partition, unzip
    , length, all, any, minimum, maximum, sum, product, member, reverse, toList
    , generate, uncons, updateAt, withDefault, withExample, removeAt
    )

{-| A list with at least one element

@docs NonEmpty


## Construction

@docs create, cons, singleton, repeat, fromList


## Transform

@docs map, indexedMap, foldl, foldr, reduce


## Filtering

@docs filter, filterMap


## Combine

@docs append, concat, concatMap, intersperse, map2, map3, map4, map5


## Sort

@docs sort, sortBy, sortWith


## Deconstruct

@docs isEmpty, head, tail, take, drop, partition, unzip


## Utilities

@docs length, all, any, minimum, maximum, sum, product, member, reverse, toList

-}

import List.Extra as List


{-| A list of a\`s with at least 2 elements.

Meta information can be associated to each segment pair.

-}
type NonEmpty a
    = NonEmpty a (List a)



--- Create


{-| Create a list with only two elements
-}
singleton : a -> NonEmpty a
singleton x =
    NonEmpty x []


{-| Create list from first element and tail
-}
create : a -> List a -> NonEmpty a
create =
    NonEmpty


{-| Create a list with n (at least 2) copies of a value
-}
repeat : Int -> a -> NonEmpty a
repeat i x =
    NonEmpty x (List.repeat (i - 1) x)


{-| Try to create sequence from a list with at least two elements
-}
fromList : List a -> Maybe (NonEmpty a)
fromList elems =
    case elems of
        [] ->
            Nothing

        x :: xs ->
            Just (NonEmpty x xs)


{-| Create NonEmpty from list, returning a singleton with the given default if list is empty.
-}
withDefault : a -> List a -> NonEmpty a
withDefault x xs =
    case xs of
        [] ->
            singleton x

        y :: rest ->
            NonEmpty y rest


{-| Create NonEmpty from list, returning example if list is empty.
-}
withExample : NonEmpty a -> List a -> NonEmpty a
withExample ne xs =
    case xs of
        [] ->
            ne

        y :: rest ->
            NonEmpty y rest


{-| Add an element to the front of a list.
-}
cons : a -> NonEmpty a -> NonEmpty a
cons x (NonEmpty y ys) =
    NonEmpty x (y :: ys)


{-| Convert data to list
-}
toList : NonEmpty a -> List a
toList (NonEmpty x xs) =
    x :: xs


{-| Apply a function to every element of a list.
-}
map : (a -> b) -> NonEmpty a -> NonEmpty b
map f (NonEmpty x xs) =
    NonEmpty (f x) (List.map f xs)


{-| Apply a two argument function pairwise to elements of both lists
-}
map2 : (a -> b -> r) -> NonEmpty a -> NonEmpty b -> NonEmpty r
map2 f (NonEmpty x xs) (NonEmpty y ys) =
    NonEmpty (f x y) (List.map2 f xs ys)


{-| Apply a 3 argument function tuples of elements from all lists
-}
map3 : (a -> b -> c -> r) -> NonEmpty a -> NonEmpty b -> NonEmpty c -> NonEmpty r
map3 f (NonEmpty x xs) (NonEmpty y ys) (NonEmpty z zs) =
    NonEmpty (f x y z) (List.map3 f xs ys zs)


{-| Apply a 4 argument function tuples of elements from all lists
-}
map4 : (a -> b -> c -> d -> r) -> NonEmpty a -> NonEmpty b -> NonEmpty c -> NonEmpty d -> NonEmpty r
map4 f (NonEmpty x xs) (NonEmpty y ys) (NonEmpty z zs) (NonEmpty w ws) =
    NonEmpty (f x y z w) (List.map4 f xs ys zs ws)


{-| Apply a 5 argument function tuples of elements from all lists
-}
map5 : (a -> b -> c -> d -> e -> r) -> NonEmpty a -> NonEmpty b -> NonEmpty c -> NonEmpty d -> NonEmpty e -> NonEmpty r
map5 f (NonEmpty x xs) (NonEmpty y ys) (NonEmpty z zs) (NonEmpty w ws) (NonEmpty q qs) =
    NonEmpty (f x y z w q) (List.map5 f xs ys zs ws qs)


{-| Sort values from lowest to highest
-}
sort : NonEmpty comparable -> NonEmpty comparable
sort =
    listAndBack List.sort


{-| Sort values by a derived property.
-}
sortBy : (a -> comparable) -> NonEmpty a -> NonEmpty a
sortBy f =
    listAndBack (List.sortBy f)


{-| Sort values with a custom comparison function.

This is also the most general sort function, allowing you to define any other: sort == sortWith compare

-}
sortWith : (a -> a -> Order) -> NonEmpty a -> NonEmpty a
sortWith f =
    listAndBack (List.sortWith f)


listAndBack : (List a -> List a) -> NonEmpty a -> NonEmpty a
listAndBack f xs =
    f (toList xs) |> fromList |> Maybe.withDefault xs


{-| Same as map but the function is also applied to the index of each element (starting at zero).
-}
indexedMap : (Int -> a -> b) -> NonEmpty a -> NonEmpty b
indexedMap f (NonEmpty x xs) =
    NonEmpty (f 0 x) (List.indexedMap (\i a -> f (i + 1) a) xs)


{-| Reduce a list from the left.
-}
foldl : (a -> b -> b) -> b -> NonEmpty a -> b
foldl f acc (NonEmpty x xs) =
    List.foldl f (f x acc) xs


{-| Reduce a list from the right.
-}
foldr : (a -> b -> b) -> b -> NonEmpty a -> b
foldr f acc (NonEmpty x xs) =
    List.foldr f acc xs |> f x


{-| Reduce a list from left using operator.

This version of foldl does not require an accumulator since the list is guaranteed to not being empty

-}
reduce : (a -> a -> a) -> NonEmpty a -> a
reduce f (NonEmpty x xs) =
    List.foldl f x xs


{-| Keep elements that satisfy the test.

Return a list, since result might be empty.

-}
filter : (a -> Bool) -> NonEmpty a -> List a
filter pred (NonEmpty x xs) =
    if pred x then
        x :: List.filter pred xs

    else
        List.filter pred xs


{-| Filter out certain values.

Return a list, since result might be empty.

-}
filterMap : (a -> Maybe b) -> NonEmpty a -> List b
filterMap f (NonEmpty x xs) =
    case f x of
        Just y ->
            y :: List.filterMap f xs

        _ ->
            List.filterMap f xs


{-| Determine the length of a list.
-}
length : NonEmpty a -> Int
length (NonEmpty _ xs) =
    1 + List.length xs


{-| Reverse a list.
-}
reverse : NonEmpty a -> NonEmpty a
reverse ((NonEmpty x xs) as ys) =
    fromList (List.reverse xs ++ [ x ]) |> Maybe.withDefault ys


{-| Figure out whether a list contains a value.
-}
member : a -> NonEmpty a -> Bool
member elem (NonEmpty x xs) =
    elem == x || List.member elem xs


{-| Put two lists together.

Repeats the last edge information from the first list when creationg the junction between two edges

-}
append : NonEmpty a -> NonEmpty a -> NonEmpty a
append (NonEmpty x xs) (NonEmpty y ys) =
    NonEmpty x (xs ++ (y :: ys))


{-| Concatenate a bunch of lists into a single list
-}
concat : NonEmpty (NonEmpty a) -> NonEmpty a
concat (NonEmpty (NonEmpty x xs) xss) =
    NonEmpty x (List.concat (xs :: List.map toList xss))


{-| Map a given function onto a list and flatten the resulting lists.

    concatMap f xs == concat (map f xs)

-}
concatMap : (a -> NonEmpty b) -> NonEmpty a -> NonEmpty b
concatMap f (NonEmpty x xs) =
    let
        (NonEmpty y ys) =
            f x

        rest =
            List.concatMap (f >> toList) xs
    in
    NonEmpty y (ys ++ rest)


{-| Determine if all elements satisfy some test.
-}
all : (a -> Bool) -> NonEmpty a -> Bool
all f (NonEmpty x xs) =
    f x && List.all f xs


{-| Determine if any elements satisfy some test.
-}
any : (a -> Bool) -> NonEmpty a -> Bool
any f (NonEmpty x xs) =
    f x || List.all f xs


{-| Find the maximum element in a non-empty list.
-}
maximum : NonEmpty comparable -> comparable
maximum xs =
    reduce max xs


{-| Find the minimum element in a non-empty list.
-}
minimum : NonEmpty comparable -> comparable
minimum xs =
    reduce min xs


{-| Get the sum of the list elements.
-}
sum : NonEmpty number -> number
sum =
    reduce (+)


isEmpty : NonEmpty a -> Bool
isEmpty _ =
    False


{-| Get the product of the list elements.
-}
product : NonEmpty number -> number
product =
    reduce (*)


{-| Return the first element
-}
head : NonEmpty a -> a
head (NonEmpty x _) =
    x


{-| Extract the rest of the list.
-}
tail : NonEmpty a -> List a
tail (NonEmpty _ xs) =
    xs


{-| Partition a list based on some test.

The first list contains all values that satisfy the test, and the second list contains all the value that do not.

-}
partition : (a -> Bool) -> NonEmpty a -> ( List a, List a )
partition pred (NonEmpty x xs) =
    let
        ( in_, out ) =
            List.partition pred xs
    in
    if pred x then
        ( x :: in_, out )

    else
        ( in_, x :: out )


{-| Decompose a list of tuples into a tuple of lists.
-}
unzip : NonEmpty ( a, b ) -> ( NonEmpty a, NonEmpty b )
unzip (NonEmpty ( x, y ) pairs) =
    let
        ( xs, ys ) =
            List.unzip pairs
    in
    ( NonEmpty x xs, NonEmpty y ys )


{-| Deconstruct list into the head and tail parts
-}
uncons : NonEmpty a -> ( a, List a )
uncons (NonEmpty x xs) =
    ( x, xs )


generate : (a -> a -> b) -> NonEmpty a -> List b
generate f (NonEmpty x xs) =
    let
        do last items =
            case items of
                [] ->
                    []

                y :: ys ->
                    f last y :: do y ys
    in
    do x xs


updateAt : Int -> (a -> a) -> NonEmpty a -> NonEmpty a
updateAt i f (NonEmpty x xs) =
    if i <= 0 then
        NonEmpty (f x) xs

    else
        NonEmpty x (List.updateAt (i - 1) f xs)


{-| Remove the element at an index from a list. Return the original list if the index is out of range or if the resulting list would be empty
-}
removeAt : Int -> NonEmpty a -> NonEmpty a
removeAt i (NonEmpty x xs) =
    if i == 0 then
        case xs of
            [] ->
                NonEmpty x xs

            y :: ys ->
                NonEmpty y ys

    else
        NonEmpty x (List.removeAt (i - 1) xs)
