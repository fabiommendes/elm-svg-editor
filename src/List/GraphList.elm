module List.GraphList exposing
    ( GraphList
    , all
    , any
    , append
    , bimap
    , bimap2
    , bimap3
    , bimap4
    , bimap5
    , concat
    , concatMap
    , cons
    , filter
    , filterMap
    , foldr
    , fromList
    , head
    , headBoth
    , headE
    , indexedMap
    , indexedMapEdges
    , length
    , lift2
    , lift3
    , map
    , mapEdges
    , maximum
    , member
    , minimum
    , pair
    , product
    , reduce
    , reduceEdges
    , repeat
    , reverse
    , splitWhenChange
    , sum
    , toEdges
    , toInterspersed
    , toList
    , toPairs
    , toSegments
    )

{-| A list of Segments.

@docs SegmentList


## Construction

-}

import List.NonEmpty as L exposing (NonEmpty(..))


{-| A list of a\`s with at least 2 elements.

Meta information can be associated to each segment pair.

-}
type GraphList edge a
    = S a (NonEmpty ( edge, a ))



--- Create


{-| Create a list with two elements and the given edge information.

Use `pair ()` if you want to construct a list with no edge data.

-}
pair : edge -> a -> a -> GraphList edge a
pair e x y =
    S x (L.singleton ( e, y ))


{-| Create a list with n (at least 2) copies of a value

The edge information is repeated to all nodes.

-}
repeat : edge -> Int -> a -> GraphList edge a
repeat e n x =
    if n <= 2 then
        pair e x x

    else
        cons e x (repeat e (n - 1) x)


{-| Add an element to the front of a list.
-}
cons : edge -> a -> GraphList edge a -> GraphList edge a
cons e x (S y ys) =
    S x (L.cons ( e, y ) ys)


{-| Try to create a GraphList from a list with at least two elements.

The function compute edge information from each pair of elements

-}
fromList : (a -> a -> edge) -> List a -> Maybe (GraphList edge a)
fromList f xs =
    let
        do x y lst =
            case lst of
                [] ->
                    pair (f x y) x y

                z :: rest ->
                    cons (f x y) x (do y z rest)
    in
    case xs of
        x :: y :: rest ->
            Just <| do x y rest

        _ ->
            Nothing


{-| Convert data to list, discarding edge information.
-}
toList : GraphList edge a -> List a
toList (S x ps) =
    x :: (ps |> L.toList |> List.map Tuple.second)


{-| Convert to a list of edge data, discarding elements.
-}
toEdges : GraphList edge a -> List edge
toEdges (S _ ps) =
    ps |> L.toList |> List.map Tuple.first


{-| Flatten edge information interespersed with data.
-}
toInterspersed : GraphList a a -> List a
toInterspersed (S x ps) =
    x :: (ps |> L.toList |> List.concatMap (\( e, y ) -> [ e, y ]))


{-| Convert to a list of pairs. Consecutive elements are repeated
-}
toPairs : GraphList edge a -> List ( a, a )
toPairs =
    toSegments (\_ -> Tuple.pair)


{-| Create segments traversing data using the given function

The function compute a segment as (edge start end -> segment).

-}
toSegments : (edge -> a -> a -> b) -> GraphList edge a -> List b
toSegments f (S x (NonEmpty ( e, y ) ps)) =
    let
        do last elems =
            case elems of
                [] ->
                    []

                ( e_, z ) :: rest ->
                    f e_ last z :: do z rest
    in
    f e x y :: do y ps


{-| Apply a function to every element of the graph.
-}
map : (a -> b) -> GraphList edge a -> GraphList edge b
map f =
    bimap f identity


{-| Recompute edges using the current annotation and the values of each vertex.

If you want to transform edge data without using the elements, use `bimap identity f lst`.

-}
mapEdges : (edge -> a -> a -> edge_) -> GraphList edge a -> GraphList edge_ a
mapEdges f (S x (NonEmpty ( e, y ) tail)) =
    let
        do last elems =
            case elems of
                [] ->
                    []

                ( ee, z ) :: rest ->
                    ( f ee last z, z ) :: do z rest
    in
    S x (L.create ( f e x y, y ) (do y tail))


{-| Execute function of two arguments using all combination of elements from each argument and collect results in a single list.
-}
lift2 : (a -> b -> r) -> GraphList edge a -> GraphList edge b -> GraphList edge r
lift2 f xs ys =
    xs |> concatMap (\x -> map (\y -> f x y) ys)


{-| Execute function of 3 arguments using all combination of elements from each argument and collect results in a single list.
-}
lift3 : (a -> b -> c -> r) -> GraphList edge a -> GraphList edge b -> GraphList edge c -> GraphList edge r
lift3 f xs ys zs =
    xs |> concatMap (\x -> lift2 (\y z -> f x y z) ys zs)


{-| Transform elements and edges simultaneously.
-}
bimap : (a -> b) -> (edge -> edge_) -> GraphList edge a -> GraphList edge_ b
bimap f g (S x ps) =
    S (f x) (L.map (\( e, y ) -> ( g e, f y )) ps)


{-| Apply two argument functions to elements and edges of a pair of lists
-}
bimap2 : (a -> b -> r) -> (e1 -> e2 -> edge) -> GraphList e1 a -> GraphList e2 b -> GraphList edge r
bimap2 f g (S x xs) (S y ys) =
    S (f x y) (L.map2 (\( e1, x1 ) ( e2, x2 ) -> ( g e1 e2, f x1 x2 )) xs ys)


{-| Apply three argument functions to elements and edges of a triple of lists
-}
bimap3 : (a -> b -> c -> r) -> (e1 -> e2 -> e3 -> edge) -> GraphList e1 a -> GraphList e2 b -> GraphList e3 c -> GraphList edge r
bimap3 f g (S x xs) (S y ys) (S z zs) =
    S (f x y z) (L.map3 (\( e1, x1 ) ( e2, x2 ) ( e3, x3 ) -> ( g e1 e2 e3, f x1 x2 x3 )) xs ys zs)


{-| Apply three argument functions to elements and edges of a triple of lists
-}
bimap4 : (a -> b -> c -> d -> r) -> (e1 -> e2 -> e3 -> e4 -> edge) -> GraphList e1 a -> GraphList e2 b -> GraphList e3 c -> GraphList e4 d -> GraphList edge r
bimap4 f g xxs yys zzs wws =
    biAp (bimap3 f g xxs yys zzs) wws


{-| Apply three argument functions to elements and edges of a triple of lists
-}
bimap5 : (a -> b -> c -> d -> e -> r) -> (e1 -> e2 -> e3 -> e4 -> e5 -> edge) -> GraphList e1 a -> GraphList e2 b -> GraphList e3 c -> GraphList e4 d -> GraphList e5 e -> GraphList edge r
bimap5 f g xxs yys zzs wws qqs =
    biAp (bimap4 f g xxs yys zzs wws) qqs


ap_ : (b -> a) -> b -> a
ap_ f x =
    f x


biAp : GraphList (e -> f) (a -> b) -> GraphList e a -> GraphList f b
biAp fs xs =
    bimap2 ap_ ap_ fs xs


{-| Same as map but the function is also applied to the index of each element (starting at zero).
-}
indexedMap : (Int -> a -> b) -> GraphList edge a -> GraphList edge b
indexedMap f =
    indexedBimap f (\_ x -> x)


{-| Same as mapE but the function is also applied to the index of each element (starting at zero).
-}
indexedMapEdges : (Int -> edge -> a -> a -> edge_) -> GraphList edge a -> GraphList edge_ a
indexedMapEdges f (S x (NonEmpty ( e, y ) ps)) =
    let
        do i last elems =
            case elems of
                [] ->
                    []

                ( ee, z ) :: rest ->
                    ( f i ee last z, z ) :: do (i + 1) z rest
    in
    S x (L.create ( f 0 e x y, y ) (do 1 y ps))


{-| Same as mapBoth but the function is also applied to the index of each element (starting at zero).
-}
indexedBimap : (Int -> a -> b) -> (Int -> edge -> edge_) -> GraphList edge a -> GraphList edge_ b
indexedBimap f g (S x ps) =
    S (f 0 x) (L.indexedMap (\i ( e, y ) -> ( g i e, f (i + 1) y )) ps)


{-| Reduce a list from the left.
-}
foldl : (a -> b -> b) -> b -> GraphList edge a -> b
foldl f acc (S x ps) =
    L.foldl (\( _, y ) tmp -> f y tmp) (f x acc) ps


{-| Reduce a list from the right.
-}
foldr : (a -> b -> b) -> b -> GraphList edge a -> b
foldr f acc (S x ps) =
    f x <| L.foldr (\( _, y ) tmp -> f y tmp) acc ps


{-| Reduce a list from left using operator.

This version of foldl does not require an accumulator since the list is guaranteed to not being empty

-}
reduce : (a -> a -> a) -> GraphList edge a -> a
reduce f (S x ps) =
    L.foldl (\( _, y ) tmp -> f y tmp) x ps


{-| Reduce a list from the left, using the edge data in the accumulator function
-}
reduceEdges : (edge -> a -> a -> a) -> GraphList edge a -> a
reduceEdges f (S x ps) =
    L.foldl (\( e, y ) tmp -> f e y tmp) x ps


{-| Keep elements that satisfy the test.

Return a list, since result might be empty.

-}
filter : (a -> Bool) -> GraphList edge a -> List a
filter pred =
    filterMap
        (\x ->
            if pred x then
                Just x

            else
                Nothing
        )


{-| Filter out certain values.

Return a list, since result might be empty.

-}
filterMap : (a -> Maybe b) -> GraphList edge a -> List b
filterMap pred (S x ps) =
    let
        tail =
            L.filterMap (\( _, y ) -> pred y) ps
    in
    case pred x of
        Just y ->
            y :: tail

        _ ->
            tail


{-| Determine the number of elements in the graph
-}
length : GraphList edge a -> Int
length =
    foldl (\_ i -> i + 1) 0


{-| Reverse the list.

Edge data is preserved, but elements connected in each node are now flipped

-}
reverse : GraphList edge a -> GraphList edge a
reverse (S x ps) =
    let
        do acc last elems =
            case elems of
                [] ->
                    S x ps

                -- this should never reach
                [ ( e, y ) ] ->
                    S y (L.create ( e, last ) acc)

                ( e, y ) :: rest ->
                    do (( e, last ) :: acc) y rest
    in
    do [] x (L.toList ps)


{-| Figure out whether a list contains a value.
-}
member : a -> GraphList edge a -> Bool
member elem (S x ps) =
    x == elem || L.any (\( _, y ) -> y == elem) ps


{-| Put two lists together.

If the junction is empty, repeats the last edge from the first list when creating the junction between two edges. Otherwise use it to connect both lists.

-}
append : Maybe edge -> GraphList edge a -> GraphList edge a -> GraphList edge a
append junction ((S x ps) as xs) ys =
    case junction of
        Just edge ->
            appendWith edge xs ys

        Nothing ->
            S x <| appendAux ps ys


appendWith : edge -> GraphList edge a -> GraphList edge a -> GraphList edge a
appendWith edge (S x ps) (S y qs) =
    S x (L.append ps (L.cons ( edge, y ) qs))


appendAux : NonEmpty ( edge, a ) -> GraphList edge a -> NonEmpty ( edge, a )
appendAux ps ys =
    case L.uncons ps of
        ( ( e, y ), [] ) ->
            L.cons ( e, y ) (toListE e ys)

        ( a, b :: rest ) ->
            L.cons a <| appendAux (L.create b rest) ys


toListE : edge -> GraphList edge a -> NonEmpty ( edge, a )
toListE e (S x ps) =
    L.cons ( e, x ) ps


{-| Concatenate a bunch of lists into a single list
-}
concat : GraphList edge (GraphList edge a) -> GraphList edge a
concat =
    concatMap identity


{-| Map a given function onto a list and flatten the resulting lists.

    concatMap f xs == concat (map f xs)

-}
concatMap : (a -> GraphList edge b) -> GraphList edge a -> GraphList edge b
concatMap f (S x (NonEmpty ( e, y ) qs)) =
    case qs of
        [] ->
            appendWith e (f x) (f y)

        ( e_, z ) :: ps ->
            appendWith e (f x) (concatMap f (S y (L.create ( e_, z ) ps)))


{-| Determine if all elements satisfy some test.
-}
all : (a -> Bool) -> GraphList edge a -> Bool
all f (S x ps) =
    f x && L.all (\( _, y ) -> f y) ps


{-| Determine if any elements satisfy some test.
-}
any : (a -> Bool) -> GraphList edge a -> Bool
any f (S x ps) =
    f x || L.any (\( _, y ) -> f y) ps


{-| Find the maximum element in a non-empty list.
-}
maximum : GraphList edge comparable -> comparable
maximum xs =
    reduce max xs


{-| Find the minimum element in a non-empty list.
-}
minimum : GraphList edge comparable -> comparable
minimum xs =
    reduce min xs


{-| Get the sum of the list elements.
-}
sum : GraphList meta number -> number
sum =
    reduce (+)


{-| Get the product of the list elements.
-}
product : GraphList meta number -> number
product =
    reduce (*)


{-| Return the first element
-}
head : GraphList edge a -> a
head (S x _) =
    x


{-| Return a pair with the edge data and the first element
-}
headBoth : GraphList edge a -> ( edge, a )
headBoth (S x ps) =
    ( L.head ps |> Tuple.first, x )


headE : GraphList edge a -> edge
headE (S _ ps) =
    L.head ps |> Tuple.first


splitWhenChange : (edge -> comparable) -> GraphList edge a -> NonEmpty (GraphList edge a)
splitWhenChange f ((S x (NonEmpty ( edge, y ) ps)) as full) =
    let
        flush a e b acc =
            reverse <| S a (L.NonEmpty ( e, b ) acc)

        do : comparable -> ( a, edge, a ) -> List ( edge, a ) -> List ( edge, a ) -> List (GraphList edge a)
        do sent ( a, e, b ) acc pending =
            case pending of
                [] ->
                    [ flush a e b acc ]

                ( eNext, next ) :: rest ->
                    let
                        sentNext =
                            f eNext
                    in
                    if sentNext == sent then
                        do sentNext ( b, eNext, next ) (( edge, b ) :: acc) rest

                    else
                        flush a e b acc :: do sentNext ( b, eNext, next ) [] rest
    in
    L.withDefault full <| do (f edge) ( x, edge, y ) [] ps
