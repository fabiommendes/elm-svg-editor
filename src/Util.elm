module Util exposing (..)


iff : Bool -> c -> c -> c
iff cond ok bad =
    if cond then
        ok

    else
        bad


flip : (c -> b -> a) -> b -> c -> a
flip f x y =
    f y x
