module Types exposing (..)

{-| Type aliases used across the whole project
-}


type alias Label =
    String


type alias Description =
    String


type alias Key =
    ( String, Int )


type alias SubKey =
    List Int


key : String -> Key
key data =
    ( data, 0 )


backgroundKey : Key
backgroundKey =
    ( "background", -1 )


anonymousKey : Key
anonymousKey =
    ( "obj", 0 )


nextKey : Key -> Key
nextKey ( a, b ) =
    ( a, b + 1 )


nextKeyBy : Int -> Key -> Key
nextKeyBy i ( a, b ) =
    ( a, b + i )


showKey : Key -> String
showKey ( a, b ) =
    a ++ "-" ++ String.fromInt b
