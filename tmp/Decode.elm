module Decode exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Polyline2d
import Types as T exposing (..)


pair : Decoder ( Float, Float )
pair =
    succeed Tuple.pair
        |> required "x" float
        |> required "y" float


point : Decoder Point
point =
    pair |> map T.point


vector : Decoder Vector
vector =
    pair |> map T.vector


line : Decoder Line
line =
    list point |> map Polyline2d.fromVertices


text : Decoder Text
text =
    succeed (\data x y -> Text data (T.point ( x, y )))
        |> required "data" string
        |> required "x" float
        |> required "y" float


image : D.Decoder Image
image =
    D.succeed (\href x y -> Image href (Point.point x y))
        |> D.required "href" D.string
        |> D.required "x" D.float
        |> D.required "y" D.float



mark : D.Decoder Mark
mark =
    D.succeed (\id label x y -> Mark id label (Point.point x y))
        |> D.required "id" D.int
        |> D.required "label" D.string
        |> D.required "x" D.float
        |> D.required "y" D.float
