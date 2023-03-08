module Svg.Editor exposing
    ( Config, Model, Msg
    , Meters, Url, Text
    , defaulConfig, init, subscriptions, update, view, cmd
    , withFigures, image, line, point, text
    , editable, grow, move, visible, withLabel, withStyle
    , load, save, select, selectSubKey
    )

{-| Basic types

@docs Config, Model, Msg


## Type alias

@docs Meters, Url, Text


## Elm Archtecture

@docs defaulConfig, init, subscriptions, update, view, cmd


## Inserting elements to the scene

@docs withFigures, image, line, point, text


## Figure transformations

@docs editable, grow, move, visible, withLabel, withStyle


## Persistence

@docs load, save, select, selectSubKey

-}

import Encode
import Figure exposing (Figure)
import Geometry as G
import Html exposing (Html)
import Json.Encode
import Lens as L
import Model
import Msg
import Scene
import Shape
import Subscriptions
import Svg.Editor.Config as Config
import Task
import Types exposing (..)
import Update
import View


{-| Editor model
-}
type Model
    = M Model.Model


{-| Message
-}
type alias Msg =
    Msg.Msg


{-| Editor config
-}
type alias Config =
    Config.Config


{-| Length units
-}
type alias Meters =
    Float


{-| Type alias for Urls
-}
type alias Url =
    String


{-| Type alias for textual data. Usually strings that are shown directly to the end user.
-}
type alias Text =
    String


{-| Init function in TEA
-}
init : Config -> Model
init =
    M << Model.init


{-| Configuration object
-}
defaulConfig : Config
defaulConfig =
    Config.init


{-| Update function in TEA
-}
update : Config -> Msg -> Model -> ( Model, Cmd Msg )
update cfg msg (M model) =
    Tuple.mapFirst M (Update.update cfg msg model)


{-| View function in TEA
-}
view : Config -> Model -> Html Msg
view cfg (M model) =
    View.view cfg model


{-| Subscription function in TEA
-}
subscriptions : Config -> Model -> Sub Msg
subscriptions cfg (M model) =
    Subscriptions.subscriptions cfg model


{-| Command to properly initialize the editor
-}
cmd : Cmd Msg
cmd =
    Task.perform (\() -> Msg.OnWindowResize) (Task.succeed ())



-------------------------------------------------------------------------------
--- Add elements to scene
-------------------------------------------------------------------------------


{-| Add list of figures to model
-}
withFigures : String -> List Figure -> Model -> Model
withFigures prefix figs (M m) =
    M (m |> Model.updateScene (Scene.insertManyAs prefix figs))


{-| Create a point at origin.

Use move to set its position.

-}
point : ( Meters, Meters ) -> Figure
point =
    Shape.point << G.point


{-| Create a line from a list of vertices
-}
line : List ( Meters, Meters ) -> Figure
line =
    Shape.line


{-| Create an image with given width and source URL
-}
image : Meters -> Url -> Figure
image =
    Shape.image


{-| Create a text figure from string
-}
text : Text -> Figure
text =
    Shape.text


{-| Move figure by the given vector displacement
-}
move : ( Meters, Meters ) -> Figure -> Figure
move =
    Figure.move << G.vector


{-| Re-scale figure by the given scaling factor
-}
grow : Float -> Figure -> Figure
grow =
    Figure.grow


{-| Control if figure is visible or not
-}
visible : Bool -> Figure -> Figure
visible =
    Figure.visible


{-| Control if figure is editable or not
-}
editable : Bool -> Figure -> Figure
editable =
    Figure.editable


{-| Set figure label
-}
withLabel : Label -> Figure -> Figure
withLabel =
    Figure.setLabel


{-| Add style to figure
-}
withStyle : String -> String -> Figure -> Figure
withStyle =
    Figure.addStyle



-------------------------------------------------------------------------------
--- Change editor
-------------------------------------------------------------------------------


{-| Load scene from json data
-}
load : String -> Model -> Model
load st (M model) =
    M (Update.loadScene st model)


{-| Save scene to json
-}
save : Model -> String
save (M model) =
    Json.Encode.encode 2 (Encode.scene (Model.scene model))


{-| Select element with the given key.

Invalid keys clear the current selection

-}
select : Key -> Model -> Model
select key (M model) =
    M (model |> Model.updateScene (Scene.select ( key, [] )))


{-| Select sub-key of the current element
-}
selectSubKey : SubKey -> Model -> Model
selectSubKey sub (M model) =
    model
        |> Model.updateScene
            (\s ->
                s |> L.selected.set (s.selected |> Maybe.map (\( k, _ ) -> ( k, sub )))
            )
        |> M
