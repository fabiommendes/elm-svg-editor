A simple SVG editor that can be embedded in other applications 

The editor was created for the croq.app project and is designed with its needs in mind.
That said, it can be configured as a fairly generic SVG editor for simple shapes.

## Configuration

The Svg editor follows TEA, with an extra configuration object that must be passed to the 
view, update and subscriptions.

```elm
module App exposing (main)

import Browser
import Svg.Editor


main : Program () Model Msg
main =
    let
        cfg = Svg.Editor.defaulConfig
    in 
    Browser.element
        { init = \_ -> ( Svg.Editor.init, cmd )
        , update = Svg.Editor.update cfg
        , view = Svg.Editor.view cfg
        , subscriptions = Svg.Editor.subscriptions cfg
        }
```