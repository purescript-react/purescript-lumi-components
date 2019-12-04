module Lumi.Components2.Examples.Box where

import Prelude
import Color.Scheme.MaterialDesign (blue, green, red)
import Lumi.Components (lumiElement)
import Lumi.Components.Example (example)
import Lumi.Components.Spacing (Space(..), vspace)
import Lumi.Components.Text (h2_, p_)
import Lumi.Components2.Box (box)
import Lumi.Styles.Box (FlexAlign(..), _alignSelf, _justify, _row)
import React.Basic (JSX)
import React.Basic.Emotion as E

docs :: JSX
docs =
  let
    childBox color =
      lumiElement box
        _
          { css =
            \_ ->
              E.css
                { backgroundColor: E.color color
                , height: E.prop S40
                , width: E.prop S40
                }
          }

    exampleContent =
      [ childBox red
      , childBox green
      , childBox blue
      ]
  in
    lumiElement box
      _
        { content =
          [ p_ "Box is a simple building block component. Lumi components default to building off of Box and generally expect their JSX arguments to be Box-compatible elements."
          , p_ "A Box is essentially a div which defaults to a flex column. The most common flex settings are available as prop modifiers. Nested boxes will also stretch to fill their given space by default. If a component shouldn't grow beyond a specific size under any circumstances, be sure to give it a max-width!"
          , vspace S24
          , h2_ "Defaults"
          , example $ lumiElement box
              $ _ { content = exampleContent }
          , h2_ "Row"
          , example $ lumiElement box
              $ _row
              $ _ { content = exampleContent }
          , h2_ "Align/justify"
          , example $ lumiElement box
              $ _row
              $ _alignSelf Stretch -- only necessary because `example` isn't a Box
              $ _justify End
              $ _ { content = exampleContent }
          , h2_ "Space evenly"
          , example $ lumiElement box
              $ _row
              $ _alignSelf Stretch -- only necessary because `example` isn't a Box
              $ _justify SpaceEvenly
              $ _ { content = exampleContent }
          ]
        }
