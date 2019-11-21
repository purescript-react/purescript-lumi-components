module Lumi.Components.Examples.ButtonGroup2 where

import Prelude

import Effect.Console (log)
import Effect.Uncurried (mkEffectFn1)
import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components (lumiComponent, lumiElement, withContent)
import Lumi.Components.Column (column_)
import Lumi.Components.Example (example)
import Lumi.Components.NativeSelect (nativeSelect, defaults)
import Lumi.Components.Text (h2_)
import Lumi.Components2.Button (button, primary, secondary)
import Lumi.Components2.ButtonGroup (buttonGroup)
import Lumi.Styles (withStyle)
import Lumi.Styles.Theme (useTheme)
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.Hooks as React

docs :: JSX
docs = (flip lumiElement identity) do
  unsafePerformEffect do
    lumiComponent "SlatExample" {className: "" }\_ -> React.do
      theme <- useTheme

      pure $ column_
        $ [ h2_ "Not Joined"
          , example
              $ lumiElement buttonGroup
                  $ withContent
                      [ lumiElement button $ withStyle theme primary >>> withContent [ R.text "Button" ]
                      , lumiElement button $ withStyle theme secondary >>> withContent [ R.text "Button" ]
                      ]
          , h2_ "Not Joined"
          , example
              $ lumiElement buttonGroup
                  $ withContent
                      [ lumiElement button $ withStyle theme primary >>> withContent [ R.text "Button" ]
                      , lumiElement button $ withStyle theme secondary >>> withContent [ R.text "Button" ]
                      , lumiElement button $ withStyle theme secondary >>> withContent [ R.text "Button" ]
                      ]
          , h2_ "Not Joined"
          , example
              $ lumiElement buttonGroup
                  $ withContent
                      [ lumiElement button $ withStyle theme primary >>> withContent [ R.text "Button" ]
                      , nativeSelect
                          defaults
                            { options = []
                            , onChange = mkEffectFn1 \_ -> log "onChange"
                            , value = "Foo bar"
                            }
                      , lumiElement button $ withStyle theme secondary >>> withContent [ R.text "Button" ]
                      ]
          , h2_ "Joined"
          , example
              $ lumiElement buttonGroup
                  $ withContent
                      [ lumiElement button $ withStyle theme secondary >>> withContent [ R.text "Button" ]
                      , lumiElement button $ withStyle theme secondary >>> withContent [ R.text "Button" ]
                      ]
          , h2_ "Joined"
          , example
              $ lumiElement buttonGroup
                  $ withContent
                      [ lumiElement button $ withStyle theme secondary >>> withContent [ R.text "Button" ]
                      , lumiElement button $ withStyle theme secondary >>> withContent [ R.text "Button" ]
                      , lumiElement button $ withStyle theme secondary >>> withContent [ R.text "Button" ]
                      ]
          ]
