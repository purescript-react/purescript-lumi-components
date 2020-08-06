module Lumi.Components2.Examples.ButtonGroup where

import Prelude

import Effect.Console (log)
import Effect.Uncurried (mkEffectFn1)
import Lumi.Components (($$$))
import Lumi.Components.Column (column_)
import Lumi.Components.Example (example)
import Lumi.Components.NativeSelect (nativeSelect, defaults)
import Lumi.Components.Text (h2_)
import Lumi.Components2.Button (button, secondary)
import Lumi.Components2.ButtonGroup (buttonGroup, joined)
import React.Basic.Classic (JSX)
import React.Basic.DOM as R

docs :: JSX
docs =
  column_
    $ [ h2_ "Not Joined"
      , example
          $ buttonGroup
          $ _
              { content =
                [ button _ { content = [ R.text "Button" ] }
                , button
                    $ secondary
                    $ _ { content = [ R.text "Button" ] }
                ]
              }
      , h2_ "Not Joined"
      , example
          $ buttonGroup
          $ _
              { content =
                [ button _ { content = [ R.text "Button" ] }
                , button
                    $ secondary
                    $ _ { content = [ R.text "Button" ] }
                , button
                    $ secondary
                    $ _ { content = [ R.text "Button" ] }
                ]
              }
      , h2_ "Not Joined"
      , example
          $ buttonGroup
          $ _
              { content =
                [ button _ { content = [ R.text "Button" ] }
                , nativeSelect
                    defaults
                      { options = []
                      , onChange = mkEffectFn1 \_ -> log "onChange"
                      , value = "Foo bar"
                      }
                , button
                    $ secondary
                    $ _ { content = [ R.text "Button" ] }
                ]
              }
      , h2_ "Joined"
      , example
          $ buttonGroup
          $ joined
          $$$
            [ button
                $ secondary
                $ _ { content = [ R.text "Button" ] }
            , button
                $ secondary
                $ _ { content = [ R.text "Button" ] }
            ]
      , h2_ "Joined"
      , example
          $ buttonGroup
          $ joined
          $$$
            [ button
                $ secondary
                $ _ { content = [ R.text "Button" ] }
            , button
                $ secondary
                $ _ { content = [ R.text "Button" ] }
            , button
                $ secondary
                $ _ { content = [ R.text "Button" ] }
            ]
      ]
