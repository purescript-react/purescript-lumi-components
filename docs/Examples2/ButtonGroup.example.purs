module Lumi.Components2.Examples.ButtonGroup where

import Prelude
import Effect.Console (log)
import Effect.Uncurried (mkEffectFn1)
import Lumi.Components.Column (column_)
import Lumi.Components.Example (example)
import Lumi.Components.NativeSelect (nativeSelect, defaults)
import Lumi.Components.Text (h2_)
import Lumi.Components2.Button (button, _secondary)
import Lumi.Components2.ButtonGroup (buttonGroup)
import React.Basic (JSX)
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
                    $ _secondary
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
                    $ _secondary
                    $ _ { content = [ R.text "Button" ] }
                , button
                    $ _secondary
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
                    $ _secondary
                    $ _ { content = [ R.text "Button" ] }
                ]
              }
      , h2_ "Joined"
      , example
          $ buttonGroup
          $ _
              { joined = true
              , content =
                [ button
                    $ _secondary
                    $ _ { content = [ R.text "Button" ] }
                , button
                    $ _secondary
                    $ _ { content = [ R.text "Button" ] }
                ]
              }
      , h2_ "Joined"
      , example
          $ buttonGroup
          $ _
              { joined = true
              , content =
                [ button
                    $ _secondary
                    $ _ { content = [ R.text "Button" ] }
                , button
                    $ _secondary
                    $ _ { content = [ R.text "Button" ] }
                , button
                    $ _secondary
                    $ _ { content = [ R.text "Button" ] }
                ]
              }
      ]
