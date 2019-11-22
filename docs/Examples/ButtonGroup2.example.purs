module Lumi.Components.Examples.ButtonGroup2 where

import Prelude
import Effect.Console (log)
import Effect.Uncurried (mkEffectFn1)
import Lumi.Components (lumiElement)
import Lumi.Components.Column (column_)
import Lumi.Components.Example (example)
import Lumi.Components.NativeSelect (nativeSelect, defaults)
import Lumi.Components.Text (h2_)
import Lumi.Components2.Button (button, primary, secondary)
import Lumi.Components2.ButtonGroup (buttonGroup)
import React.Basic (JSX)
import React.Basic.DOM as R

docs :: JSX
docs =
  column_
    $ [ h2_ "Not Joined"
      , example
          $ lumiElement buttonGroup
          $ _
              { content =
                [ lumiElement button $ primary _ { content = [ R.text "Button" ] }
                , lumiElement button $ secondary _ { content = [ R.text "Button" ] }
                ]
              }
      , h2_ "Not Joined"
      , example
          $ lumiElement buttonGroup
          $ _
              { content =
                [ lumiElement button $ primary _ { content = [ R.text "Button" ] }
                , lumiElement button $ secondary _ { content = [ R.text "Button" ] }
                , lumiElement button $ secondary _ { content = [ R.text "Button" ] }
                ]
              }
      , h2_ "Not Joined"
      , example
          $ lumiElement buttonGroup
          $ _
              { content =
                [ lumiElement button $ primary _ { content = [ R.text "Button" ] }
                , nativeSelect
                    defaults
                      { options = []
                      , onChange = mkEffectFn1 \_ -> log "onChange"
                      , value = "Foo bar"
                      }
                , lumiElement button $ secondary _ { content = [ R.text "Button" ] }
                ]
              }
      , h2_ "Joined"
      , example
          $ lumiElement buttonGroup
          $ _
              { joined = true
              , content =
                [ lumiElement button $ secondary _ { content = [ R.text "Button" ] }
                , lumiElement button $ secondary _ { content = [ R.text "Button" ] }
                ]
              }
      , h2_ "Joined"
      , example
          $ lumiElement buttonGroup
          $ _
              { joined = true
              , content =
                [ lumiElement button $ secondary _ { content = [ R.text "Button" ] }
                , lumiElement button $ secondary _ { content = [ R.text "Button" ] }
                , lumiElement button $ secondary _ { content = [ R.text "Button" ] }
                ]
              }
      ]
