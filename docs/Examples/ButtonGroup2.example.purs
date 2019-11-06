module Lumi.Components.Examples.ButtonGroup2 where

import Prelude

import Effect.Console (log)
import Effect.Uncurried (mkEffectFn1)
import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components (lumiElement)
import Lumi.Components.Color (colors)
import Lumi.Components.Column (column_)
import Lumi.Components.Example (example)
import Lumi.Components.NativeSelect (nativeSelect, defaults)
import Lumi.Components.Text (h2_)
import Lumi.Components2.Button (mkButton, primary, secondary)
import Lumi.Components2.ButtonGroup (mkButtonGroup)
import Lumi.Styles.Theme (LumiTheme)
import React.Basic (JSX, createContext)
import React.Basic.DOM as R

theme :: LumiTheme
theme = { colors }

docs :: JSX
docs = unsafePerformEffect do
  t <- createContext theme
  button <- mkButton t
  buttonGroup <- mkButtonGroup t

  pure $ column_ $
    [ h2_ "Not Joined"
    , example
        $ lumiElement buttonGroup _
            { content =
                [ lumiElement button \_ -> primary { content = [ R.text "Button" ] }
                , lumiElement button \_ -> secondary { content = [ R.text "Button" ] }
                ]
            }
    , h2_ "Not Joined"
    , example
        $ lumiElement buttonGroup _
            { content =
                [ lumiElement button \_ -> primary { content = [ R.text "Button" ] }
                , lumiElement button \_ -> secondary { content = [ R.text "Button" ] }
                , lumiElement button \_ -> secondary { content = [ R.text "Button" ] }
                ]
            }
    , h2_ "Not Joined"
    , example
        $ lumiElement buttonGroup _
            { content =
                [ lumiElement button \_ -> primary { content = [ R.text "Button" ] }
                , nativeSelect defaults
                    { options = []
                    , onChange = mkEffectFn1 \_ -> log "onChange"
                    , value = "Foo bar"
                    }
                , lumiElement button \_ -> secondary { content = [ R.text "Button" ] }
                ]
            }
    , h2_ "Joined"
    , example
        $ lumiElement buttonGroup _
            { content =
                [ lumiElement button \_ -> secondary { content = [ R.text "Button" ] }
                , lumiElement button \_ -> secondary { content = [ R.text "Button" ] }
                ]
            }
    , h2_ "Joined"
    , example
        $ lumiElement buttonGroup _
            { content =
                [ lumiElement button \_ -> secondary { content = [ R.text "Button" ] }
                , lumiElement button \_ -> secondary { content = [ R.text "Button" ] }
                , lumiElement button \_ -> secondary { content = [ R.text "Button" ] }
                ]
            }
    ]
