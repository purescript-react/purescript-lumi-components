module Lumi.Components.Examples.Button2 where

import Prelude

import Data.Array (intercalate)
import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components (lumiComponent, lumiElement, withContent)
import Lumi.Components.Column (column_)
import Lumi.Components.Example (example)
import Lumi.Components.Icon (IconType(..), icon)
import Lumi.Components.Size (Size(..))
import Lumi.Components.Spacing (Space(..), hspace, vspace)
import Lumi.Components.Text (h2_, h4_)
import Lumi.Components2.Button (linkStyle, button, primary, secondary)
import Lumi.Styles (withStyle)
import Lumi.Styles.Button (ButtonState(..))
import Lumi.Styles.Theme (useTheme)
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.Hooks as React

docs :: JSX
docs = (flip lumiElement identity) do
  unsafePerformEffect do
    lumiComponent "SlatExample" {className: "" }\_ -> React.do
      theme <- useTheme

      pure $ column_ $
        intercalate [ vspace S16 ]
          [ [ example
                $ lumiElement button _ { content = [ R.text "Button" ] }
            ]

          , [ h2_ "Disabled"
            , example
                $ lumiElement button _ { content = [ R.text "Button" ], buttonState = Disabled }
            ]

          , [ h2_ "Size"
            , h4_ "Medium (default)"
            , example
                $ lumiElement button _ { content = [ R.text "Button" ], size = Medium }
            ]

          , [ h4_ "Small"
            , example
                $ lumiElement button _ { content = [ R.text "Button" ], size = Small }
            ]
          , [ h4_ "Large"
            , example
                $ lumiElement button _ { content = [ R.text "Button" ], size = Large }
            ]
          , [ h4_ "Extra Large"
            , example
                $ lumiElement button _ { content = [ R.text "Button" ], size = ExtraLarge }
            ]

          , [ h2_ "Color"
            , h4_ "Primary"
            , example
                $ lumiElement button $ withStyle theme primary >>> withContent [ R.text "Button" ]
            ]
          , [ h4_ "Primary + Disabled"
            , example
                $ lumiElement button $ withStyle theme primary >>> _{ content = [ R.text "Button" ], buttonState = Disabled }
            ]
          , [ h4_ "Secondary Medium (default)"
            , example
                $ lumiElement button $ withStyle theme secondary >>> _{ content = [ R.text "Button" ] }
            ]
          , [ h4_ "Secondary Small"
            , example
                $ lumiElement button $ withStyle theme secondary >>> _{ content = [ R.text "Button" ], size = Small }
            ]
          , [ h4_ "Secondary Large"
            , example
                $ lumiElement button $ withStyle theme secondary >>> _{ content = [ R.text "Button" ], size = Large }
            ]
          , [ h4_ "Secondary Extra Large"
            , example
                $ lumiElement button $ withStyle theme secondary >>> _{ content = [ R.text "Button" ], size = ExtraLarge }
            ]
          , [ h4_ "Secondary + Disabled"
            , example
                $ lumiElement button $ withStyle theme secondary >>> _{ content = [ R.text "Button" ], buttonState = Disabled }
            ]
          , [ h4_ "Icon button"
            , example
                $ lumiElement button _ { content = [ buttonIcon Plus, hspace S8, R.text "Add new item" ] }
            ]
          , [ h4_ "Icon button"
            , example
                $ lumiElement button $ withStyle theme secondary >>> _{ content = [ buttonIcon Plus, hspace S8, R.text "Add new item" ] }
            ]
          , [ h4_ "Icon button"
            , example
                $ lumiElement button _ { content = [ R.text "Add new item", hspace S8, buttonIcon Plus ] }
            ]
          , [ h4_ "Link style"
            , example
                $ lumiElement button $ withStyle theme linkStyle >>> withContent [ R.text "Button w/ link style" ]
            ]
          , [ h4_ "Loading (Medium/default)"
            , example
                $ lumiElement button $ withStyle theme primary >>> _{ buttonState = Loading }
            ]
          , [ h4_ "Loading (Small) "
            , example
                $ lumiElement button $ withStyle theme primary >>> _{ buttonState = Loading, size = Small }
            ]
          , [ h4_ "Loading (Large) "
            , example
                $ lumiElement button $ withStyle theme primary >>> _{ buttonState = Loading, size = Large }
            ]
          , [ h4_ "Loading (ExtraLarge) "
            , example
                $ lumiElement button $ withStyle theme primary >>> _{ buttonState = Loading, size = ExtraLarge }
            ]
          ]
  where
    buttonIcon type_ = icon { type_, style: R.css { fontSize: 11 } }
