module Lumi.Components.Examples.Button2 where

import Prelude

import Data.Array (intercalate)
import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components.Color (colors)
import Lumi.Components.Column (column_)
import Lumi.Components.Example (example)
import Lumi.Components.Icon (IconType(..), icon)
import Lumi.Components.Size (Size(..))
import Lumi.Components.Spacing (Space(..), hspace, vspace)
import Lumi.Components.Text (h2_, h4_)
import Lumi.Components2.Button (defaults, linkStyle, mkButton, primary, secondary)
import Lumi.Styles.Button (ButtonState(..))
import Lumi.Styles.Theme (LumiTheme)
import React.Basic (JSX, createContext, element)
import React.Basic.DOM as R

theme :: LumiTheme
theme = { colors }

docs :: JSX
docs = unsafePerformEffect do
  t <- createContext theme
  button <- mkButton t
  pure $ column_ $
    intercalate [ vspace S16 ]
      [ [ example
            $ element button defaults { content = [ R.text "Button" ] }
        ]

      , [ h2_ "Disabled"
        , example
            $ element button defaults { content = [ R.text "Button" ], buttonState = Disabled }
        ]

      , [ h2_ "Size"
        , h4_ "Medium (default)"
        , example
            $ element button defaults { content = [ R.text "Button" ], size = Medium }
        ]

      , [ h4_ "Small"
        , example
            $ element button defaults { content = [ R.text "Button" ], size = Small }
        ]
      , [ h4_ "Large"
        , example
            $ element button defaults { content = [ R.text "Button" ], size = Large }
        ]
      , [ h4_ "Extra Large"
        , example
            $ element button defaults { content = [ R.text "Button" ], size = ExtraLarge }
        ]

      , [ h2_ "Color"
        , h4_ "Primary"
        , example
            $ element button primary { content = [ R.text "Button" ] }
        ]
      , [ h4_ "Primary + Disabled"
        , example
            $ element button primary { content = [ R.text "Button" ], buttonState = Disabled }
        ]
      , [ h4_ "Secondary Medium (default)"
        , example
            $ element button secondary { content = [ R.text "Button" ] }
        ]
      , [ h4_ "Secondary Small"
        , example
            $ element button secondary { content = [ R.text "Button" ], size = Small }
        ]
      , [ h4_ "Secondary Large"
        , example
            $ element button secondary { content = [ R.text "Button" ], size = Large }
        ]
      , [ h4_ "Secondary Extra Large"
        , example
            $ element button secondary { content = [ R.text "Button" ], size = ExtraLarge }
        ]
      , [ h4_ "Secondary + Disabled"
        , example
            $ element button secondary { content = [ R.text "Button" ], buttonState = Disabled }
        ]
      , [ h4_ "Icon button"
        , example
            $ element button defaults { content = [ buttonIcon Plus, hspace S8, R.text "Add new item" ] }
        ]
      , [ h4_ "Icon button"
        , example
            $ element button secondary { content = [ buttonIcon Plus, hspace S8, R.text "Add new item" ] }
        ]
      , [ h4_ "Icon button"
        , example
            $ element button defaults { content = [ R.text "Add new item", hspace S8, buttonIcon Plus ] }
        ]
      , [ h4_ "Link style"
        , example
            $ element button linkStyle { content = [ R.text "Button w/ link style" ] }
        ]
      , [ h4_ "Loading (Medium/default)"
        , example
            $ element button primary { buttonState = Loading }
        ]
      , [ h4_ "Loading (Small) "
        , example
            $ element button primary { buttonState = Loading, size = Small }
        ]
      , [ h4_ "Loading (Large) "
        , example
            $ element button primary { buttonState = Loading, size = Large }
        ]
      , [ h4_ "Loading (ExtraLarge) "
        , example
            $ element button primary { buttonState = Loading, size = ExtraLarge }
        ]
      ]
  where
    buttonIcon type_ = icon { type_, style: R.css { fontSize: 11 } }
