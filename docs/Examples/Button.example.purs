module Lumi.Components.Examples.Button where

import Prelude

import Data.Array (intercalate)
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Lumi.Components.Size (Size(..))
import Lumi.Components.Button (ButtonState(..), button, defaults, primary, secondary, linkStyle, iconButton, iconButtonDefaults)
import Lumi.Components.Color (colorNames)
import Lumi.Components.Column (column_)
import Lumi.Components.Icon (IconType(..))
import Lumi.Components.Text (h2_, h4_)
import Lumi.Components.Example (example)
import React.Basic (JSX)
import React.Basic.DOM (css)
import React.Basic.DOM as R

docs :: JSX
docs =
  column_ $
    -- Spacing experiment.. I don't think I like it though
    sections
      [ [ example
            $ button defaults { title = "Button" }
        ]

      , [ h2_ "Disabled"
        , example
            $ button defaults { title = "Button", buttonState = Disabled }
        ]

      , [ h2_ "Size"
        , h4_ "Medium (default)"
        , example
            $ button defaults { title = "Button", size = Medium }
        ]

      , [ h4_ "Small"
        , example
            $ button defaults { title = "Button", size = Small }
        ]
      , [ h4_ "Large"
        , example
            $ button defaults { title = "Button", size = Large }
        ]
      , [ h4_ "Extra Large"
        , example
            $ button defaults { title = "Button", size = ExtraLarge }
        ]

      , [ h2_ "Color"
        , h4_ "Primary"
        , example
            $ button primary { title = "Button" }
        ]
      , [ h4_ "Primary + Disabled"
        , example
            $ button primary { title = "Button", buttonState = Disabled }
        ]
      , [ h4_ "Secondary Medium (default)"
        , example
            $ button secondary { title = "Button" }
        ]
      , [ h4_ "Secondary Small"
        , example
            $ button secondary { title = "Button", size = Small }
        ]
      , [ h4_ "Secondary Large"
        , example
            $ button secondary { title = "Button", size = Large }
        ]
      , [ h4_ "Secondary Extra Large"
        , example
            $ button secondary { title = "Button", size = ExtraLarge }
        ]
      , [ h4_ "Secondary + Disabled"
        , example
            $ button secondary { title = "Button", buttonState = Disabled }
        ]
      , [ h4_ "Icon button"
        , example
            $ iconButton iconButtonDefaults { title = "Add new item", iconLeft = Just $ Plus }
        ]
      , [ h4_ "Icon button"
        , example
            $ iconButton iconButtonDefaults
                { title = "Add new item"
                , iconLeft = Just $ Plus
                , color = toNullable $ Just $ colorNames.secondary
                }
        ]
      , [ h4_ "Icon button"
        , example
            $ iconButton iconButtonDefaults { title = "Add new item", iconRight = Just $ Plus }
        ]
      , [ h4_ "Link style"
        , example
            $ button linkStyle { title = "Button w/ link style" }
        ]
      , [ h4_ "Loading (Medium/default)"
        , example
            $ button primary { buttonState = Loading }
        ]
      , [ h4_ "Loading (Small) "
        , example
            $ button primary { buttonState = Loading, size = Small }
        ]
      , [ h4_ "Loading (Large) "
        , example
            $ button primary { buttonState = Loading, size = Large }
        ]
      , [ h4_ "Loading (ExtraLarge) "
        , example
            $ button primary { buttonState = Loading, size = ExtraLarge }
        ]
      ]

sections :: Array (Array JSX) -> Array JSX
sections children = intercalate [ spacer ] children
  where
    spacer = R.div { style: css { display: "flex", height: 1, paddingBottom: "20px" } }
