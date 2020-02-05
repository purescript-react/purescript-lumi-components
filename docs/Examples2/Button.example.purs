module Lumi.Components2.Examples.Button where

import Prelude
import Data.Array (intercalate)
import Data.Maybe (Maybe(..))
import Lumi.Components (lumiElement)
import Lumi.Components.Column (column_)
import Lumi.Components.Example (example)
import Lumi.Components.Icon (IconType(..), icon)
import Lumi.Components.Size (Size(..))
import Lumi.Components.Spacing (Space(..), hspace, vspace)
import Lumi.Components.Text (h2_, h4_)
import Lumi.Components2.Button (_linkStyle, button, _secondary)
import Lumi.Styles.Box (_interactive)
import Lumi.Styles.Button (ButtonState(..))
import React.Basic (JSX)
import React.Basic.DOM as R
import Web.HTML (window)
import Web.HTML.Window (alert)

docs :: JSX
docs =
  column_
    $ intercalate [ vspace S16 ]
        [ [ example
              $ lumiElement button
              $ _ { content = [ R.text "Button" ] }
          ]
        , [ h2_ "Disabled"
          , example
              $ lumiElement button
              $ _
                  { content = [ R.text "Button" ]
                  , state = Disabled
                  }
          ]
        , [ h2_ "Size"
          , h4_ "Medium (default)"
          , example
              $ lumiElement button
              $ _
                  { content = [ R.text "Button" ]
                  , size = Medium
                  }
          ]
        , [ h4_ "Small"
          , example
              $ lumiElement button
              $ _
                  { content = [ R.text "Button" ]
                  , size = Small
                  }
          ]
        , [ h4_ "Large"
          , example
              $ lumiElement button
              $ _
                  { content = [ R.text "Button" ]
                  , size = Large
                  }
          ]
        , [ h4_ "Extra Large"
          , example
              $ lumiElement button
              $ _
                  { content = [ R.text "Button" ]
                  , size = ExtraLarge Nothing
                  }
          ]
        , [ h2_ "Color"
          , h4_ "Primary (default)"
          , example
              $ lumiElement button
              $ _interactive
              $ _ { content = [ R.text "Button" ] }
          ]
        , [ h4_ "Primary + Disabled"
          , example
              $ lumiElement button
              $ _
                  { content = [ R.text "Button" ]
                  , state = Disabled
                  }
          ]
        , [ h4_ "Secondary (outline)"
          , example
              $ lumiElement button
              $ _secondary
              $ _ { content = [ R.text "Button" ] }
          ]
        , [ h4_ "Secondary Small"
          , example
              $ lumiElement button
              $ _secondary
              $ _
                  { content = [ R.text "Button" ]
                  , size = Small
                  }
          ]
        , [ h4_ "Secondary Large"
          , example
              $ lumiElement button
              $ _secondary
              $ _
                  { content = [ R.text "Button" ]
                  , size = Large
                  }
          ]
        , [ h4_ "Secondary Extra Large"
          , example
              $ lumiElement button
              $ _secondary
              $ _
                  { content = [ R.text "Button" ]
                  , size = ExtraLarge Nothing
                  }
          ]
        , [ h4_ "Secondary + Disabled"
          , example
              $ lumiElement button
              $ _secondary
              $ _
                  { content = [ R.text "Button" ]
                  , state = Disabled
                  }
          ]
        , [ h4_ "Icon button"
          , example
              $ lumiElement button
              $ _ { content = [ buttonIcon Plus, hspace S8, R.text "Add new item" ] }
          ]
        , [ h4_ "Icon button"
          , example
              $ lumiElement button
              $ _secondary
              $ _ { content = [ buttonIcon Plus, hspace S8, R.text "Add new item" ] }
          ]
        , [ h4_ "Icon button"
          , example
              $ lumiElement button
              $ _ { content = [ R.text "Add new item", hspace S8, buttonIcon Plus ] }
          ]
        , [ h4_ "Link style"
          , example
              $ lumiElement button
              $ _linkStyle
              $ _ { content = [ R.text "Button w/ link style" ], onPress = alert "asdf" =<< window }
          , example
              $ lumiElement button
              $ _linkStyle
              $ _ { state = Disabled, content = [ R.text "Button w/ link style" ], onPress = alert "asdf" =<< window }
          ]
        , [ h4_ "Loading (Medium/default)"
          , example
              $ lumiElement button
              $ _ { state = Loading }
          ]
        , [ h4_ "Loading (Small) "
          , example
              $ lumiElement button
              $ _
                  { state = Loading
                  , size = Small
                  }
          ]
        , [ h4_ "Loading (Large) "
          , example
              $ lumiElement button
              $ _
                  { state = Loading
                  , size = Large
                  }
          ]
        , [ h4_ "Loading (ExtraLarge) "
          , example
              $ lumiElement button
              $ _
                  { state = Loading
                  , size = ExtraLarge Nothing
                  }
          ]
        ]
  where
  buttonIcon type_ = icon { type_, style: R.css { fontSize: 11 } }
