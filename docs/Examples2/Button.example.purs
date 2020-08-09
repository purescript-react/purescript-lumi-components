module Lumi.Components2.Examples.Button where

import Prelude

import Color (lighten)
import Color.Scheme.MaterialDesign as Colors
import Data.Array (intercalate)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Class (liftEffect)
import Lumi.Components (($$$))
import Lumi.Components.Column (column_)
import Lumi.Components.Example (example)
import Lumi.Components.Icon (IconType(..), icon)
import Lumi.Components.Size (Size(..))
import Lumi.Components.Spacing (Space(..), hspace, vspace)
import Lumi.Components.Text (h2_, h4_)
import Lumi.Components2.Button (ButtonState(..), button, linkButton, loadingContent, onPress, resize, secondary, submit)
import Lumi.Styles (style_)
import Lumi.Styles as S
import Lumi.Styles.Box (_interactive)
import React.Basic.Classic (JSX)
import React.Basic.DOM as R
import Web.HTML (window)
import Web.HTML.Window (alert)

docs :: JSX
docs =
  column_
    $ intercalate [ vspace S16 ]
        [ [ example
              $ button
              $ onPress do
                  delay $ Milliseconds 1000.0
              $$$ [ column_ [ R.text "Click me" ] ]
          ]
        , [ h2_ "Disabled"
          , example
              $ button
              $$$ [ R.text "Button" ]
          ]
        , [ h2_ "Size"
          , h4_ "Medium (default)"
          , example
              $ button
              $ resize Medium
              $$$ [ R.text "Button" ]
          ]
        , [ h4_ "Small"
          , example
              $ button
              $ resize Small
              $$$ [ R.text "Button" ]
          ]
        , [ h4_ "Large"
          , example
              $ button
              $ resize Large
              $$$ [ R.text "Button" ]
          ]
        , [ h4_ "Extra Large"
          , example
              $ button
              $ resize ExtraLarge
              $$$ [ R.text "Button" ]
          ]
        , [ h2_ "Color"
          , h4_ "Primary (default)"
          , example
              $ button
              $ _interactive
              $$$ [ R.text "Button" ]
          ]
        , [ h4_ "Primary + Disabled"
          , example
              $ button
              $ submit Disabled
              $$$ [ R.text "Button" ]
          ]
        , [ h4_ "Secondary (outline)"
          , example
              $ button
              $ secondary
              $$$ [ R.text "Button" ]
          ]
        , [ h4_ "Secondary Small"
          , example
              $ button
              $ secondary
              $ resize Small
              $$$ [ R.text "Button" ]
          ]
        , [ h4_ "Secondary Large"
          , example
              $ button
              $ secondary
              $ resize Large
              $$$ [ R.text "Button" ]
          ]
        , [ h4_ "Secondary Extra Large"
          , example
              $ button
              $ secondary
              $ resize ExtraLarge
              $$$ [ R.text "Button" ]
          ]
        , [ h4_ "Secondary + Disabled"
          , example
              $ button
              $ secondary
              $ submit Disabled
              $$$ [ R.text "Button" ]
          ]
        , [ h4_ "Icon button"
          , example
              $ button
              $$$ [ buttonIcon Plus, hspace S8, R.text "Add new item" ]
          ]
        , [ h4_ "Icon button"
          , example
              $ button
              $ secondary
              $$$ [ buttonIcon Plus, hspace S8, R.text "Add new item" ]
          ]
        , [ h4_ "Icon button"
          , example
              $ button
              $$$ [ R.text "Add new item", hspace S8, buttonIcon Plus ]
          ]
        , [ h4_ "Link style"
          , example
              $ linkButton
              $ onPress do
                  liftEffect do alert "asdf" =<< window
              $ loadingContent [ R.text "Loading..." ]
              $$$ [ R.text "Button w/ link style" ]
          , example
              $ linkButton
              $ style_
                  ( S.css
                      { borderColor: S.color $ lighten 0.4 Colors.purple
                      , borderStyle: S.solid
                      , borderSize: S.px 2
                      }
                  )
              $ onPress do
                  delay $ Milliseconds 1000.0
              $ loadingContent [ R.text "Border size remains unchanged -->" ]
              $$$ [ R.text "Link buttons retain their size when displaying their loading state (click me)" ]
          , example
              $ linkButton
              $ submit Disabled
              $ onPress do
                  liftEffect do alert "asdf" =<< window
              $$$ [ R.text "Button w/ link style (disabled)" ]
          ]
        , [ h4_ "Loading (Medium/default)"
          , example
              $ button
              $ submit Loading
              $$$ [ R.text "Save" ]
          ]
        , [ h4_ "Loading (Small) "
          , example
              $ button
              $ resize Small
              $ submit Loading
              $$$ [ R.text "Save" ]
          ]
        , [ h4_ "Loading (Large) "
          , example
              $ button
              $ resize Large
              $ submit Loading
              $$$ [ R.text "Save" ]
          ]
        , [ h4_ "Loading (ExtraLarge) "
          , example
              $ button
              $ resize ExtraLarge
              $ submit Loading
              $$$ [ R.text "Save" ]
          ]
        ]
  where
  buttonIcon type_ = icon { type_, style: R.css { fontSize: 11 } }
