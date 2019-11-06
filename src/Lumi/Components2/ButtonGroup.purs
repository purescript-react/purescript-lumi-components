module Lumi.Components2.ButtonGroup where

import Prelude

import Effect (Effect)
import Lumi.Components (LumiComponent, lumiComponent)
import Lumi.Styles.Button as Styles.Button
import Lumi.Styles.Theme (LumiTheme)
import React.Basic.DOM as R
import React.Basic.Emotion as E
import React.Basic.Hooks (JSX, ReactContext)

type ButtonGroupProps
  = ( joined :: Boolean
    , content :: Array JSX
    )

mkButtonGroup :: ReactContext LumiTheme -> Effect (LumiComponent ButtonGroupProps ButtonGroupProps)
mkButtonGroup t = do
  lumiComponent "ButtonGroup" { className: "", joined: false, content: [] } \props ->
    pure
      $ E.element
          (Styles.Button.buttonGroup props.joined)
          R.div'
          { className: props.className
          , children: props.content
          }
