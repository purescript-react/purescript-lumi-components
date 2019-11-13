-- | WARNING: not production ready -- this is a demo of react-basic-emotion and LumiComponent
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

mkButtonGroup :: ReactContext LumiTheme -> Effect (LumiComponent ButtonGroupProps)
mkButtonGroup t = do
  lumiComponent "ButtonGroup" { joined: false, content: [] } \props ->
    pure
      $ E.element R.div'
          { className: props.className
          , children: props.content
          , css: Styles.Button.buttonGroup props.joined
          }
