-- | WARNING: not production ready -- this is a demo of react-basic-emotion and LumiComponent
module Lumi.Components2.ButtonGroup where

import Prelude

import Effect (Effect)
import Lumi.Components as L
import Lumi.Styles (styleModifier, toCSS)
import Lumi.Styles as Styles
import Lumi.Styles.Button as Styles.Button
import Lumi.Styles.Theme (LumiTheme)
import React.Basic.DOM as R
import React.Basic.Emotion as E
import React.Basic.Hooks (JSX, ReactContext, useContext)
import React.Basic.Hooks as React

type ButtonGroupProps
  = ( joined :: Boolean
    , content :: Array JSX
    )

mkButtonGroup :: ReactContext LumiTheme -> Effect (L.LumiComponent ButtonGroupProps)
mkButtonGroup t = do
  L.lumiComponent "ButtonGroup" { joined: false, content: [] } \props -> React.do
    theme <- useContext t
    pure
      $ E.element R.div'
          { className: props.className
          , children: props.content
          , css: toCSS theme Styles.do
              Styles.Button.buttonGroup props.joined
              styleModifier props.style
          }
