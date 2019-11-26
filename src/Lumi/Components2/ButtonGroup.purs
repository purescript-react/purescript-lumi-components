module Lumi.Components2.ButtonGroup where

import Prelude
import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components as L
import Lumi.Styles (toCSS)
import Lumi.Styles.Button as Styles.Button
import Lumi.Styles.Theme (useTheme)
import React.Basic.DOM as R
import React.Basic.Emotion as E
import React.Basic.Hooks (JSX)
import React.Basic.Hooks as React

type ButtonGroupProps
  = ( joined :: Boolean
    , content :: Array JSX
    )

buttonGroup :: L.LumiComponent ButtonGroupProps
buttonGroup =
  unsafePerformEffect do
    L.lumiComponent "ButtonGroup" { joined: false, content: [] } \props -> React.do
      theme <- useTheme
      pure
        $ E.element R.div'
            { className: props.className
            , children: props.content
            , css: toCSS theme props (Styles.Button.buttonGroup props.joined)
            }
