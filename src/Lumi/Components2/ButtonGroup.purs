module Lumi.Components2.ButtonGroup where

import Prelude

import Lumi.Components (LumiComponent')
import Lumi.Styles.Button as Styles.Button
import React.Basic.DOM as R
import React.Basic.Emotion as E
import React.Basic.Hooks (component)

type ButtonGroupProps
  = ( joined :: Boolean
    )

mkButtonGroup :: LumiComponent' ButtonGroupProps
mkButtonGroup t = do
  component "ButtonGroup" \props ->
    pure
      $ E.element R.div'
          { css: Styles.Button.buttonGroup props.joined
          , className: props.className
          , children: props.content
          }
