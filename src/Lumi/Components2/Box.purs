-- | WARNING: not production ready -- this is a demo of react-basic-emotion and LumiComponent
module Lumi.Components2.Box where

import Prelude

import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components (LumiComponent, lumiComponent)
import Lumi.Styles (toCSS)
import Lumi.Styles.Box as Styles.Box
import Lumi.Styles.Theme (lumiThemeContext)
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.Emotion as E
import React.Basic.Hooks as React

type BoxProps
  = ( content :: Array JSX )

box :: LumiComponent BoxProps
box = unsafePerformEffect do
  lumiComponent "Box" { content: [] } \props -> React.do
    theme <- React.useContext lumiThemeContext
    pure
      $ E.element R.div'
          { children: props.content
          , className: props.className
          , css: toCSS theme props Styles.Box.box
          }
