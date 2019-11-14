-- | WARNING: not production ready -- this is a demo of react-basic-emotion and LumiComponent
module Lumi.Components2.Box where

import Prelude

import Effect (Effect)
import Lumi.Components (LumiComponent, lumiComponent)
import Lumi.Styles (styleModifier, toCSS)
import Lumi.Styles.Box as Styles.Box
import Lumi.Styles.Theme (LumiTheme)
import React.Basic (JSX, ReactContext)
import React.Basic.DOM as R
import React.Basic.Emotion as E
import React.Basic.Hooks as React

type BoxProps = ( content :: Array JSX )

mkBox :: ReactContext LumiTheme -> Effect (LumiComponent BoxProps)
mkBox t = do
  lumiComponent "Box" { content: [] } \props -> React.do
    theme <- React.useContext t
    pure
      $ E.element R.div'
          { children: props.content
          , className: props.className
          , css: toCSS theme (Styles.Box.box >>> styleModifier props.style)
          }
