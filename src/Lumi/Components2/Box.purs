module Lumi.Components2.Box where

import Prelude

import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components (LumiComponent, lumiComponent)
import Lumi.Styles (toCSS)
import Lumi.Styles.Box as Styles.Box
import Lumi.Styles.Theme (useTheme)
import React.Basic.Classic (JSX)
import React.Basic.DOM as R
import React.Basic.Emotion as E
import React.Basic.Hooks as React

type BoxProps
  = ( content :: Array JSX )

box :: LumiComponent BoxProps
box =
  unsafePerformEffect do
    lumiComponent "Box" { content: [] } \props -> React.do
      theme <- useTheme
      pure
        $ E.element R.div'
            { children: props.content
            , className: props.className
            , css: theme # toCSS Styles.Box.box <> props.css
            }

row :: LumiComponent BoxProps
row = box <<< Styles.Box._row

column :: LumiComponent BoxProps
column = box <<< Styles.Box._column
