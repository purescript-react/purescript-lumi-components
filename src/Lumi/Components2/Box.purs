-- | WARNING: not production ready -- this is a demo of react-basic-emotion and LumiComponent
module Lumi.Components2.Box where

import Prelude

import Effect (Effect)
import Lumi.Components (LumiComponent, lumiComponent)
import Lumi.Styles.Box as Styles.Box
import React.Basic.DOM as R
import React.Basic (JSX)
import React.Basic.Emotion as E
import React.Basic.Hooks as React

type BoxProps = ( content :: Array JSX )

mkBox :: Effect (LumiComponent (BoxProps) (BoxProps))
mkBox = do
  lumiComponent "Box" { content: [], className: "" } \props -> React.do
    pure
      $ E.element Styles.Box.box R.div'
          { children: props.content
          , className: props.className
          }
