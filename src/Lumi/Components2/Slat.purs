module Lumi.Components2.Slat where

import Prelude

import Effect (Effect)
import Lumi.Components (LumiComponent, lumiComponent)
import Lumi.Styles.Border (Border(..))
import Lumi.Styles.Slat as Styles.Slat
import Lumi.Styles.Theme (LumiTheme)
import React.Basic.DOM as R
import React.Basic.Emotion as E
import React.Basic.Hooks (JSX, ReactContext, useContext)
import React.Basic.Hooks as React

type SlatProps
  = ( content :: Array JSX, border :: Border )

mkSlat :: ReactContext LumiTheme -> Effect (LumiComponent SlatProps)
mkSlat t = do
  lumiComponent "Slat" { content: [], className: "", border: BorderRound } \props -> React.do
    theme <- useContext t
    let
      slatStyle = Styles.Slat.slat theme props.border
    pure $
      E.element slatStyle
        R.div'
          { children: props.content
          , className: props.className
          }
