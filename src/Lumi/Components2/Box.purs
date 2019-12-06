module Lumi.Components2.Box where

import Prelude

import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components (LumiComponent, lumiComponent)
import Lumi.Styles (toCSS)
import Lumi.Styles.Box as Styles.Box
import Lumi.Styles.Theme (useTheme)
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.Emotion as E
import React.Basic.Events (EventHandler, handler_)
import React.Basic.Hooks as React

type BoxProps
  = ( content :: Array JSX
    , onClick :: EventHandler
    )

box :: LumiComponent BoxProps
box =
  unsafePerformEffect do
    lumiComponent "Box" defaults \props -> React.do
      theme <- useTheme
      pure
        $ E.element R.div'
            { children: props.content
            , className: props.className
            , css: toCSS theme props Styles.Box.box
            , onClick: props.onClick
            }
  where
    defaults :: Record BoxProps
    defaults =
      { content: []
      , onClick: handler_ mempty
      }
