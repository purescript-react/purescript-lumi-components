module Lumi.Components2.Slat where

import Prelude

import Data.Maybe (Maybe(..), isJust)
import Effect (Effect)
import Lumi.Components (LumiComponent, lumiComponent, (%))
import Lumi.Components2.Box (mkBox)
import Lumi.Styles.Border (Border(..))
import Lumi.Styles.Slat as Styles.Slat
import Lumi.Styles.Theme (LumiTheme)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)
import React.Basic.Emotion as E
import React.Basic.Events (EventHandler)
import React.Basic.Hooks (JSX, ReactContext, useContext)
import React.Basic.Hooks as React
import Simple.JSON (undefined)
import Unsafe.Coerce (unsafeCoerce)

type SlatProps
  = ( content :: Array JSX
    , border :: Border
    , onClick :: Maybe (Effect Unit)
    )

mkSlat ::
  ReactContext LumiTheme ->
  Effect
    { slat :: LumiComponent SlatProps
    , slatSpacer :: JSX
    }
mkSlat t = do
  box <- mkBox t
  slat <- lumiComponent
    "Slat"
    { content: []
    , className: ""
    , border: BorderRound
    , onClick: Nothing
    }
    ( \props -> React.do
        theme <- useContext t
        let
          slatStyle = Styles.Slat.slat theme props.border (isJust props.onClick)
        pure
          $ E.element slatStyle R.div'
              { children: props.content
              , className: props.className
              , onClick:
                case props.onClick of
                  Nothing -> unsafeCoerce undefined :: EventHandler
                  Just onClick -> capture_ onClick
              }
    )
  slatSpacer <- lumiComponent
    "SlatSpacer"
    { className: "" }
    (pure <<< R.div)
  pure { slat, slatSpacer: slatSpacer % identity }
