module Lumi.Components2.Slat where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Effect (Effect)
import Lumi.Components (LumiComponent, lumiComponent)
import Lumi.Components as L
import Lumi.Styles (StyleModifier, styleModifier_, toCSS)
import Lumi.Styles as Styles
import Lumi.Styles.Border as Border
import Lumi.Styles.Box as Box
import Lumi.Styles.Slat as Styles.Slat
import Lumi.Styles.Theme (LumiTheme)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)
import React.Basic.Emotion as E
import React.Basic.Hooks (JSX, ReactContext, useContext)
import React.Basic.Hooks as React
import Web.HTML.History (URL(..))

type SlatProps
  = ( content :: Array JSX
    , interaction :: Maybe SlatInteraction
    )

type SlatInteraction
  = { onClick :: Effect Unit
    , tabIndex :: Int
    , href :: Maybe URL
    }

mkSlat ::
  ReactContext LumiTheme ->
  Effect (LumiComponent SlatProps)
mkSlat t =
  lumiComponent "Slat" defaults \props@{ className, style } -> React.do
    theme <- useContext t
    let
      slatStyle :: StyleModifier
      slatStyle = L.do
        Styles.Slat.slat
        appearanceNone
        Styles.styleModifier style
    pure
      case props.interaction of
        Nothing ->
          E.element R.div'
            { css: toCSS theme slatStyle
            , children: props.content
            , className
            }
        Just interaction@{ href: Nothing } ->
          E.element R.button'
            { css: toCSS theme slatStyle
            , children: props.content
            , onClick: capture_ interaction.onClick
            , tabIndex: interaction.tabIndex
            , className
            }
        Just interaction@{ href: Just href } ->
          E.element R.a'
            { css: toCSS theme slatStyle
            , children: props.content
            , onClick: capture_ interaction.onClick
            , tabIndex: interaction.tabIndex
            , href: un URL href
            , className
            }
  where
    defaults :: Record SlatProps
    defaults =
      { content: []
      , interaction: Nothing
      }

    appearanceNone :: StyleModifier
    appearanceNone = styleModifier_ $ E.css { appearance: E.none }

interactive :: SlatInteraction -> L.PropsModifier SlatProps
interactive interaction = L.do
  Border.interactive
  Box.interactive
  L.propsModifier _
    { interaction = Just interaction
    }
