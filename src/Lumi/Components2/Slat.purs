module Lumi.Components2.Slat where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components (LumiComponent, lumiComponent, propsModifier)
import Lumi.Styles (StyleModifier, styleModifier_, toCSS)
import Lumi.Styles.Border as Border
import Lumi.Styles.Box as Box
import Lumi.Styles.Slat as Styles.Slat
import Lumi.Styles.Theme (lumiThemeContext)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)
import React.Basic.Emotion as E
import React.Basic.Hooks (JSX, useContext)
import React.Basic.Hooks as React
import Web.HTML.History (URL(..))

type SlatProps
  = ( content :: Array JSX
    , interaction :: Maybe SlatInteraction
    , isList :: Boolean
    )

type SlatInteraction
  = { onClick :: Effect Unit
    , tabIndex :: Int
    , href :: Maybe URL
    }

slat :: LumiComponent SlatProps
slat = unsafePerformEffect do
  lumiComponent "Slat" defaults \props@{ className } -> React.do
    theme <- useContext lumiThemeContext
    let
      slatStyle =
        Styles.Slat.slat
          >>> styleModifier_ (E.css { appearance: E.none })
    pure case props.interaction of
      Nothing ->
        E.element R.div'
          { css: toCSS theme props slatStyle
          , children: props.content
          , className
          }
      Just interaction@{ href: Nothing } ->
        E.element R.button'
          { css: toCSS theme props slatStyle
          , children: props.content
          , onClick: capture_ interaction.onClick
          , tabIndex: interaction.tabIndex
          , className
          }
      Just interaction@{ href: Just href } ->
        E.element R.a'
          { css: toCSS theme props slatStyle
          , children: props.content
          , onClick: capture_ interaction.onClick
          , tabIndex: interaction.tabIndex
          , href: un URL href
          , className
          }
  where
  defaults =
    { content: []
    , interaction: Nothing
    , isList: true
    }

interactive :: SlatInteraction -> StyleModifier SlatProps
interactive interaction =
  Border.interactive
    >>> Box.interactive
    >>> propsModifier
        _
          { interaction = Just interaction
          }
