module Lumi.Components2.Slat
  ( SlatProps
  , SlatInteraction
  , slat
  , _interactive
  , module Styles.Slat
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components (LumiComponent, PropsModifier, lumiComponent, propsModifier)
import Lumi.Styles (styleModifier_, toCSS)
import Lumi.Styles.Slat (_interactive, slat) as Styles.Slat.Hidden
import Lumi.Styles.Slat hiding (_interactive, slat) as Styles.Slat
import Lumi.Styles.Theme (useTheme)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)
import React.Basic.Emotion as E
import React.Basic.Hooks (JSX)
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
slat =
  unsafePerformEffect do
    lumiComponent "Slat" defaults \props@{ className } -> React.do
      theme <- useTheme
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

  slatStyle =
    Styles.Slat.Hidden.slat
      >>> styleModifier_ (E.css { appearance: E.none })

_interactive :: SlatInteraction -> PropsModifier SlatProps
_interactive interaction =
  Styles.Slat.Hidden._interactive
    >>> propsModifier
        _
          { interaction = Just interaction
          }
