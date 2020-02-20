module Lumi.Components2.Slat
  ( SlatProps
  , SlatInteraction
  , SlatInteractionType(..)
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
import Lumi.Styles.Slat (_interactive, _interactiveBg, slat) as Styles.Slat.Hidden
import Lumi.Styles.Slat hiding (_interactive, _interactiveBg, slat) as Styles.Slat
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
    )

data SlatInteractionType
  = BorderInteraction
  | BackgroundInteraction

type SlatInteraction
  = { onClick :: Effect Unit
    , tabIndex :: Int
    , href :: Maybe URL
    , _type :: Maybe SlatInteractionType
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
        Just interaction@{ href: Nothing, _type: t } ->
          E.element R.button'
            { css:
                case t of
                  Just BorderInteraction -> toCSS theme props slatStyleInteractive
                  Just BackgroundInteraction -> toCSS theme props slatStyleInteractiveBg
                  _ -> toCSS theme props slatStyleInteractive
            , children: props.content
            , onClick: capture_ interaction.onClick
            , tabIndex: interaction.tabIndex
            , className
            }
        Just interaction@{ href: Just href,  _type: t } ->
          E.element R.a'
            { css:
                case t of
                  Just BorderInteraction -> toCSS theme props slatStyleInteractive
                  Just BackgroundInteraction -> toCSS theme props slatStyleInteractiveBg
                  _ -> toCSS theme props slatStyleInteractive
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
    }

  slatStyle =
    Styles.Slat.Hidden.slat
      >>> styleModifier_ (E.css { appearance: E.none })

  slatStyleInteractive =
    slatStyle
      >>> Styles.Slat.Hidden._interactive

  slatStyleInteractiveBg =
    slatStyle
      >>> Styles.Slat.Hidden._interactiveBg

_interactive :: SlatInteraction -> PropsModifier SlatProps
_interactive interaction =
  propsModifier
    _
      { interaction = Just interaction
      }
