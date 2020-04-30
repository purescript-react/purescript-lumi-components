module Lumi.Components2.Slat
  ( SlatProps
  , SlatInteraction
  , slat
  , _interactive
  , _interactiveBackground
  , module Styles.Slat
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components (LumiComponent, PropsModifier, lumiComponent, propsModifier)
import Lumi.Styles (style, style_, toCSS)
import Lumi.Styles.Slat (_interactive, slat) as Styles.Slat.Hidden
import Lumi.Styles.Slat hiding (_interactive,slat) as Styles.Slat
import Lumi.Styles.Theme (LumiTheme(..), useTheme)
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
            { css: theme # toCSS slatStyle <> props.css
            , children: props.content
            , className
            }
        Just interaction@{ href: Nothing } ->
          E.element R.button'
            { css: theme # toCSS slatStyleInteractive <> props.css
            , children: props.content
            , onClick: capture_ interaction.onClick
            , tabIndex: interaction.tabIndex
            , className
            }
        Just interaction@{ href: Just href } ->
          E.element R.a'
            { css: theme # toCSS slatStyleInteractive <> props.css
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
      <<< style_ (E.css { appearance: E.none })

  slatStyleInteractive =
    slatStyle
      <<< Styles.Slat.Hidden._interactive

_interactive :: SlatInteraction -> PropsModifier SlatProps
_interactive interaction =
  propsModifier
    _
      { interaction = Just interaction
      }

_interactiveBackground :: SlatInteraction -> PropsModifier SlatProps
_interactiveBackground interaction =
  propsModifier
    _
      { interaction = Just interaction
      }
    <<< style \(LumiTheme theme) ->
        E.css
          { "&:hover":
            E.nested
              $ E.css
                  { backgroundColor: E.color theme.colors.primary4
                  , borderColor: E.color theme.colors.black4
                  }
          }
