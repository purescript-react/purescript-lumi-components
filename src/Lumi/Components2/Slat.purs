module Lumi.Components2.Slat where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Effect (Effect)
import Lumi.Components (LumiComponent, lumiComponent)
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
    , interaction ::
      Maybe
        { onClick :: Effect Unit
        , tabIndex :: Int
        , href :: Maybe URL
        }
    )

mkSlat ::
  ReactContext LumiTheme ->
  Effect (LumiComponent SlatProps)
mkSlat t =
  lumiComponent "Slat" defaults \props@{ className } -> React.do
    theme <- useContext t
    let
      slatStyle =
        E.merge
          [ Styles.Slat.slat theme
          , appearanceNone
          ]
    pure
      case props.interaction of
        Nothing ->
          E.element R.div'
            { css: slatStyle
            , children: props.content
            , className
            }
        Just interaction@{ href: Nothing } ->
          E.element R.button'
            { css: slatStyle
            , children: props.content
            , onClick: capture_ interaction.onClick
            , tabIndex: interaction.tabIndex
            , className
            }
        Just interaction@{ href: Just href } ->
          E.element R.a'
            { css: slatStyle
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

    appearanceNone :: E.Style
    appearanceNone = E.css { appearance: E.none }
