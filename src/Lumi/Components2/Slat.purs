module Lumi.Components2.Slat where

import Prelude
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (un)
import Effect (Effect)
import Lumi.Components (LumiComponent, lumiComponent)
import Lumi.Components2.Box (mkBox)
import Lumi.Styles.Border (Border(..))
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
    , border :: Border
    , isList :: Boolean
    , onInteraction ::
      Maybe
        { onClick :: Effect Unit
        , tabIndex :: Int
        , href :: Maybe URL
        }
    )

mkSlat ::
  ReactContext LumiTheme ->
  Effect (LumiComponent SlatProps)
mkSlat t = do
  box <- mkBox t
  lumiComponent
    "Slat"
    { content: []
    , className: ""
    , border: BorderRound
    , isList: true
    , onInteraction: Nothing
    }
    ( \props -> React.do
        theme <- useContext t
        let
          slatStyle =
            E.merge
              [ Styles.Slat.slat
                theme
                { border: props.border
                , isInteractive: isJust props.onInteraction
                , isList: props.isList
                }
              , appearanceNone
              ]
        pure case props.onInteraction of
          Nothing ->
            E.element slatStyle R.div'
              { children: props.content
              , className: props.className
              }
          Just onInteraction -> case onInteraction.href of
            Nothing ->
              E.element slatStyle R.button'
                { children: props.content
                , className: props.className
                , onClick: capture_ onInteraction.onClick
                , tabIndex: onInteraction.tabIndex
                }
            Just href ->
              E.element slatStyle R.a'
                { children: props.content
                , className: props.className
                , onClick: capture_ onInteraction.onClick
                , tabIndex: onInteraction.tabIndex
                , href: un URL href
                }
    )

appearanceNone :: E.Style
appearanceNone = E.css { appearance: E.none }
