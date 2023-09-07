module Lumi.Components.Link where

import Prelude

import Color (cssStringHSLA)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Nullable (toNullable)
import Data.String (Pattern(..), contains)
import Effect (Effect)
import Effect.Uncurried (runEffectFn1)
import Effect.Unsafe (unsafePerformEffect)
import JSS (JSS, jss)
import Lumi.Components.Color (colors)
import React.Basic.Classic (Component, JSX, createComponent, element, empty, makeStateless)
import React.Basic.DOM (CSS, css, unsafeCreateDOMComponent)
import React.Basic.DOM as R
import React.Basic.DOM.Events (altKey, button, ctrlKey, metaKey, preventDefault, shiftKey, stopPropagation)
import React.Basic.Events (handler, merge, syntheticEvent)
import Web.HTML.History (URL(..))


type LinkProps =
  { className :: Maybe String
  , href :: URL
  , navigate :: Maybe (Effect Unit)
  , style :: CSS
  , target :: Maybe String
  , testId :: Maybe String
  , text :: JSX
  }

component :: Component LinkProps
component = createComponent "Link"

link :: LinkProps -> JSX
link = makeStateless component render
  where
    lumiAnchorElement = element (unsafePerformEffect $ unsafeCreateDOMComponent "a")

    render { className, href, navigate, style, target, testId, text } =
      lumiAnchorElement
        { children: text
        , className: "lumi" <> foldMap (" " <> _) className
        , "data-testid": toNullable testId
        , href: un URL href
        , onClick: handler (merge { button, metaKey, altKey, ctrlKey, shiftKey, syntheticEvent })
            \{ button, metaKey, altKey, ctrlKey, shiftKey, syntheticEvent } -> do
              case navigate, button, metaKey, altKey, ctrlKey, shiftKey of
                Just n', Just 0, Just false, Just false, Just false, Just false ->
                  runEffectFn1
                    (handler (stopPropagation <<< preventDefault) $ const n')
                    syntheticEvent
                _      , _     , _         , _         , _         , _          ->
                  runEffectFn1
                    (handler stopPropagation mempty)
                    syntheticEvent
        , style
        , target: toNullable target
        }

defaults :: LinkProps
defaults =
  { className: Nothing
  , href: URL ""
  , navigate: Nothing
  , style: css {}
  , target: Nothing
  , testId: Nothing
  , text: empty
  }

styles :: JSS
styles = jss
  { "@global":
      { "a.lumi":
          { color: cssStringHSLA colors.primary
          , textDecoration: "none"
          , "&:visited":
              { color: cssStringHSLA colors.primary
              , textDecoration: "none"
              }
          , "&:hover":
              { cursor: "pointer"
              , textDecoration: "underline"
              }
          , "&:focus, &:active": { outline: "0" }
          }
      }
  }

linkOut
  :: { href :: URL
     , label :: String
     }
  -> JSX
linkOut { label, href: (URL href) } =
    link defaults
      { className = Just "action"
      , href = href'
      , text = R.text label
      , target = Just "_blank"
      }
  where href' = URL if contains (Pattern "http") href then href else "https://" <> href