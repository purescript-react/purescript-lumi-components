module Lumi.Components2.Link where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Nullable (toNullable)
import Effect (Effect)
import Effect.Uncurried (runEffectFn1)
import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components (LumiComponent, lumiComponent)
import Lumi.Styles (toCSS)
import Lumi.Styles.Link as Styles.Link
import Lumi.Styles.Theme (useTheme)
import React.Basic (JSX)
import React.Basic.DOM (unsafeCreateDOMComponent)
import React.Basic.DOM.Events (altKey, button, ctrlKey, metaKey, preventDefault, shiftKey, stopPropagation)
import React.Basic.Emotion as E
import React.Basic.Events (handler, merge, syntheticEvent)
import React.Basic.Hooks as React
import Web.HTML.History (URL(..))

type LinkProps
  = ( href :: URL
    , navigate :: Maybe (Effect Unit)
    , tabIndex :: Int
    , target :: Maybe String
    , download :: Maybe String
    , content :: Array JSX
    , className :: String
    )

link :: LumiComponent LinkProps
link =
  unsafePerformEffect do
    let
      lumiAnchorElement = E.element (unsafeCreateDOMComponent "a")

      defaults =
        { className: ""
        , href: URL ""
        , navigate: Nothing
        , tabIndex: 0
        , target: Nothing
        , download: Nothing
        , content: []
        }
    lumiComponent "Link" defaults \props@{ className } -> React.do
      theme <- useTheme
      pure
        $ lumiAnchorElement
            { css: toCSS theme props Styles.Link.link
            , children: props.content
            , className
            , href: un URL props.href
            , onClick:
                handler (merge { button, metaKey, altKey, ctrlKey, shiftKey, syntheticEvent }) \{ button, metaKey, altKey, ctrlKey, shiftKey, syntheticEvent } -> do
                  case props.navigate, button, metaKey, altKey, ctrlKey, shiftKey of
                    Just n', Just 0, Just false, Just false, Just false, Just false ->
                      runEffectFn1
                        (handler (stopPropagation <<< preventDefault) $ const n')
                        syntheticEvent
                    _, _, _, _, _, _ ->
                      runEffectFn1
                        (handler stopPropagation mempty)
                        syntheticEvent
            , target: toNullable props.target
            , tabIndex: props.tabIndex
            , download: toNullable props.download
            }
