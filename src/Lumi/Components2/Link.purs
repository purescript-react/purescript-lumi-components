module Lumi.Components2.Link where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Effect (Effect)
import Effect.Uncurried (runEffectFn1)
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object (fromHomogeneous)
import Lumi.Components (LumiComponent, lumiComponent, unsafeMaybeToNullableAttr)
import Lumi.Styles (toCSS)
import Lumi.Styles.Link as Styles.Link
import Lumi.Styles.Theme (useTheme)
import React.Basic.Classic (JSX)
import React.Basic.DOM as R
import React.Basic.DOM.Events (altKey, button, ctrlKey, metaKey, preventDefault, shiftKey, stopPropagation)
import React.Basic.Emotion as E
import React.Basic.Events (handler, merge, syntheticEvent)
import React.Basic.Hooks as React
import Web.HTML.History (URL(..))

type LinkProps
  = ( href :: URL
    , navigate :: Maybe (Effect Unit)
    , tabIndex :: Maybe Int
    , target :: Maybe String
    , rel :: Maybe String
    , download :: Maybe String
    , ariaLabel :: Maybe String
    , content :: Array JSX
    , className :: String
    )

link :: LumiComponent LinkProps
link =
  unsafePerformEffect do
    lumiComponent "Link" defaults \props@{ className } -> React.do
      theme <- useTheme
      pure
        $ E.element R.a'
            { _aria: unsafeMaybeToNullableAttr $ map (fromHomogeneous <<< { label: _ }) props.ariaLabel
            , css: theme # toCSS Styles.Link.link <> props.css
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
            , target: unsafeMaybeToNullableAttr props.target
            , rel: unsafeMaybeToNullableAttr props.rel
            , tabIndex: unsafeMaybeToNullableAttr props.tabIndex
            , download: unsafeMaybeToNullableAttr props.download
            }
  where
  defaults =
    { className: ""
    , href: URL ""
    , navigate: Nothing
    , tabIndex: Nothing
    , target: Nothing
    , rel: Nothing
    , download: Nothing
    , ariaLabel: Nothing
    , content: []
    }
