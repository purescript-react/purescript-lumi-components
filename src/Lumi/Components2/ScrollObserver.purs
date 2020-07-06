module Lumi.Components2.ScrollObserver where

import Prelude
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components (LumiComponent, lumiComponentFromHook)
import React.Basic.Classic (JSX)
import React.Basic.Hooks (Hook, UnsafeReference(..), UseEffect, UseState, coerceHook, useEffect, useState, (/\))
import React.Basic.Hooks as React
import Web.DOM (Element, Node)
import Web.DOM.Element (scrollLeft, scrollTop)
import Web.DOM.Element as Element
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (EventListener, EventTarget, eventListener)

newtype UseScrollObserver hooks
  = UseScrollObserver (UseEffect (UnsafeReference (Nullable Node)) (UseState Boolean (UseState Boolean hooks)))

derive instance ntUseScrollObserver :: Newtype (UseScrollObserver hooks) _

useScrollObserver :: Nullable Node -> Hook UseScrollObserver { hasScrolledX :: Boolean, hasScrolledY :: Boolean }
useScrollObserver root =
  coerceHook React.do
    hasScrolledY /\ setHasScrolledY <- useState false
    hasScrolledX /\ setHasScrolledX <- useState false
    useEffect (UnsafeReference root) do
      scrollParent <- getScrollParent root
      let
        onScroll = do
          top <- scrollTop scrollParent
          left <- scrollLeft scrollParent
          setHasScrolledY \_ -> top > 0.0
          setHasScrolledX \_ -> left > 0.0
      onScrollListener <- eventListener \_ -> onScroll
      onScroll
      Element.toEventTarget scrollParent # addPassiveEventListener (EventType "scroll") onScrollListener false
      pure do
        Element.toEventTarget scrollParent # removePassiveEventListener (EventType "scroll") onScrollListener false
    pure { hasScrolledY, hasScrolledX }

type ScrollObserverProps
  = ( node :: Nullable Node
    , render :: { hasScrolledX :: Boolean, hasScrolledY :: Boolean } -> JSX
    )

scrollObserver :: LumiComponent ScrollObserverProps
scrollObserver =
  unsafePerformEffect do
    lumiComponentFromHook "ScrollObserver" defaults (useScrollObserver <<< _.node)
  where
  defaults = { node: Nullable.null, render: \_ -> mempty }

foreign import getScrollParent :: Nullable Node -> Effect Element

-- | Adds a listener to an event target. The boolean argument indicates whether
-- | the listener should be added for the "capture" phase.
foreign import addPassiveEventListener ::
  EventType ->
  EventListener ->
  Boolean ->
  EventTarget ->
  Effect Unit

-- | Removes a listener to an event target. The boolean argument indicates
-- | whether the listener should be removed for the "capture" phase.
foreign import removePassiveEventListener ::
  EventType ->
  EventListener ->
  Boolean ->
  EventTarget ->
  Effect Unit
