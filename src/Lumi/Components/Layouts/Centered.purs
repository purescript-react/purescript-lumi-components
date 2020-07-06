module Lumi.Components.Layouts.Centered where

import Prelude

import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Classic (JSX, element)
import React.Basic.DOM (unsafeCreateDOMComponent)

layout :: JSX -> JSX
layout = \content ->
  lumiLayoutCentered
    { children: [content]
    }
  where
    lumiLayoutCentered = element (unsafePerformEffect $ unsafeCreateDOMComponent "lumi-layout-centered")

layoutFullWidth :: JSX -> JSX
layoutFullWidth = \content ->
  lumiLayoutCentered
    { children: [content]
    }
  where
    lumiLayoutCentered = element (unsafePerformEffect $ unsafeCreateDOMComponent "lumi-layout-centered-full-width")
