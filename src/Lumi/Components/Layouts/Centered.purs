module Lumi.Components.Layouts.Centered where

import React.Basic (JSX, element)
import React.Basic.DOM (unsafeCreateDOMComponent)

layout :: JSX -> JSX
layout content =
  lumiLayoutCentered
    { children: [content]
    }
  where
    lumiLayoutCentered = element (unsafeCreateDOMComponent "lumi-layout-centered")
