module Lumi.Styles.Responsive where

import Prelude

import Lumi.Styles (Style, StyleModifier, css, nested, style, toCSS)

-- | Create a style modifier that, only in a desktop-sized screen, applies the
-- | styles accumulated in the modifier passed in as argument.
-- |
-- | NOTE: the value passed in as argument must be a props modifier that touches
-- | no component-specific props, a property that currently defines style
-- | modifiers.
onDesktop :: StyleModifier -> StyleModifier
onDesktop m =
  style \theme ->
    desktopQuery
      $ toCSS m
      $ theme

-- | Guard a style so that it's only applied in a desktop resolution.
desktopQuery :: Style -> Style
desktopQuery style =
  css
    { "@media (min-width: 860px)": nested style
    }

-- | Create a style modifier that, only in a mobile-sized screen, applies the
-- | styles accumulated in the modifier passed in as argument.
-- |
-- | NOTE: the value passed in as argument must be a props modifier that touches
-- | no component-specific props, a property that currently defines style
-- | modifiers.
onMobile :: StyleModifier -> StyleModifier
onMobile m =
  style \theme ->
    mobileQuery
      $ toCSS m
      $ theme

-- | Guard a style so that it's only applied in a mobile screen resolution.
mobileQuery :: Style -> Style
mobileQuery style =
  css
    { "@media (max-width: 859px)": nested style
    }
