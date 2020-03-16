module Lumi.Styles.Responsive where

import Prelude

import Lumi.Components (PropsModifier)
import Lumi.Styles (Style, css, nested, styleModifier)

-- | Create a style modifier that, only in a desktop-sized screen, applies the
-- | styles accumulated in the modifier passed in as argument.
-- |
-- | NOTE: the value passed in as argument must be a props modifier that works
-- | on any set of props, a property that currently defines style modifiers.
onDesktop :: forall props. (forall a. PropsModifier a) -> PropsModifier props
onDesktop m =
  styleModifier \theme ->
    desktopQuery
      $ (m identity { className: "", css: mempty }).css
      $ theme

-- | Guard a style so that it's only applied in a desktop resolution.
desktopQuery :: Style -> Style
desktopQuery style =
  css
    { "@media (min-width: 860px)": nested style
    }
