module Lumi.Styles.Responsive where

import Prelude

import Lumi.Components (PropsModifier)
import Lumi.Styles (Style, css, nested, styleModifier)

desktopQuery :: Style -> Style
desktopQuery style =
  css
    { "@media (min-width: 860px)": nested style
    }

onDesktop :: forall props. PropsModifier () -> PropsModifier props
onDesktop m =
  styleModifier \theme ->
    desktopQuery
      $ (m identity mempty).css
      $ theme
