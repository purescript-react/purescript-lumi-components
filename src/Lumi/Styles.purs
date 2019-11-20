module Lumi.Styles
  ( styleModifier
  , styleModifier_
  , toCSS
  ) where

import Prelude

import Lumi.Components (PropsModifier, LumiProps)
import Lumi.Styles.Theme (LumiTheme)
import React.Basic.Emotion as Emotion

styleModifier :: forall props. (LumiTheme -> Emotion.Style) -> PropsModifier props
styleModifier f m = m >>> \p -> p { css = p.css <> f }

styleModifier_ :: forall props. Emotion.Style -> PropsModifier props
styleModifier_ = styleModifier <<< const

toCSS :: forall props. LumiTheme -> LumiProps props -> PropsModifier props -> Emotion.Style
toCSS theme p m = (m identity p).css theme
