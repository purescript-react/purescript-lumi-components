module Lumi.Styles where

import Prelude hiding (bind, discard)

import Lumi.Components (PropsModifier)
import Lumi.Styles.Theme (LumiTheme)
import React.Basic.Emotion as Emotion

type StyleModifier = forall props. PropsModifier props

styleModifier :: (LumiTheme -> Emotion.Style) -> StyleModifier
styleModifier f m = m >>> \p -> p { style = p.style <> f }

styleModifier_ :: Emotion.Style -> StyleModifier
styleModifier_ = styleModifier <<< const

toCSS :: LumiTheme -> StyleModifier -> Emotion.Style
toCSS theme m = (m identity { className: "", style: mempty }).style theme
