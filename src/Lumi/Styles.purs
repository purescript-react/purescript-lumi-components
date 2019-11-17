module Lumi.Styles
  ( StyleModifier
  , styleModifier
  , styleModifier_
  , toCSS
  , module Components
  ) where

import Prelude hiding (bind, discard)
import Lumi.Components (PropsModifier)
import Lumi.Components (bind, discard) as Components
import Lumi.Styles.Theme (LumiTheme)
import React.Basic.Emotion as Emotion

type StyleModifier
  = forall props. PropsModifier props

styleModifier :: (LumiTheme -> Emotion.Style) -> StyleModifier
styleModifier f m = m >>> \p -> p { css = p.css <> f }

styleModifier_ :: Emotion.Style -> StyleModifier
styleModifier_ = styleModifier <<< const

toCSS :: LumiTheme -> StyleModifier -> Emotion.Style
toCSS theme m = (m identity { className: "", css: mempty }).css theme
