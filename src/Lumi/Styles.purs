module Lumi.Styles
  ( module Lumi.Styles
  , module Emotion
  ) where

import Prelude

import Data.Foldable (fold)
import Lumi.Components (PropsModifier, propsModifier)
import Lumi.Styles.Theme (LumiTheme)
import React.Basic.Emotion hiding (element,style) as Emotion

type StyleModifier = forall props. PropsModifier props

-- | Lift a themed set of styles into a `StyleModifier` for composition with other modifiers.
style :: (LumiTheme -> Emotion.Style) -> StyleModifier
style f = propsModifier \props -> props { css = props.css <> f }

-- | Lift a static set of styles into a `StyleModifier` for composition with other modifiers.
style_ :: Emotion.Style -> StyleModifier
style_ = style <<< const

-- | Lift an array of themed styles into a `StyleModifier` for composition with other modifiers.
styles :: Array (LumiTheme -> Emotion.Style) -> StyleModifier
styles = style <<< fold

-- | Lift an array of static styles into a `StyleModifier` for composition with other modifiers.
styles_ :: Array Emotion.Style -> StyleModifier
styles_ = style_ <<< fold

-- | Flatten a `PropsModifier` and extract the Emotion styles for use with `React.Basic.Emotion.element`.
-- | This function is mainly used inside component implementations where the `LumiComponent` boundary
-- | gives way to DOM components or other `ReactComponent`s.
toCSS ::
  PropsModifier () ->
  LumiTheme ->
  Emotion.Style
toCSS m = (m identity { className: "", css: mempty }).css
