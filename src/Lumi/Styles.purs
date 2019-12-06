module Lumi.Styles
  ( styleModifier
  , styleModifier_
  , toCSS
  , module Emotion
  ) where

import Prelude

import Lumi.Components (PropsModifier, LumiProps, propsModifier)
import Lumi.Styles.Theme (LumiTheme)
import React.Basic.Emotion hiding (element) as Emotion

-- | Lift a themed set of styles into a `PropsModifier` for composition with other modifiers.
-- |
-- | Note: A style modifier should generally leave the `props` type unconstrained and take
-- |   configuration as regular arguments instead. Adding constraints to `props` makes it
-- |   difficult to compose style and prop modifiers together across different components,
-- |   where the same field name could mean different things.
styleModifier :: forall props. (LumiTheme -> Emotion.Style) -> PropsModifier props
styleModifier f = propsModifier \props -> props { css = f <> props.css }

-- | Lift a static set of styles into a `PropsModifier` for composition with other modifiers.
-- |
-- | Note: A style modifier should generally leave the `props` type unconstrained and take
-- |   configuration as regular arguments instead. Adding constraints to `props` makes it
-- |   difficult to compose style and prop modifiers together across different components,
-- |   where the same field name could mean different things.
styleModifier_ :: forall props. Emotion.Style -> PropsModifier props
styleModifier_ = styleModifier <<< const

-- | Flatten a `PropsModifier` and extract the Emotion styles for use with `React.Basic.Emotion.element`.
-- | This function is mainly used inside component implementations where the `LumiComponent` boundary
-- | gives way to DOM components or other `ReactComponent`s.
toCSS ::
  forall props.
  LumiTheme ->
  LumiProps props ->
  PropsModifier props ->
  Emotion.Style
toCSS theme props m = (m identity props).css theme
