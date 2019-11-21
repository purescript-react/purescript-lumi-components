module Lumi.Styles
  ( StyleModifier
  , styleModifier
  , styleModifier_
  , toCSS
  , module Emotion
  ) where

import Prelude

import Lumi.Components (PropsModifier, LumiProps, propsModifier)
import Lumi.Styles.Theme (LumiTheme)
import React.Basic.Emotion hiding (element) as Emotion

type StyleModifier props = PropsModifier props

styleModifier :: forall props. (LumiTheme -> Emotion.Style) -> PropsModifier props
styleModifier f = propsModifier \props -> props { css = props.css <> f }

styleModifier_ :: forall props. Emotion.Style -> PropsModifier props
styleModifier_ = styleModifier <<< const

toCSS ::
  forall props.
  LumiTheme ->
  LumiProps props ->
  PropsModifier props ->
  Emotion.Style
toCSS theme props m = (m identity props).css theme
