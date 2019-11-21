module Lumi.Styles
  ( StyleModifier
  , styleModifier
  , styleModifier_
  , propsModifier
  , propsModifier_
  , withStyle
  , toCSS
  , module Emotion
  ) where

import Prelude
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Lumi.Components (LumiModifier, LumiProps)
import Lumi.Styles.Theme (LumiTheme)
import React.Basic.Emotion hiding (element) as Emotion
import Record.Unsafe.Union (unsafeUnion)

type StyleModifier props
  = ((LumiTheme /\ LumiProps props) -> (LumiTheme /\ LumiProps props)) ->
    ((LumiTheme /\ LumiProps props) -> (LumiTheme /\ LumiProps props))

styleModifier :: forall props. (LumiTheme -> Emotion.Style) -> StyleModifier props
styleModifier f = propsModifier \t p -> p { css = p.css <> f t }

styleModifier_ :: forall props. Emotion.Style -> StyleModifier props
styleModifier_ s = styleModifier \_ -> s

propsModifier :: forall props. (LumiTheme -> LumiModifier props) -> StyleModifier props
propsModifier f m = m >>> \(t /\ p) -> t /\ f t p

propsModifier_ :: forall props. LumiModifier props -> StyleModifier props
propsModifier_ f = propsModifier \_ -> f

withStyle :: forall props. LumiTheme -> StyleModifier props -> LumiModifier props
withStyle t m p = snd (m identity (t /\ p))

toCSS :: forall props. LumiTheme -> { className :: String | props } -> StyleModifier props -> Emotion.Style
toCSS t p m = (withStyle t m (unsafeUnion { css: mempty } p)).css
