module Lumi.Components2.Clip where

import Prelude
import Data.Foldable (traverse_)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components (LumiComponent, lumiComponent, lumiElement)
import Lumi.Components.Spacing (Space(..))
import Lumi.Components2.Button (_linkStyle, button)
import Lumi.Styles (styleModifier_, toCSS)
import Lumi.Styles.Clip as Styles.Clip
import Lumi.Styles.Theme (useTheme)
import React.Basic.DOM as R
import React.Basic.Emotion as E
import React.Basic.Events (handler_)
import React.Basic.Hooks (JSX, element, readRefMaybe, useRef, useState, (/\))
import React.Basic.Hooks as React
import Web.DOM (Element)
import Web.DOM.Element as Element

type ClipProps
  = ( content :: Array JSX
    )

clip :: LumiComponent ClipProps
clip =
  unsafePerformEffect do
    lumiComponent "Clip" defaults \props -> React.do
      theme <- useTheme
      ref <- useRef Nullable.null
      copied /\ setCopied <- useState false
      let
        setCopiedOn = setCopied \_ -> true

        setCopiedOff = setCopied \_ -> false

        onCopy = do
          el <- readRefMaybe ref
          traverse_ copyElementContents (Element.fromNode =<< el)
          setCopiedOn

        copyButton =
          lumiElement button
            $ _linkStyle
            $ styleModifier_ (E.css { marginLeft: E.prop S16, lineHeight: E.str "12px" })
            $ _
                { content = [ R.text if copied then "Copied!" else "Copy" ]
                , onPress = onCopy
                }
      pure
        $ E.element R.div'
            { className: props.className
            , css: toCSS theme props Styles.Clip.clip
            , onBlur: handler_ setCopiedOff
            , children:
              [ element R.div' { ref, children: props.content }
              , copyButton
              ]
            }
  where
  defaults = { content: [] }

foreign import copyElementContents :: Element -> Effect Unit
