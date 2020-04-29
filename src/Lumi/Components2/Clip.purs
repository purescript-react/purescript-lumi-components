module Lumi.Components2.Clip where

import Prelude

import Data.Foldable (for_)
import Data.Monoid (guard)
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Aff (Error, Milliseconds(..), delay, message)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Uncurried (EffectFn1, EffectFn3, mkEffectFn1, runEffectFn3)
import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components (LumiComponent, lumiComponent)
import Lumi.Components.Spacing (Space(..))
import Lumi.Components2.Box (box)
import Lumi.Components2.Button (_linkStyle, button)
import Lumi.Styles (styleModifier_, toCSS)
import Lumi.Styles.Box (FlexAlign(..), _justify)
import Lumi.Styles.Box as Styles.Box
import Lumi.Styles.Clip as Styles.Clip
import Lumi.Styles.Theme (LumiTheme(..), useTheme)
import React.Basic.DOM as R
import React.Basic.Emotion as E
import React.Basic.Hooks (Hook, JSX, Ref, UseState, coerceHook, readRefMaybe, useRef, useState, (/\), type (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (UseAff, useAff)
import React.Basic.Hooks.ResetToken (ResetToken, UseResetToken, useResetToken)
import Web.DOM (Node)

type ClipProps
  = ( content :: Array JSX
    )

clip :: LumiComponent ClipProps
clip =
  unsafePerformEffect do
    lumiComponent "Clip" defaults \props -> React.do
      theme@(LumiTheme { colors }) <- useTheme
      ref <- useRef Nullable.null
      { copied, copy } <- useClip ref
      let
        copyButton =
          button
            $ _linkStyle
            $ styleModifier_
                ( E.merge
                    [ E.css
                        { marginLeft: E.prop S16
                        , lineHeight: E.str "1.2"
                        }
                    , guard copied do
                        E.css
                          { color: E.color colors.black1
                          , "&:hover": E.nested $ E.css { textDecoration: E.none }
                          }
                    ]
                )
            $ _
                { content = [ R.text if copied then "Copied!" else "Copy" ]
                , onPress = copy
                }
      pure
        $ E.element R.div'
            { className: props.className
            , css: toCSS theme props Styles.Clip.clip
            , children:
              [ E.element R.div'
                  { className: ""
                  , css:
                    toCSS theme props
                      $ Styles.Box.box
                      >>> Styles.Box._justify Center
                  , ref
                  , children: props.content
                  }
              , box
                  $ _justify Center
                  $ _ { content = [ copyButton ] }
              ]
            }
  where
  defaults = { content: [] }

newtype UseClip hooks
  = UseClip (UseAff (ResetToken /\ Boolean) Unit (UseState Boolean (UseResetToken hooks)))

derive instance ntUseClip :: Newtype (UseClip hooks) _

useClip :: Ref (Nullable Node) -> Hook UseClip { copied :: Boolean, copy :: Effect Unit }
useClip nodeRef =
  coerceHook React.do
    token /\ resetToken <- useResetToken
    copied /\ setCopied <- useState false
    let
      copy = do
        node <- readRefMaybe nodeRef
        for_ node
          $ runEffectFn3 copyNodeContents
              ( do
                  setCopied \_ -> true
                  resetToken
              )
              (mkEffectFn1 $ Console.error <<< message)
    useAff (token /\ copied) do
      when copied do
        delay $ Milliseconds 5000.0
        liftEffect $ setCopied \_ -> false
    pure { copied, copy }

foreign import copyNodeContents :: EffectFn3 (Effect Unit) (EffectFn1 Error Unit) Node Unit
