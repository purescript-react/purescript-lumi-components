module Lumi.Components2.Clip where

import Prelude

import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Aff (Aff, Error, Milliseconds(..), delay, makeAff, message, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Uncurried (EffectFn1, EffectFn3, mkEffectFn1, runEffectFn3)
import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components (LumiComponent, lumiComponent, ($$$))
import Lumi.Components.Spacing (Space(..), hspace)
import Lumi.Components2.Box (box)
import Lumi.Components2.Button (linkButton, loadingContent, onPress)
import Lumi.Styles (toCSS)
import Lumi.Styles as S
import Lumi.Styles.Box (FlexAlign(..), _align, _flex)
import Lumi.Styles.Box as Styles.Box
import Lumi.Styles.Clip as Styles.Clip
import Lumi.Styles.Theme (LumiTheme(..), useTheme)
import React.Basic.DOM as R
import React.Basic.Emotion as E
import React.Basic.Hooks (JSX, Ref, readRefMaybe, useRef)
import React.Basic.Hooks as React
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
      let
        copyButton =
          linkButton
            $ S.style_ (S.css { "&:disabled": S.nested $ S.css { color: S.color colors.black1 } })
            $ onPress do
                copy ref
                delay $ Milliseconds 5000.0
            $ loadingContent [ box $ _flex $ _align End $$$ [ R.text "Copied!" ] ]
            $$$ [ box $ _flex $ _align End $$$ [ R.text "Copy" ] ]
      pure
        $ E.element R.div'
            { className: props.className
            , css: theme # toCSS Styles.Clip.clip <> props.css
            , children:
              [ E.element R.div'
                  { className: ""
                  , css: theme # toCSS (Styles.Box.box <<< Styles.Box._justify Center)
                  , ref
                  , children: props.content
                  }
              , hspace S8
              , box
                  $ _align End
                  $$$ [ copyButton ]
              ]
            }
  where
  defaults = { content: [] }

copy :: Ref (Nullable Node) -> Aff Unit
copy nodeRef = do
  nodeM <- liftEffect do readRefMaybe nodeRef
  for_ nodeM \node ->
    makeAff \done -> do
      runEffectFn3 copyNodeContents
        ( done $ Right unit )
        ( mkEffectFn1 \e -> do
            Console.error $ message e
            done $ Right unit
        )
        node
      pure nonCanceler

foreign import copyNodeContents :: EffectFn3 (Effect Unit) (EffectFn1 Error Unit) Node Unit
