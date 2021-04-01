module Lumi.Components2.Image
  ( image
  , small
  , medium
  , large
  , extraLarge
  , _customSize
  , round
  ) where

import Prelude

import Data.Map (size)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components (LumiComponent, PropsModifier, lumiComponent, propsModifier)
import Lumi.Components.Size (Size(..), medium)
import Lumi.Components.Spacing (Space(..))
import Lumi.Styles (style, style_, toCSS)
import Lumi.Styles.Image as Styles.Image
import Lumi.Styles.Slat (_interactive, slat) as Styles.Slat.Hidden
import Lumi.Styles.Slat hiding (_interactive,slat) as Styles.Slat
import Lumi.Styles.Theme (LumiTheme(..), useTheme)
import React.Basic.DOM (s)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)
import React.Basic.Emotion as E
import React.Basic.Hooks (JSX)
import React.Basic.Hooks as React
import Web.HTML.History (URL(..))

type ImageProps
  = ( src :: String
    )

-- @TODO need a default value if no size is provided...
-- @TODO need to determine round ("avatar") vs square ("product")
image :: LumiComponent ImageProps
image =
  unsafePerformEffect do
    lumiComponent "Image" { src: "" } \props -> React.do
      theme <- useTheme
      pure
        $ E.element R.div'
            { children:
                [ E.element R.img'
                    -- @TODO case on "" string and show placeholder img
                    -- should this be a maybe? (likely)
                    { src: props.src
                    , className: props.className
                    , css: E.css { maxWidth: E.percent 100.0 }
                    }
                ]
            , className: props.className
            , css: theme # toCSS Styles.Image.image <> props.css
            }

_customSize :: Space -> PropsModifier ImageProps
_customSize size =
  style \(LumiTheme theme) ->
    E.css
      { width: E.prop size
      , height: E.prop size
      }

round :: LumiComponent ImageProps
round = image <<< Styles.Image._round

small :: LumiComponent ImageProps
small = image <<< Styles.Image._small

medium :: LumiComponent ImageProps
medium = image <<< Styles.Image._medium

large :: LumiComponent ImageProps
large = image <<< Styles.Image._large

extraLarge :: LumiComponent ImageProps
extraLarge = image <<< Styles.Image._extraLarge


