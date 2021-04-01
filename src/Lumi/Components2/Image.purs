module Lumi.Components2.Image
  ( imageThumb
  , small
  , medium
  , large
  , extraLarge
  , _customSize
  , round
  ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid as Monoid
import Data.Nullable as Nullable
import Data.String as String
import Data.Tuple.Nested ((/\))
import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components (LumiComponent, PropsModifier, lumiComponent, ($$$))
import Lumi.Components.Loader (loader)
import Lumi.Components.Spacing (Space)
import Lumi.Components.Svg (userSvg)
import Lumi.Components2.Box as Box
import Lumi.Styles (style, toCSS)
import Lumi.Styles.Box (FlexAlign(..))
import Lumi.Styles.Box as Styles.Box
import Lumi.Styles.Image as Styles.Image
import Lumi.Styles.Slat hiding (_interactive,slat) as Styles.Slat
import Lumi.Styles.Theme (LumiTheme(..), useTheme)
import React.Basic.DOM as R
import React.Basic.Emotion as E
import React.Basic.Events (handler_)
import React.Basic.Hooks (JSX)
import React.Basic.Hooks as Hooks

type ImageProps
  = ( src :: String
    , placeholder :: Maybe JSX
    )

imageThumb :: LumiComponent ImageProps
imageThumb =
  unsafePerformEffect do
    lumiComponent "Image" { src: "", placeholder: Nothing } \props -> Hooks.do
      theme <- useTheme
      loaded /\ setLoaded <- Hooks.useState' false
      pure
        $ E.element R.div'
            { children:
                [ if String.null props.src
                    -- @TODO generate a placeholder svg
                    then fromMaybe userSvg props.placeholder
                    else
                      Box.column
                      $ Styles.Box._flex
                      $ Styles.Box._align Center
                      $ Styles.Box._justify Center
                      $$$
                        [ Monoid.guard (not loaded)
                            $ loader { style: R.css { width: "20px", height: "20px", borderWidth: "2px" }, testId: Nullable.toNullable Nothing }
                        , E.element R.img'
                            { src: props.src
                            , className: ""
                            , css: E.css { maxWidth: E.percent 100.0 }
                            , onLoad: handler_ $ setLoaded true
                            }
                        ]
                ]
            , className: props.className
            , css: theme # toCSS Styles.Image.imageThumb <> props.css
            }

-- NOTE do we want to use the Space data type? (adheres to our 8px grid system..)
-- or should we give consumers more flexibility?
_customSize :: Space -> PropsModifier ImageProps
_customSize size =
  style \(LumiTheme theme) ->
    E.css
      { width: E.prop size
      , height: E.prop size
      }

round :: LumiComponent ImageProps
round = imageThumb <<< Styles.Image._round

small :: LumiComponent ImageProps
small = imageThumb <<< Styles.Image._small

medium :: LumiComponent ImageProps
medium = imageThumb <<< Styles.Image._medium

large :: LumiComponent ImageProps
large = imageThumb <<< Styles.Image._large

extraLarge :: LumiComponent ImageProps
extraLarge = imageThumb <<< Styles.Image._extraLarge


