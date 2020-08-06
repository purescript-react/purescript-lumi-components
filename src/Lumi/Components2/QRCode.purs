module Lumi.Components2.QRCode
  ( useQRCode
  , UseQRCode
  , ErrorCorrectLevel(..)
  , errorCorrectLevelToString
  , qrcode_
  , generateSVGUrl
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components (LumiComponent, lumiComponent)
import Lumi.Styles (StyleModifier, style_, toCSS)
import Lumi.Styles.Box (box)
import Lumi.Styles.Theme (useTheme)
import React.Basic.DOM as R
import React.Basic.Emotion as E
import React.Basic.Hooks (type (/\), Hook, ReactComponent, Ref, UnsafeReference(..), UseEffect, UseMemo, UseRef, UseState, coerceHook, element, useEffect, useMemo, useRef, useState, (/\))
import React.Basic.Hooks as React
import Web.DOM (Node)
import Web.HTML.History (URL(..))

useQRCode :: ErrorCorrectLevel -> String -> Hook UseQRCode { qrcode :: LumiComponent (), url :: Maybe URL }
useQRCode level value =
  coerceHook React.do
    ref <- useRef Nullable.null
    qrcode <-
      useMemo (value /\ level) \_ ->
        unsafePerformEffect do
          lumiComponent "QRCode" {} \props -> React.do
            theme <- useTheme
            pure
              $ E.element R.div'
                  { children:
                      [ element qrcode_
                          { value
                          , level: errorCorrectLevelToString level
                          , renderAs: "svg"
                          , xmlns: "http://www.w3.org/2000/svg"
                          , size: Nullable.null
                          , bgColor: "rgba(255,255,255,0.0)"
                          , fgColor: "rgba(0,0,0,1.0)"
                          }
                      ]
                  , ref
                  , className: props.className
                  , css: theme # toCSS qrcodeStyle <> props.css
                  }
    url /\ setUrl <- useState Nothing
    useEffect (UnsafeReference qrcode) do
      svgUrl <- generateSVGUrl ref
      setUrl \_ -> Just $ URL svgUrl.url
      pure svgUrl.dispose
    pure { qrcode, url }

qrcodeStyle :: StyleModifier
qrcodeStyle = box <<< style_ (E.css { label: E.str "qrcode" })

newtype UseQRCode hooks
  = UseQRCode
  ( UseEffect
      ( UnsafeReference (LumiComponent ()) )
      ( UseState
          ( Maybe URL )
          ( UseMemo
              (String /\ ErrorCorrectLevel)
              (LumiComponent ())
              (UseRef (Nullable.Nullable Node) hooks)
          )
      )
  )

derive instance ntUseQRCode :: Newtype (UseQRCode hooks) _

data ErrorCorrectLevel
  = ECLLow
  | ECLMedium
  | ECLQuartile
  | ECLHigh

derive instance eqErrorCorrectLevel :: Eq ErrorCorrectLevel

errorCorrectLevelToString :: ErrorCorrectLevel -> String
errorCorrectLevelToString = case _ of
  ECLLow -> "L"
  ECLMedium -> "M"
  ECLQuartile -> "Q"
  ECLHigh -> "H"

foreign import qrcode_ ::
  ReactComponent
    { value :: String
    , level :: String
    , renderAs :: String
    , xmlns :: String
    , size :: Nullable.Nullable Int
    , bgColor :: String
    , fgColor :: String
    }

foreign import generateSVGUrl :: Ref (Nullable.Nullable Node) -> Effect { url :: String, dispose :: Effect Unit }
