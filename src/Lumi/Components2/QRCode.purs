module Lumi.Components2.QRCode where

import Prelude
import Data.Newtype (class Newtype)
import Data.Nullable as Nullable
import Effect (Effect)
import Lumi.Styles (toCSS)
import Lumi.Styles.QRCode as Styles.QRCode
import Lumi.Styles.Theme (LumiTheme, useTheme)
import React.Basic.DOM as R
import React.Basic.Emotion as E
import React.Basic.Hooks (Hook, JSX, ReactComponent, Ref, UseContext, UseRef, coerceHook, element, useRef)
import React.Basic.Hooks as React
import Web.DOM (Node)

newtype UseQRCode hooks
  = UseQRCode (UseRef (Nullable.Nullable Node) (UseContext LumiTheme hooks))

derive instance ntUseQRCode :: Newtype (UseQRCode hooks) _

data ErrorCorrectLevel
  = L
  | M
  | Q
  | H

errorCorrectLevelToString :: ErrorCorrectLevel -> String
errorCorrectLevelToString = case _ of
  L -> "L"
  M -> "M"
  Q -> "Q"
  H -> "H"

useQRCode :: String -> ErrorCorrectLevel -> Hook UseQRCode { qrcode :: JSX, saveOnClick :: String -> Effect Unit }
useQRCode value level =
  coerceHook React.do
    theme <- useTheme
    ref <- useRef Nullable.null
    pure
      { qrcode:
          E.element R.div'
            { children:
                [ element qrcode_
                    { value
                    , level: errorCorrectLevelToString level
                    , renderAs: "svg"
                    , xmlns: "http://www.w3.org/2000/svg"
                    , size: 126
                    }
                ]
            , ref
            , className: ""
            , css: toCSS theme { className: "", css: mempty } Styles.QRCode.qrcode
            }
      , saveOnClick: saveOnClick ref
      }

foreign import qrcode_ ::
  ReactComponent
    { value :: String
    , level :: String
    , renderAs :: String
    , xmlns :: String
    , size :: Int
    }

foreign import saveOnClick :: Ref (Nullable.Nullable Node) -> String -> Effect Unit
