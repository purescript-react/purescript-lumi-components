module Lumi.Components2.Examples.QRCode where

import Prelude
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components.Example (example)
import Lumi.Components.Input as Input
import Lumi.Components.Spacing (Space(..), vspace)
import Lumi.Components.Text (subsectionHeader_)
import Lumi.Components2.Box (box)
import Lumi.Components2.Link (link)
import Lumi.Components2.QRCode (ErrorCorrectLevel(..), useQRCode)
import Lumi.Styles as S
import Lumi.Styles.Border as Border
import Lumi.Styles.Box (FlexAlign(..))
import Lumi.Styles.Box as Box
import React.Basic.DOM.Events (capture, targetValue)
import React.Basic.Hooks (JSX, component, useState, (/\))
import React.Basic.Hooks as React
import Web.HTML.History (URL(..))

docs :: JSX
docs =
  unit # unsafePerformEffect do
        component "QRCodeExample" \_ -> React.do
          value /\ setValue <- useState "https://www.lumi.com"
          pure
            $ box
                _
                  { content =
                    [ Input.input
                        Input.text_
                          { value = value
                          , onChange = capture targetValue $ traverse_ (const >>> setValue)
                          }
                    , vspace S24
                    , example
                        $ qrcodeExample { value }
                    ]
                  }

qrcodeExample :: { value :: String } -> JSX
qrcodeExample =
  unsafePerformEffect do
    component "QRCode" \props -> React.do
      { qrcode, url } <- useQRCode ECLLow props.value
      pure
        $ box
        $ Box._align Center
        $ _
            { content =
              [ qrcode
                  $ Border.border
                  $ Border._round
                  $ S.style_
                      ( S.css
                          { padding: S.px 16
                          , width: S.px 140
                          }
                      )
                  $ identity
              , vspace S8
              , link
                  _
                    { href = fromMaybe (URL "") url
                    , download = Just "qrcode.svg"
                    , content =
                      [ subsectionHeader_ "Download SVG"
                      ]
                    }
              ]
            }
