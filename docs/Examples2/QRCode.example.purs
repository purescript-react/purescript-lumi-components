module Lumi.Components2.Examples.QRCode where

import Prelude
import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components (lumiElement)
import Lumi.Components.Example (example)
import Lumi.Components.Spacing (Space(..), vspace)
import Lumi.Components.Text (p_)
import Lumi.Components2.Box (box)
import Lumi.Components2.Button (button)
import Lumi.Components2.Button as Button
import Lumi.Components2.QRCode (ErrorCorrectLevel(..), useQRCode)
import Lumi.Styles as S
import Lumi.Styles.Border as Border
import React.Basic.DOM as R
import React.Basic.Hooks (JSX, ReactComponent, component, element)
import React.Basic.Hooks as React

docs :: JSX
docs =
  lumiElement box
    _
      { content =
        [ p_ "A QR Code pointing to \"https://www.lumi.com\""
        , vspace S24
        , example
            $ element qrcodeExample { value: "https://www.lumi.com" }
        ]
      }

qrcodeExample :: ReactComponent { value :: String }
qrcodeExample =
  unsafePerformEffect do
    component "QRCode" \props -> React.do
      { qrcode, saveOnClick } <- useQRCode props.value L
      pure
        $ lumiElement box
            _
              { content =
                [ lumiElement box
                    <<< Border.border
                    >>> Border._round
                    >>> S.styleModifier_ (S.css { padding: S.int 16 })
                    $ _ { content = [ qrcode ] }
                , vspace S8
                , lumiElement button
                    <<< Button._secondary
                    $ _
                        { content = [ R.text "Download SVG" ]
                        , onPress = saveOnClick "qrcode.svg"
                        }
                ]
              }
