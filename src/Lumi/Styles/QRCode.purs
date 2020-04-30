module Lumi.Styles.QRCode where

import Prelude
import Lumi.Styles (StyleModifier, style_)
import Lumi.Styles.Box (box)
import React.Basic.Emotion (css, str)

qrcode :: StyleModifier
qrcode =
  box
    >>> style_
        ( css
            { label: str "qrcode"
            }
        )
