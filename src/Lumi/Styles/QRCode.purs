module Lumi.Styles.QRCode where

import Prelude
import Lumi.Styles (StyleModifier, styleModifier_)
import Lumi.Styles.Box (box)
import React.Basic.Emotion (css, str)

qrcode :: StyleModifier
qrcode =
  box
    >>> styleModifier_
        ( css
            { label: str "qrcode"
            }
        )
