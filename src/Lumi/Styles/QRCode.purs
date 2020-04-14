module Lumi.Styles.QRCode where

import Prelude
import Lumi.Components (PropsModifier)
import Lumi.Styles (styleModifier_)
import Lumi.Styles.Box (box)
import React.Basic.Emotion (css, str)

qrcode :: forall props. PropsModifier props
qrcode =
  box
    >>> styleModifier_
        ( css
            { label: str "qrcode"
            }
        )
