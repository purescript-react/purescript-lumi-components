module Lumi.Components.Badge where

import Prelude

import Color (cssStringHSLA)
import Effect.Unsafe (unsafePerformEffect)
import JSS (JSS, jss)
import Lumi.Components.Color (Color, colors)
import Lumi.Components.Text (lumiSubtextFontSize)
import React.Basic.Classic (JSX, element)
import React.Basic.DOM as R

type BadgeProps =
  { background :: Color
  , color :: Color
  , style :: R.CSS
  , text :: String
  }

badge_ :: String -> JSX
badge_ = badge <<< defaults { text = _ }

badge :: BadgeProps -> JSX
badge = \{ background, color, style, text } ->
  lumiBadgeElement
    { style: R.mergeStyles
        [ R.css
            { color: cssStringHSLA color
            , background: cssStringHSLA background
            }
        , style
        ]
    , children: R.text text
    }
  where
    lumiBadgeElement = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-badge")

defaults :: BadgeProps
defaults =
  { background: colors.black5
  , color: colors.secondary
  , style: R.css {}
  , text: ""
  }

styles :: JSS
styles = jss
  { "@global":
      { "lumi-badge":
          { alignSelf: "baseline"
          , display: "inline"
          , fontSize: lumiSubtextFontSize
          , lineHeight: "17px"
          , borderRadius: "9px"
          , marginTop: "2px"
          , padding: "0px 6px"
          }
      }
  }
