module Lumi.Styles.Box where

import React.Basic.Emotion (Style, class IsStyleProperty, css, merge, prop, str)

box :: Style
box =
  css
    { display: str "flex"
    , flexDirection: str "column"
    , boxSizing: str "border-box"
    , minHeight: str "0"
    , flex: str "0 0 auto"
    }

column :: Style
column = box

row :: Style
row =
  merge
    [ box
    , css { flexDirection: str "row" }
    ]

wrap :: Style
wrap =
  css
    { flexWrap: str "wrap"
    }

data FlexAlign
  = Start
  | End
  | Center
  | Stretch
  | Baseline
  | Unset

instance isStylePropertyFlexAlign :: IsStyleProperty FlexAlign where
  prop a =
    str case a of
      Start -> "flex-start"
      End -> "flex-end"
      Center -> "center"
      Stretch -> "stretch"
      Baseline -> "baseline"
      Unset -> "unset"

justify :: FlexAlign -> Style
justify a = css { justifyContent: prop a }

align :: FlexAlign -> Style
align a = css { alignItems: prop a }

alignSelf :: FlexAlign -> Style
alignSelf a = css { alignSelf: prop a }

interactive :: Style
interactive =
  css
    { touchAction: str "manipulation"
    , userSelect: str "none"
    , cursor: str "pointer"
    }
