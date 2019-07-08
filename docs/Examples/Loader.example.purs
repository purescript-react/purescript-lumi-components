module Lumi.Components.Examples.Loader where

import Prelude

import Color (cssStringHSLA, desaturate, lighten)
import Data.Nullable (null)
import Lumi.Components.Color (Color(..), colorNames, colors)
import Lumi.Components.Column (column, column_)
import Lumi.Components.Loader (loader)
import Lumi.Components.Example (example)
import React.Basic (JSX)
import React.Basic.DOM as R

docs :: JSX
docs =
  column_
    [ example $
        column {
          children:
            [ loader
                { style: R.css {}
                , testId: null
                , color: colorNames.white
                , bgColor: colorNames.primary
                }
            ]
          , style: R.css { backgroundColor: cssStringHSLA $ lighten 0.4137 $ desaturate 0.1972 $ colors.primary }
        }
    , example $
        column {
          children:
            [ loader
                { style: R.css {}
                , testId: null
                , color: colorNames.black
                , bgColor: colorNames.white
                }
            ]
          , style: R.css { backgroundColor: cssStringHSLA colors.white }
        }
    , example $
        column {
          children:
            [ loader
                { style: R.css {}
                , testId: null
                , color: colorNames.black1
                , bgColor: colorNames.white
                }
            ]
          , style: R.css { backgroundColor: cssStringHSLA colors.white }
        }
    , example $
        column {
          children:
            [ loader
                { style: R.css {}
                , testId: null
                , color: colorNames.black2
                , bgColor: colorNames.white
                }
            ]
          , style: R.css { backgroundColor: cssStringHSLA colors.white }
        }
    , example $
        column {
          children:
            [ loader
                { style: R.css {}
                , testId: null
                , color: colorNames.black3
                , bgColor: colorNames.white
                }
            ]
          , style: R.css { backgroundColor: cssStringHSLA colors.white }
        }
    , example $
        column {
          children:
            [ loader
                { style: R.css {}
                , testId: null
                , color: colorNames.black4
                , bgColor: colorNames.white
                }
            ]
          , style: R.css { backgroundColor: cssStringHSLA colors.white }
        }
    , example $
        column {
          children:
            [ loader
                { style: R.css {}
                , testId: null
                , color: colorNames.black5
                , bgColor: colorNames.white
                }
            ]
          , style: R.css { backgroundColor: cssStringHSLA colors.white }
        }
    , example $
        column {
          children:
            [ loader
                { style: R.css {}
                , testId: null
                , color: colorNames.black6
                , bgColor: colorNames.white
                }
            ]
          , style: R.css { backgroundColor: cssStringHSLA colors.white }
        }
    , example $
        column {
          children:
            [ loader
                { style: R.css {}
                , testId: null
                , color: colorNames.black7
                , bgColor: colorNames.white
                }
            ]
          , style: R.css { backgroundColor: cssStringHSLA colors.white }
        }
    , example $
        column {
          children:
            [ loader
                { style: R.css {}
                , testId: null
                , color: colorNames.black8
                , bgColor: colorNames.white
                }
            ]
          , style: R.css { backgroundColor: cssStringHSLA colors.white }
        }
    ]
