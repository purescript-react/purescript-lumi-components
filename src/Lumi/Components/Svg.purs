module Lumi.Components.Svg where

import Prelude

import Color (cssStringHSLA)
import Lumi.Components.Color (colors)
import React.Basic.Classic (JSX)
import React.Basic.DOM.SVG as RS

placeholderSvg :: JSX
placeholderSvg =
  RS.svg
    { viewBox: "0 0 24 24"
    , fill: "none"
    , stroke: cssStringHSLA colors.black4
    , xmlns: "http://www.w3.org/2000/svg"
    , children:
        [ "M4 16l4.586-4.586a2 2 0 012.828 0L16 16m-2-2l1.586-1.586a2 2 0 012.828 0L20 14m-6-6h.01M6 20h12a2 2 0 002-2V6a2 2 0 00-2-2H6a2 2 0 00-2 2v12a2 2 0 002 2z"
        ]
          <#>
            { d: _
            , strokeLinecap: "round"
            , strokeLinejoin: "round"
            }
          >>> RS.path
    }

clientSvg :: JSX
clientSvg =
  RS.svg
    { xmlns: "http://www.w3.org/2000/svg"
    , width: "100%"
    , viewBox: "0 0 200 200"
    , children:
        [ RS.g
            { fill
            , fillRule
            , children:
                [ RS.path { d }
                , RS.path
                    { d: "M32.939 71.563l10.19-2.73 35.199 131.365H67.356z"
                    }
                ]
            }
        ]
    }
  where
    fill = "#DDDDDC"
    fillRule = "evenodd"
    d = "M147.61 55.396a2.89 2.89 0 0 0-2.458.186c-.15.085-15.444 8.745-28.157 12.151-4.362 1.17-7.85 1.576-10.364 1.205-3-.44-3.763-1.892-4.09-3.11-.95-3.546-4.434-8.997-17.082-5.608C71.61 63.93 53.366 76.076 52.593 76.59c-.977.656-1.435 1.829-1.141 2.928l16.669 62.21c.225.84.862 1.519 1.702 1.81a2.85 2.85 0 0 0 2.534-.328c.182-.116 18.226-12.13 31.16-15.595 9.158-2.454 10.03.796 10.315 1.864 1.303 4.863 6.098 9.785 21.532 5.65l.006-.002c13.374-3.584 28.609-12.122 29.248-12.488 1.098-.616 1.64-1.862 1.325-3.039l-16.728-62.427a2.652 2.652 0 0 0-1.605-1.776"

userSvg :: JSX
userSvg =
  RS.svg
    { xmlns: "http://www.w3.org/2000/svg"
    , width: "100%"
    , viewBox: "0 0 200 200"
    , children:
        [ RS.g
            { fill
            , fillRule
            , children:
                [ RS.path { d }
                ]
            }
        ]
    }
  where
    fill = "#DDDDDC"
    fillRule = "nonzero"
    d = "M30.613 200c4.223-39.011 29.994-69 69.387-69s65.164 29.989 69.387 69H30.613zM100 120c30.281 0 39-17.909 38-40s-12.31-40-38-40c-25.69 0-37 17.909-38 40s7.719 40 38 40z"
