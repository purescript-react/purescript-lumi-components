module Lumi.Components.Images where

import Prelude

import Color (cssStringHSLA)
import Effect.Unsafe (unsafePerformEffect)
import JSS (JSS, jss)
import Lumi.Components.Color (colors)
import Lumi.Components.Size (Size)
import React.Basic.Classic (JSX, element)
import React.Basic.DOM (CSS, css, unsafeCreateDOMComponent)

avatar
  :: { image :: JSX
     , size :: Size
     , style :: CSS
     }
  -> JSX
avatar = \{ image, size, style } ->
  avatarEl
    { "data-size": show size
    , children: image
    , style
    }
  where
  avatarEl = element (unsafePerformEffect $ unsafeCreateDOMComponent "lumi-avatar")

productThumb
  :: { image :: JSX
     , size :: Size
     , style :: CSS
     }
  -> JSX
productThumb = \{ image, size, style } ->
  productThumbEl
    { "data-size": show size
    , children: image
    , style
    }
  where
  productThumbEl = element (unsafePerformEffect $ unsafeCreateDOMComponent "lumi-product-thumb")

avatar_ :: { image :: JSX, size :: Size } -> JSX
avatar_ { image, size } = avatar { image, size, style: css {} }

defaultAvatar :: Size -> JSX
defaultAvatar size = avatar_ { size, image: mempty }

productThumb_ :: { image :: JSX, size :: Size } -> JSX
productThumb_ { image, size } = productThumb { image, size, style: css {} }

styles :: JSS
styles = jss
  { "@global":
      { "lumi-avatar":
          { boxSizing: "border-box"
          , overflow: "hidden"
          , display: "flex"

          , border: [ "1px", "solid", cssStringHSLA colors.black4 ]
          , borderRadius: "50%"

          , backgroundColor: cssStringHSLA colors.white
          , backgroundImage: "url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='100%25' viewBox='0 0 200 200'%3E%3Cg fill='%23DDDDDC' fill-rule='nonzero'%3E%3Cpath d='M30.613 200c4.223-39.011 29.994-69 69.387-69s65.164 29.989 69.387 69H30.613zM100 120c30.281 0 39-17.909 38-40s-12.31-40-38-40c-25.69 0-37 17.909-38 40s7.719 40 38 40z'%3E%3C/path%3E%3C/g%3E%3C/svg%3E\")"
          , backgroundClip: "padding-box"
          , backgroundSize: "cover"
          , backgroundPosition: "center"
          , backgroundRepeat: "no-repeat"

          , "&[data-size=\"small\"]":
              { width: "24px"
              , height: "24px"
              }
          , "&[data-size=\"medium\"]":
              { width: "30px"
              , height: "30px"
              }
          , "&[data-size=\"large\"]":
              { width: "36px"
              , height: "36px"
              }
          , "&[data-size=\"extra-large\"]":
              { width: "140px"
              , height: "140px"
              }

          , "& > *":
              { maxHeight: "100%"
              , maxWidth: "100%"
              }
          }

      , "lumi-product-thumb":
          { boxSizing: "border-box"
          , overflow: "hidden"
          , display: "flex"

          , border: [ "1px", "solid", cssStringHSLA colors.black4 ]

          , backgroundColor: cssStringHSLA colors.black6
          , backgroundSize: "50%"
          , backgroundPosition: "center"
          , backgroundRepeat: "no-repeat"

          , "&[data-size=\"small\"]":
              { backgroundImage: "url(\"data:image/svg+xml;charset=UTF-8,%3csvg width='112' height='99' xmlns='http://www.w3.org/2000/svg'%3e%3cg stroke-width='7' stroke='%23CBCBC9' fill='none' fill-rule='evenodd' opacity='.7'%3e%3cpath d='M111 95L80.9997013 43 56 81.1326499 31.0002987 60.3333333 1 95'/%3e%3cpath d='M1 98h110V1H1z'/%3e%3cpath d='M49 33c0 6.6314752-5.3685248 12-12 12-6.62359 0-12-5.3685248-12-12 0-6.62359 5.37641-12 12-12 6.6314752 0 12 5.37641 12 12z'/%3e%3c/g%3e%3c/svg%3e \")"
              , width: "40px"
              , height: "40px"
              }
          , "&[data-size=\"medium\"]":
              { backgroundImage: "url(\"data:image/svg+xml;charset=UTF-8,%3csvg width='112' height='99' xmlns='http://www.w3.org/2000/svg'%3e%3cg stroke-width='6' stroke='%23CBCBC9' fill='none' fill-rule='evenodd' opacity='.7'%3e%3cpath d='M111 95L80.9997013 43 56 81.1326499 31.0002987 60.3333333 1 95'/%3e%3cpath d='M1 98h110V1H1z'/%3e%3cpath d='M49 33c0 6.6314752-5.3685248 12-12 12-6.62359 0-12-5.3685248-12-12 0-6.62359 5.37641-12 12-12 6.6314752 0 12 5.37641 12 12z'/%3e%3c/g%3e%3c/svg%3e \")"
              , width: "56px"
              , height: "56px"
              }
          , "&[data-size=\"large\"]":
              { backgroundImage: "url(\"data:image/svg+xml;charset=UTF-8,%3csvg width='112' height='99' xmlns='http://www.w3.org/2000/svg'%3e%3cg stroke-width='4' stroke='%23CBCBC9' fill='none' fill-rule='evenodd' opacity='.7'%3e%3cpath d='M111 95L80.9997013 43 56 81.1326499 31.0002987 60.3333333 1 95'/%3e%3cpath d='M1 98h110V1H1z'/%3e%3cpath d='M49 33c0 6.6314752-5.3685248 12-12 12-6.62359 0-12-5.3685248-12-12 0-6.62359 5.37641-12 12-12 6.6314752 0 12 5.37641 12 12z'/%3e%3c/g%3e%3c/svg%3e \")"
              , width: "72px"
              , height: "72px"
              }
          , "&[data-size=\"extra-large\"]":
              { backgroundImage: "url(\"data:image/svg+xml;charset=UTF-8,%3csvg width='112' height='99' xmlns='http://www.w3.org/2000/svg'%3e%3cg stroke-width='2' stroke='%23CBCBC9' fill='none' fill-rule='evenodd' opacity='.7'%3e%3cpath d='M111 95L80.9997013 43 56 81.1326499 31.0002987 60.3333333 1 95'/%3e%3cpath d='M1 98h110V1H1z'/%3e%3cpath d='M49 33c0 6.6314752-5.3685248 12-12 12-6.62359 0-12-5.3685248-12-12 0-6.62359 5.37641-12 12-12 6.6314752 0 12 5.37641 12 12z'/%3e%3c/g%3e%3c/svg%3e \")"
              , width: "140px"
              , height: "140px"
              }

          , "& > *":
              { maxHeight: "100%"
              , maxWidth: "100%"
              }
          }
      }
  }
