module Lumi.Components.Icon where

import Prelude

import Effect.Unsafe (unsafePerformEffect)
import JSS (JSS, jss)
import React.Basic.Classic (Component, JSX, createComponent, element, makeStateless)
import React.Basic.DOM (CSS, css, unsafeCreateDOMComponent)

data IconType
  = ArrowDown
  | ArrowLeft
  | ArrowRight
  | ArrowUp
  | Bin
  | Cart
  | Check
  | Close
  | Currency
  | Grid
  | Hamburger
  | Indeterminate
  | Info
  | List
  | Minus
  | Overflow
  | Percent
  | Plus
  | Remove
  | Search
  | Rearrange

instance showIconType :: Show IconType where
  show ArrowDown = "↓"
  show ArrowLeft = "←"
  show ArrowRight = "→"
  show ArrowUp = "↑"
  show Bin = "🗑"
  show Cart = "🛒"
  show Check = "✓"
  show Close = "X"
  show Currency = "$"
  show Grid = "⌗"
  show Hamburger = ""
  show Indeterminate = "⎻"
  show Info = "?"
  show List = ""
  show Minus = "-"
  show Overflow = "⋯"
  show Percent = "%"
  show Plus = "+"
  show Remove = "x"
  show Search = "🔍"
  show Rearrange = ""

iconArrowDown :: IconType
iconArrowDown = ArrowDown

iconArrowLeft :: IconType
iconArrowLeft = ArrowLeft

iconArrowRight :: IconType
iconArrowRight = ArrowRight

iconArrowUp :: IconType
iconArrowUp = ArrowUp

iconBin :: IconType
iconBin = Bin

iconCart :: IconType
iconCart = Cart

iconClose :: IconType
iconClose = Close

iconCurrency :: IconType
iconCurrency = Currency

iconGrid :: IconType
iconGrid = Grid

iconHamburger :: IconType
iconHamburger = Hamburger

iconIndeterminate :: IconType
iconIndeterminate = Indeterminate

iconInfo :: IconType
iconInfo = Info

iconList :: IconType
iconList = List

iconMinus :: IconType
iconMinus = Minus

iconOverflow :: IconType
iconOverflow = Overflow

iconPercent :: IconType
iconPercent = Percent

iconPlus :: IconType
iconPlus = Plus

iconRemove :: IconType
iconRemove = Remove

iconSearch :: IconType
iconSearch = Search

iconRearrange :: IconType
iconRearrange = Rearrange

type IconProps =
  { type_ :: IconType
  , style :: CSS
  }

component :: Component IconProps
component = createComponent "Icon"

icon :: IconProps -> JSX
icon = makeStateless component $ lumiIconElement <<< mapProps
  where
    lumiIconElement = element (unsafePerformEffect $ unsafeCreateDOMComponent "lumi-font-icon")

    mapProps props =
      { "data-icon-char": show props.type_
      , style: props.style
      }

icon_ :: IconType -> JSX
icon_ type_ = icon
  { type_
  , style: css {}
  }

styles :: JSS
styles = jss
  { "@font-face":
      { fontFamily: "lumi-font-icons"
      , src: "url(https://s3-us-west-2.amazonaws.com/lumi-gumball/fonts/unicode/lumi-font-icons.woff)"
      , fontWeight: "400"
      , fontStyle: "normal"
      }

  , "@global":
      { "lumi-font-icon":
          { fontSize: "inherit"
          , color: "inherit"
          , lineHeight: "inherit"
          , "&::before":
              { fontFamily: "lumi-font-icons!important"
              , content: "attr(data-icon-char)"
              , fontStyle: "normal!important"
              , fontWeight: "normal!important"
              , fontVariant: "normal!important"
              , textTransform: "none!important"
              , speak: "none"
              , "WebkitFontSmoothing": "antialiased"
              , "MozOsxFontSmoothing": "grayscale"
              , verticalAlign: "middle"
              }
          }
      }
  }
