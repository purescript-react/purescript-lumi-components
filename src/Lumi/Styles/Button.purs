module Lumi.Styles.Button where

import Prelude

import Color (Color, darken, desaturate, lighten)
import Data.Foldable (fold)
import Data.Maybe (Maybe, fromMaybe)
import Data.Monoid (guard)
import Lumi.Components.Size (Size(..))
import Lumi.Components.ZIndex (ziButtonGroup)
import Lumi.Styles.Box (FlexAlign(..), align, focusable, interactive, justify, row)
import Lumi.Styles.Theme (LumiTheme)
import React.Basic.Emotion (Style, color, css, int, merge, nested, str)

data ButtonKind
  = Primary
  | Secondary

data ButtonState
  = Enabled
  | Disabled
  | Loading

button :: LumiTheme -> Maybe Color -> ButtonKind -> ButtonState -> Size -> Style
button theme hue buttonKind buttonState size =
  merge
    [ row
    , align Center
    , justify Center
    , interactive
    , focusable theme
    , css
        { appearance: str "none"
        , minWidth: int 70
        , padding: str "10px 20px"
        , fontSize: int 14
        , lineHeight: int 20
        , whiteSpace: str "nowrap"
        , textOverflow: str "ellipsis"
        , overflow: str "hidden"
        , height: int 40
        , borderRadius: int 3
        , borderWidth: int 1
        , borderStyle: str "solid"
        , "@media (min-width: 860px)":
          nested
            $ fold
                [ css
                    { padding: str "6px 16px"
                    , height: int 32
                    }
                , case size of
                    Small ->
                      css
                        { fontSize: int 12
                        , lineHeight: int 16
                        , height: int 28
                        }
                    Medium -> mempty
                    Large ->
                      css
                        { fontSize: int 15
                        , lineHeight: int 24
                        , padding: str "12px 24px"
                        , height: int 48
                        }
                    ExtraLarge ->
                      css
                        { fontSize: int 20
                        , lineHeight: int 32
                        , padding: str "16px 32px"
                        , height: int 64
                        }
                ]
        }
    , _buttonStateStyles
      { hue: fromMaybe theme.colors.primary hue
      , black: theme.colors.black
      , white: theme.colors.white
      }
      buttonKind
      buttonState
    ]

_buttonStateStyles ::
  { hue :: Color
  , black :: Color
  , white :: Color
  } ->
  ButtonKind -> ButtonState -> Style
_buttonStateStyles { hue, white, black } buttonKind buttonState =
  let
    hueDarker = darken 0.1 hue

    hueDarkest = darken 0.15 hue

    hueDisabled = lighten 0.4137 $ desaturate 0.1972 hue

    grey1 = lighten 0.7 black

    grey2 = lighten 0.82 black
  in
    case buttonKind of
      Primary ->
        let
          disabledStyles =
            css
              { cursor: str "default"
              , color: color white
              , borderColor: color hueDisabled
              , backgroundColor: color hueDisabled
              }
        in
          case buttonState of
            Enabled ->
              css
                { borderColor: color hue
                , color: color white
                , backgroundColor: color hue
                , "&:hover":
                  nested
                    $ css
                        { borderColor: color hueDarker
                        , backgroundColor: color hueDarker
                        }
                , "&:active":
                  nested
                    $ css
                        { borderColor: color hueDarkest
                        , backgroundColor: color hueDarkest
                        }
                , "&:disabled": nested disabledStyles
                }
            Disabled -> disabledStyles
            Loading -> disabledStyles
      Secondary ->
        let
          disabledStyles =
            css
              { cursor: str "default"
              , color: color grey1
              , borderColor: color grey2
              , backgroundColor: color white
              }
        in
          case buttonState of
            Enabled ->
              css
                { borderColor: color grey1
                , color: color black
                , backgroundColor: color white
                , "&:hover":
                  nested
                    $ css
                        { borderColor: color hueDarker
                        , color: color hueDarker
                        , backgroundColor: color white
                        }
                , "&:active":
                  nested
                    $ css
                        { borderColor: color hueDarkest
                        , color: color hueDarkest
                        , backgroundColor: color white
                        }
                , "&:disabled": nested disabledStyles
                }
            Disabled -> disabledStyles
            Loading -> disabledStyles

buttonGroup :: Boolean -> Style
buttonGroup joined =
  merge
    [ row
    , guard (not joined) do
        css
          { "& > *:not(:last-child)":
            nested
              $ css
                  { marginRight: int 10
                  }
          }
    , guard joined do
        css
          { "& > button:not(:last-child)":
            nested
              $ css
                  { marginRight: int (-1)
                  , borderTopRightRadius: int 0
                  , borderBottomRightRadius: int 0
                  }
          , "& > button:not(:first-child)":
            nested
              $ css
                  { borderTopLeftRadius: int 0
                  , borderBottomLeftRadius: int 0
                  }
          , "& > *:focus, & > *:hover":
            nested
              $ css
                  { zIndex: int ziButtonGroup
                  }
          }
    ]
