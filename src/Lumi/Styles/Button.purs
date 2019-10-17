module Lumi.Styles.Button where

import Prelude

import Color (cssStringHSLA, darken)
import Data.Foldable (fold)
import Data.Monoid (guard)
import Lumi.Components.Size (Size(..))
import Lumi.Components.ZIndex (ziButtonGroup)
import Lumi.Styles.Box (FlexAlign(..), align, interactive, justify, row)
import Lumi.Styles.Theme (LumiTheme)
import React.Basic.Emotion (Style, color, css, int, merge, selector, str)

data ButtonKind
  = Primary
  | Secondary

-- | Outline
-- | Link
data ButtonState
  = Enabled
  | Disabled
  | Loading

button :: LumiTheme -> ButtonKind -> ButtonState -> Size -> Style
button theme buttonKind buttonState size =
  merge
    [ row
    , align Center
    , justify Center
    , interactive
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
        , borderColor: color theme.colors.primary
        , borderRadius: int 3
        , borderWidth: int 1
        , borderStyle: str "solid"
        , color: color theme.colors.white
        , backgroundColor: color theme.colors.primary
        , "&:hover":
          selector
            $ css
                { borderColor: color $ darken 0.1 theme.colors.primary
                , backgroundColor: color $ darken 0.1 theme.colors.primary
                }
        , "&:active":
          selector
            $ css
                { borderColor: color $ darken 0.1 theme.colors.primary
                , backgroundColor: color $ darken 0.15 theme.colors.primary
                }
        , "&:disabled": selector disabledStyle
        , "&:focus":
          selector
            $ css
                { outline: str "0"
                , boxShadow: str ("0 0 0 3px " <> cssStringHSLA theme.colors.primary3)
                }
        , "@media (min-width: 860px)":
          selector
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
    , case buttonKind of
        Primary -> mempty
        Secondary ->
          css
            { borderColor: color theme.colors.black3
            , color: color theme.colors.black
            , backgroundColor: color theme.colors.transparent
            , "&:hover":
              selector
                $ css
                    { borderColor: color theme.colors.primary
                    , color: color theme.colors.primary
                    , backgroundColor: color theme.colors.transparent
                    }
            }
    , case buttonState of
        Enabled -> mempty
        Disabled -> disabledStyle
        Loading -> disabledStyle
    ]
  where
  disabledStyle = case buttonKind of
    Primary ->
      css
        { borderColor: color theme.colors.primary2
        , backgroundColor: color theme.colors.primary2
        , cursor: str "default"
        }
    Secondary ->
      css
        { color: color theme.colors.black2
        , borderColor: color theme.colors.black3
        , backgroundColor: color theme.colors.transparent
        , cursor: str "default"
        , "&:hover":
          selector
            $ css
                { color: color theme.colors.black2
                , borderColor: color theme.colors.black3
                , backgroundColor: color theme.colors.transparent
                }
        }

buttonGroup :: Boolean -> Style
buttonGroup joined =
  merge
    [ row
    , guard (not joined) do
        css
          { "& > *:not(:last-child)":
            selector
              $ css
                  { marginRight: int 10
                  }
          }
    , guard joined do
        css
          { "& > button:not(:last-child)":
            selector
              $ css
                  { marginRight: int (-1)
                  , borderTopRightRadius: int 0
                  , borderBottomRightRadius: int 0
                  }
          , "& > button:not(:first-child)":
            selector
              $ css
                  { borderTopLeftRadius: int 0
                  , borderBottomLeftRadius: int 0
                  }
          , "& > *:focus, & > *:hover":
            selector
              $ css
                  { zIndex: int ziButtonGroup
                  }
          }
    ]

-- styles :: JSS
-- styles =
--   jss
--     { "@global":
--       { "button.lumi":
--         { "&[data-color=\"secondary\"]":
--           { extend: buttonSecondary
--           , backgroundColor: cssStringHSLA colors.transparent
--           , "&:hover":
--             { color: cssStringHSLA colors.primary
--             , borderColor: cssStringHSLA colors.primary
--             }
--           , "&:disabled, &[data-loading=\"true\"]":
--             { color: cssStringHSLA colors.black2
--             , borderColor: cssStringHSLA colors.black3
--             }
--           }
--         , "&[data-loading=\"true\"]":
--           { "&:after": spinnerMixin { radius: "16px", borderWidth: "2px" }
--           , "@media (min-width: $break-point-mobile)":
--             { "&[data-size=\"small\"]":
--               { "&:after": spinnerMixin { radius: "12px", borderWidth: "2px" }
--               }
--             , "&[data-size=\"large\"]":
--               { "&:after": spinnerMixin { radius: "24px", borderWidth: "3px" }
--               }
--             , "&[data-size=\"extra-large\"]":
--               { "&:after": spinnerMixin { radius: "34px", borderWidth: "4px" }
--               }
--             }
--           }
--         , "&[data-color=\"black\"]": buttonColorHoverMixin colors.black
--         , "&[data-color=\"black-1\"]": buttonColorHoverMixin colors.black1
--         , "&[data-color=\"black-2\"]": buttonColorHoverMixin colors.black2
--         , "&[data-color=\"black-3\"]": buttonColorHoverMixin colors.black3
--         , "&[data-color=\"black-4\"]": buttonColorHoverMixin colors.black4
--         , "&[data-color=\"black-5\"]": buttonColorHoverMixin colors.black5
--         , "&[data-color=\"black-6\"]": buttonColorHoverMixin colors.black6
--         , "&[data-color=\"black-7\"]": buttonColorHoverMixin colors.black7
--         , "&[data-color=\"black-8\"]": buttonColorHoverMixin colors.black8
--         , "&[data-color=\"primary\"]": buttonColorHoverMixin colors.primary
--         , "&[data-color=\"primary-1\"]": buttonColorHoverMixin colors.primary1
--         , "&[data-color=\"primary-2\"]": buttonColorHoverMixin colors.primary2
--         , "&[data-color=\"primary-3\"]": buttonColorHoverMixin colors.primary3
--         , "&[data-color=\"primary-4\"]": buttonColorHoverMixin colors.primary4
--         , "&[data-color=\"accent-1\"]": buttonColorHoverMixin colors.accent1
--         , "&[data-color=\"accent-2\"]": buttonColorHoverMixin colors.accent2
--         , "&[data-color=\"accent-3\"]": buttonColorHoverMixin colors.accent3
--         , "&[data-color=\"accent-3-3\"]": buttonColorHoverMixin colors.accent33
--         , "&[data-color=\"white\"]": buttonColorHoverMixin colors.white
--         , "&[data-color=\"finished\"]": buttonColorHoverMixin colors.primary
--         , "&[data-color=\"active\"]": buttonColorHoverMixin colors.accent1
--         , "&[data-color=\"warning\"]": buttonColorHoverMixin colors.accent2
--         , "&[data-color=\"error\"]": buttonColorHoverMixin colors.accent3
--         }
--       }
--     }
--   where
--   buttonColorHoverMixin value =
--     { backgroundColor: cssStringHSLA value
--     , "&:hover": { backgroundColor: cssStringHSLA $ darken 0.1 value }
--     , "&:active": { backgroundColor: cssStringHSLA $ darken 0.15 value }
--     , "&:disabled, &[data-loading=\"true\"]":
--       { backgroundColor: cssStringHSLA $ lighten 0.4137 $ desaturate 0.1972 $ value
--       }
--     }
