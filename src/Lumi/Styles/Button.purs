module Lumi.Styles.Button where

import Prelude
import Color (Color, darken, desaturate, lighten)
import Data.Foldable (fold)
import Data.Maybe (Maybe, fromMaybe)
import Lumi.Components (PropsModifier)
import Lumi.Components.Size (Size(..))
import Lumi.Components.ZIndex (ziButtonGroup)
import Lumi.Styles (merge, none, styleModifier, styleModifier_)
import Lumi.Styles.Box (FlexAlign(..), _align, _focusable, _interactive, _justify, _row, box)
import Lumi.Styles.Link as Link
import Lumi.Styles.Loader (mkLoader, spin)
import Lumi.Styles.Theme (LumiTheme(..))
import React.Basic.Emotion (color, css, int, nested, str)

data ButtonKind
  = Primary
  | Secondary
  | Link

data ButtonState
  = Enabled
  | Disabled
  | Loading

button ::
  forall props.
  Maybe Color ->
  ButtonKind ->
  ButtonState ->
  Size ->
  PropsModifier props
button colo kind state size = case kind of
  Primary ->
    buttonStyle
      >>> styleModifier \theme@(LumiTheme { colors }) ->
          let
            { hue, hueDarker, hueDarkest, hueDisabled, white } =
              makeColorShades
                { hue: fromMaybe colors.primary colo
                , black: colors.black
                , white: colors.white
                }

            disabledStyles =
              css
                { cursor: str "default"
                , color: color white
                , borderColor: color hueDisabled
                , backgroundColor: color hueDisabled
                }
          in
            case state of
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
              Loading ->
                merge
                  [ disabledStyles
                  , loadingStyles theme
                  ]
  Secondary ->
    buttonStyle
      >>> styleModifier \theme@(LumiTheme { colors }) ->
          let
            { hueDarker, hueDarkest, grey1, grey2, white, black } =
              makeColorShades
                { hue: fromMaybe colors.primary colo
                , black: colors.black
                , white: colors.white
                }

            disabledStyles =
              css
                { cursor: str "default"
                , color: color grey1
                , borderColor: color grey2
                , backgroundColor: color white
                }
          in
            case state of
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
              Loading ->
                merge
                  [ disabledStyles
                  , loadingStyles theme
                  ]
  Link ->
    Link.link
      >>> styleModifier \(LumiTheme { colors }) ->
          let
            { hueDisabled } =
              makeColorShades
                { hue: fromMaybe colors.primary colo
                , black: colors.black
                , white: colors.white
                }

            disabledStyles =
              css
                { cursor: str "default"
                , color: color hueDisabled
                , "&:hover, &:active":
                    nested
                      $ css
                          { cursor: str "default"
                          , textDecoration: none
                          }
                }
          in
            merge
              [ css
                  { label: str "button"
                  , appearance: none
                  , padding: int 0
                  , background: none
                  , border: none
                  }
              , case state of
                  Disabled -> disabledStyles
                  Loading -> disabledStyles
                  Enabled -> mempty
              ]
  where
  buttonStyle =
    box
      >>> _row
      >>> _align Center
      >>> _justify Center
      >>> case state of
          Disabled -> identity
          Loading -> identity
          Enabled -> _interactive >>> _focusable
      >>> styleModifier_
          ( css
              { label: str "button"
              , appearance: none
              , minWidth: int 70
              , padding: str "10px 20px"
              , fontSize: int 14
              , lineHeight: int 1
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
                                , height: int 28
                                }
                            Medium -> mempty
                            Large ->
                              css
                                { fontSize: int 15
                                , padding: str "12px 24px"
                                , height: int 48
                                }
                            ExtraLarge ->
                              css
                                { fontSize: int 20
                                , padding: str "16px 32px"
                                , height: int 64
                                }
                            ExtraExtraLarge ->
                              css
                                { fontSize: int 20
                                , padding: str "16px 32px"
                                , height: int 64
                                }
                        ]
              }
          )

  loadingStyles theme =
    merge
      [ spin
      , css
          { label: str "loading"
          , "&:after": nested $ mkLoader theme { radius: "16px", borderWidth: "2px" }
          , "@media (min-width: 860px)":
              nested case size of
                Small ->
                  css
                    { "&:after":
                        nested
                          $ mkLoader theme { radius: "12px", borderWidth: "2px" }
                    }
                Medium -> mempty
                Large ->
                  css
                    { "&:after":
                        nested
                          $ mkLoader theme { radius: "24px", borderWidth: "3px" }
                    }
                ExtraLarge ->
                  css
                    { "&:after":
                        nested
                          $ mkLoader theme { radius: "34px", borderWidth: "4px" }
                    }
                ExtraExtraLarge ->
                  css
                    { "&:after":
                        nested
                          $ mkLoader theme { radius: "34px", borderWidth: "4px" }
                    }
          }
      ]

  makeColorShades { hue, white, black } =
    let
      hueDarker = darken 0.1 hue

      hueDarkest = darken 0.15 hue

      hueDisabled = lighten 0.4137 $ desaturate 0.1972 hue

      grey1 = lighten 0.7 black

      grey2 = lighten 0.82 black
    in
      { hue, hueDarker, hueDarkest, hueDisabled, grey1, grey2, white, black }

buttonGroup :: forall props. Boolean -> PropsModifier props
buttonGroup joined =
  box
    >>> _row
    >>> styleModifier_ (css { label: str "buttonGroup" })
    >>> styleModifier_
        if not joined then
          css
            { label: str "notJoined"
            , "& > *:not(:last-child)":
                nested
                  $ css
                      { marginRight: int 8
                      }
            }
        else
          css
            { label: str "joined"
            , "& > *:not(:last-child)":
                nested
                  $ css
                      { marginRight: int (-1)
                      , borderTopRightRadius: int 0
                      , borderBottomRightRadius: int 0
                      }
            , "& > *:not(:first-child)":
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
