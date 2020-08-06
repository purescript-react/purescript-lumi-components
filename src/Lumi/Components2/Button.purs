module Lumi.Components2.Button
  ( Button
  , ButtonState(..), ButtonType(..)
  , button

  , linkButton
  , LinkButton

  , ButtonModifier
  , primary, secondary, resize

  , recolor
  , varButtonHue, varButtonHueDarker, varButtonHueDarkest
  , varButtonHueDisabled, varButtonGrey1, varButtonGrey2
  , varButtonBlack, varButtonWhite
  ) where

import Prelude

import Color (Color)
import Data.Array (fold)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, finally, launchAff_)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object (fromHomogeneous)
import Lumi.Components (LumiComponent, PropsModifier, lumiComponent, unsafeMaybeToNullableAttr)
import Lumi.Components.Button (invisibleSpace)
import Lumi.Components.Color (ColorMap, shade)
import Lumi.Components.Size (Size(..))
import Lumi.Components2.Box as Box
import Lumi.Styles (StyleModifier, StyleProperty, color, css, inherit, merge, nested, none, px, str, style, style_, toCSS)
import Lumi.Styles.Box (FlexAlign(..), _align, _focusable, _interactive, _justify, _row, box)
import Lumi.Styles.Loader (mkLoader, spin)
import Lumi.Styles.Theme (LumiTheme(..), useTheme)
import React.Basic.DOM as R
import React.Basic.Emotion as E
import React.Basic.Events (handler_)
import React.Basic.Hooks (JSX, useState', (/\))
import React.Basic.Hooks as React

data Button = Button'

data ButtonState
  = Enabled
  | Disabled
  | Loading

data ButtonType
  = Button
  | Submit
  | Reset

type ButtonProps
  = ( component :: Button
    , autoFocus :: Boolean
    , tabIndex :: Maybe Int
    , onPress :: Aff Unit
    , type :: ButtonType
    , state :: ButtonState
    -- Set `ariaLabel` when the button content is not legible text,
    -- for example a button that only contains an X might set this
    -- label to "Close".
    , ariaLabel :: Maybe String
    , content :: Array JSX
    )

button :: LumiComponent ButtonProps
button = primary >>>
  unsafePerformEffect do
    lumiComponent "Button" defaults render
  where
  defaults :: Record ButtonProps
  defaults =
    { component: Button'
    , ariaLabel: mempty
    , autoFocus: false
    , tabIndex: Nothing
    , onPress: mempty
    , type: Button
    , state: Enabled
    , content: mempty
    }

  render props = React.do
    theme <- useTheme
    clickInProgress /\ setClickInProgress <- useState' false
    let
      loading =
        clickInProgress || case props.state of
          Enabled -> false
          Disabled -> false
          Loading -> true
      disabled =
        loading || case props.state of
          Enabled -> false
          Disabled -> true
          Loading -> true
    pure
      $ E.element R.button'
          { _aria:
              unsafeMaybeToNullableAttr
                $ map (fromHomogeneous <<< { label: _ }) props.ariaLabel
          , autoFocus: props.autoFocus
          , tabIndex: unsafeMaybeToNullableAttr props.tabIndex
          , css: toCSS buttonStyle theme <> props.css theme
          , className: props.className
          , onClick: handler_ do
              setClickInProgress true
              launchAff_ do
                props.onPress # finally do
                  liftEffect do setClickInProgress false
          , type:
              case props.type of
                Button -> "button"
                Submit -> "submit"
                Reset -> "reset"
          , disabled
          , _data:
              unsafeMaybeToNullableAttr
                if loading then
                  Just (fromHomogeneous { loading: "" })
                else
                  Nothing
          , children:
              [ Box.box _
                  { className = "button-content"
                  , content =
                      if Array.length props.content == 0 then
                        [ R.text invisibleSpace ] -- preserves button size when content is empty
                      else
                        props.content
                  }
              ]
          }
    where
    buttonStyle :: StyleModifier
    buttonStyle =
      box
        <<< _row
        <<< _align Center
        <<< _justify Center
        <<< _interactive
        <<< _focusable
        <<< style \(theme@(LumiTheme { colors, fontSizes })) ->
              css
                { label: str "button"
                , appearance: none
                , outline: none
                , minWidth: px 70
                , lineHeight: px 1
                , whiteSpace: str "nowrap"
                , textOverflow: str "ellipsis"
                , overflow: str "hidden"
                , borderRadius: px 3
                , borderWidth: px 1
                , borderStyle: str "solid"
                , fontSize: px fontSizes.body
                , padding: str "10px 20px"
                , height: px 40
                , "@media (min-width: 860px)":
                    nested
                      $ css
                          { fontSize: px fontSizes.body
                          , padding: str "6px 16px"
                          , height: px 32
                          }
                , "&:disabled": nested $ css { cursor: str "default" }
                , "&[data-loading]":
                    nested
                      $ merge
                          [ spin
                          , css
                              { "&:after":
                                  nested
                                    $ merge
                                        [ css { position: str "absolute" }
                                        , mkLoader
                                            { color: colors.white
                                            , highlightColor: colors.transparent
                                            , radius: "16px"
                                            , borderWidth: "2px"
                                            }
                                        ]
                              , "> .button-content": nested $ css { opacity: str "0" }
                              }
                          ]
                }

type ButtonModifier c = forall r. PropsModifier ( component :: c | r )

-- The default button style
primary :: ButtonModifier Button
primary =
  recolor _.primary
    <<< style_
        ( css
            { color: varButtonWhite
            , borderColor: varButtonHue
            , backgroundColor: varButtonHue
            , "&:hover":
                nested
                  $ css
                      { color: varButtonWhite
                      , borderColor: varButtonHueDarker
                      , backgroundColor: varButtonHueDarker
                      }
            , "&:active":
                nested
                  $ css
                      { color: varButtonWhite
                      , borderColor: varButtonHueDarkest
                      , backgroundColor: varButtonHueDarkest
                      }
            , "&:disabled":
                nested
                  $ css
                      { color: varButtonWhite
                      , borderColor: varButtonHueDisabled
                      , backgroundColor: varButtonHueDisabled
                      }
            }
        )

-- An outline-style button
secondary :: ButtonModifier Button
secondary =
  recolor _.primary
    <<< style_
        ( css
            { color: varButtonBlack
            , borderColor: varButtonGrey1
            , backgroundColor: varButtonWhite
            , "&:hover":
                nested
                  $ css
                      { color: varButtonHueDarker
                      , borderColor: varButtonHueDarker
                      , backgroundColor: varButtonWhite
                      }
            , "&:active":
                nested
                  $ css
                      { color: varButtonHueDarkest
                      , borderColor: varButtonHueDarkest
                      , backgroundColor: varButtonWhite
                      }
            , "&:disabled":
                nested
                  $ css
                      { borderColor: varButtonGrey2
                      , color: varButtonGrey1
                      , backgroundColor: varButtonWhite
                      }
            }
        )

resize :: Size -> ButtonModifier Button
resize size =
  style \(LumiTheme { fontSizes, lineHeightFactor, textMarginFactor }) ->
    css
      { "@media (min-width: 860px)":
          nested
            $ fold
                [ case size of
                    Small ->
                      css
                        { fontSize: px fontSizes.subtext
                        , padding: str "6px 16px"
                        , height: px 28
                        }
                    Medium ->
                      mempty
                    Large ->
                      css
                        { fontSize: px fontSizes.subsectionHeader
                        , padding: str "12px 24px"
                        , height: px 48
                        }
                    ExtraLarge ->
                      css
                        { fontSize: px fontSizes.sectionHeader
                        , padding: str "16px 32px"
                        , height: px 64
                        }
                    ExtraExtraLarge ->
                      css
                        { fontSize: px fontSizes.sectionHeader
                        , padding: str "16px 32px"
                        , height: px 64
                        }
                ]
      }

    -- loadingStyles theme size =
    --   merge
    --     [ spin
    --     , css
    --         { label: str "loading"
    --         , "&:after": nested $ mkLoader theme { radius: "16px", borderWidth: "2px" }
    --         , "@media (min-width: 860px)":
    --             nested case size of
    --               Small ->
    --                 css
    --                   { "&:after":
    --                       nested
    --                         $ mkLoader theme { radius: "12px", borderWidth: "2px" }
    --                   }
    --               Medium -> mempty
    --               Large ->
    --                 css
    --                   { "&:after":
    --                       nested
    --                         $ mkLoader theme { radius: "24px", borderWidth: "3px" }
    --                   }
    --               ExtraLarge ->
    --                 css
    --                   { "&:after":
    --                       nested
    --                         $ mkLoader theme { radius: "34px", borderWidth: "4px" }
    --                   }
    --               ExtraExtraLarge ->
    --                 css
    --                   { "&:after":
    --                       nested
    --                         $ mkLoader theme { radius: "34px", borderWidth: "4px" }
    --                   }
    --         }
    --     ]


data LinkButton = LinkButton

type LinkButtonProps
  = ( component :: LinkButton
    , autoFocus :: Boolean
    , tabIndex :: Maybe Int
    , onPress :: Aff Unit
    , type :: ButtonType
    , state :: ButtonState
    -- Set `ariaLabel` when the button content is not legible text,
    -- for example a button that only contains an X might set this
    -- label to "Close".
    , ariaLabel :: Maybe String
    , content :: Array JSX
    )

linkButton :: LumiComponent LinkButtonProps
linkButton = recolor _.primary >>>
  unsafePerformEffect do
    lumiComponent "LinkButton" defaults render
  where
  defaults :: Record LinkButtonProps
  defaults =
    { component: LinkButton
    , autoFocus: false
    , tabIndex: Nothing
    , onPress: mempty
    , type: Button
    , state: Enabled
    , ariaLabel: Nothing
    , content: mempty
    }

  render props = React.do
    theme <- useTheme
    clickInProgress /\ setClickInProgress <- useState' false
    pure
      $ E.element R.button'
          { _aria:
              unsafeMaybeToNullableAttr
                $ map (fromHomogeneous <<< { label: _ }) props.ariaLabel
          , autoFocus: props.autoFocus
          , tabIndex: unsafeMaybeToNullableAttr props.tabIndex
          , css: toCSS linkButtonStyle theme <> props.css theme
          , className: props.className
          , onClick: handler_ do
              setClickInProgress true
              launchAff_ do
                props.onPress # finally do
                  liftEffect do setClickInProgress false
          , type:
              case props.type of
                Button -> "button"
                Submit -> "submit"
                Reset -> "reset"
          , disabled:
              clickInProgress ||
                case props.state of
                  Enabled -> false
                  Disabled -> true
                  Loading -> true
          , children:
              if Array.length props.content == 0 then
                [ R.text invisibleSpace ] -- preserves button size when content is empty
              else
                props.content
          }
    where
    linkButtonStyle :: StyleModifier
    linkButtonStyle =
      box
        <<< _row
        <<< _align Baseline
        <<< _interactive
        <<< _focusable
        <<< style \(LumiTheme { fontSizes }) ->
              css
                { label: str "link-button"
                , appearance: none
                , outline: none
                , background: none
                , border: none
                , display: str "inline-flex"
                , whiteSpace: str "nowrap"
                , textOverflow: str "ellipsis"
                , overflow: str "hidden"
                , fontSize: inherit -- TODO: Set fixed link button size? -- px fontSizes.body
                , color: varButtonHue
                , textDecoration: none
                , "&:visited":
                    nested
                      $ css
                          { color: varButtonHue
                          , textDecoration: none
                          }
                , "&:hover":
                    nested
                      $ css
                          { cursor: str "pointer"
                          , textDecoration: str "underline"
                          }
                , "&:disabled":
                    nested
                      $ css
                          { color: varButtonHueDisabled
                          , "&:hover, &:active":
                              nested
                                $ css
                                    { cursor: str "default"
                                    , textDecoration: none
                                    }
                          }
                }

recolor :: forall b. (ColorMap Color -> Color) -> ButtonModifier b
recolor f =
  style
    ( \theme@(LumiTheme { colors: colors@{ black, white } }) ->
        let
          shades =
            shade { hue: f colors, black, white }
        in
          css
            { "--button-hue": color shades.hue
            , "--button-hue-darker": color shades.hueDarker
            , "--button-hue-darkest": color shades.hueDarkest
            , "--button-hue-disabled": color shades.hueDisabled
            , "--button-grey1": color shades.grey1
            , "--button-grey2": color shades.grey2
            , "--button-black": color shades.black
            , "--button-white": color shades.white
            }
    )

varButtonHue :: StyleProperty
varButtonHue = var "--button-hue"

varButtonHueDarker :: StyleProperty
varButtonHueDarker = var "--button-hue-darker"

varButtonHueDarkest :: StyleProperty
varButtonHueDarkest = var "--button-hue-darkest"

varButtonHueDisabled :: StyleProperty
varButtonHueDisabled = var "--button-hue-disabled"

varButtonGrey1 :: StyleProperty
varButtonGrey1 = var "--button-grey1"

varButtonGrey2 :: StyleProperty
varButtonGrey2 = var "--button-grey2"

varButtonBlack :: StyleProperty
varButtonBlack = var "--button-black"

varButtonWhite :: StyleProperty
varButtonWhite = var "--button-white"

------------------------------------------------------

-- TODO: move to react-basic-emotion
var :: String -> StyleProperty
var n = str ("var(" <> n <> ")")
