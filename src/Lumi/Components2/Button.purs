module Lumi.Components2.Button
  ( Button
  , ButtonState(..), ButtonType(..)
  , ButtonProps
  , button

  , linkButton
  , LinkButtonProps
  , LinkButton

  , ButtonModifier
  , primary, secondary, resize, loadingContent

  , recolor
  , varButtonHue, varButtonHueDarker, varButtonHueDarkest
  , varButtonHueDisabled, varButtonGrey1, varButtonGrey2
  , varButtonBlack, varButtonWhite

  , submit, reset, state, onPress, onPress'
  , autoFocus, tabIndex, ariaLabel
  ) where

import Prelude

import Color (Color)
import Data.Array (fold)
import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, finally, launchAff_)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object (fromHomogeneous)
import Lumi.Components (LumiComponent, PropsModifier, lumiComponent, propsModifier, unsafeMaybeToNullableAttr)
import Lumi.Components.Color (ColorMap, shade)
import Lumi.Components.Size (Size(..))
import Lumi.Components2.Box as Box
import Lumi.Styles (StyleModifier, StyleProperty, block, color, css, default, ellipsis, hidden, inherit, inlineFlex, merge, nested, none, nowrap, pointer, px, px2, solid, str, style, style_, toCSS, underline, var)
import Lumi.Styles.Box (FlexAlign(..), _align, _focusable, _interactive, _justify, _row, box)
import Lumi.Styles.Loader (mkLoader)
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
      loading :: Boolean
      loading =
        clickInProgress || case props.state of
          Enabled -> false
          Disabled -> false
          Loading -> true
      disabled :: Boolean
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
              [ Box.box
                  $ _row
                  $ _align Center
                  $ _justify Center
                  $ _ { className = "button-content"
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
                , whiteSpace: nowrap
                , textOverflow: ellipsis
                , overflow: hidden
                , borderRadius: px 3
                , borderWidth: px 1
                , borderStyle: solid
                , fontSize: px fontSizes.body
                , padding: px2 10 20
                , height: px 40
                , "@media (min-width: 860px)":
                    nested
                      $ css
                          { fontSize: px fontSizes.body
                          , padding: px2 6 16
                          , height: px 32
                          }
                , "&:disabled": nested $ css { cursor: default }
                , "&[data-loading]":
                    nested
                      $ css
                          { display: block
                          , "&:after":
                              nested
                                $ merge
                                    [ mkLoader
                                        { color: var "--button-hue-loader"
                                        , radius: px 16
                                        , borderWidth: px 2
                                        }
                                    ]
                          , "> .button-content":
                              nested
                                $ css
                                  { visibility: hidden
                                  }
                          }
                }

type ButtonModifier c =
  forall r.
  PropsModifier
    ( component :: c
    , autoFocus :: Boolean
    , tabIndex :: Maybe Int
    , type :: ButtonType
    , state :: ButtonState
    , onPress :: Aff Unit
    , ariaLabel :: Maybe String
    | r
    )

-- The default button style
primary :: ButtonModifier Button
primary =
  recolor _.primary
    <<< style_
        ( css
            { color: varButtonWhite
            , borderColor: varButtonHue
            , backgroundColor: varButtonHue
            , "--button-hue-loader": varButtonWhite
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
            , "--button-hue-loader": varButtonGrey1
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
                      { color: varButtonGrey1
                      , borderColor: varButtonGrey2
                      , backgroundColor: varButtonWhite
                      }
            }
        )

resize :: Size -> ButtonModifier Button
resize size =
  style \(LumiTheme { colors, fontSizes, lineHeightFactor, textMarginFactor }) ->
    css
      { "@media (min-width: 860px)":
          nested
            $ fold
                [ case size of
                    Small ->
                      css
                        { fontSize: px fontSizes.subtext
                        , padding: px2 6 16
                        , height: px 28
                        , "&[data-loading]":
                            loadingStyles { radius: px 12, borderWidth: px 2 }
                        }
                    Medium ->
                      mempty
                    Large ->
                      css
                        { fontSize: px fontSizes.subsectionHeader
                        , padding: px2 12 24
                        , height: px 48
                        , "&[data-loading]":
                            loadingStyles { radius: px 24, borderWidth: px 3 }
                        }
                    ExtraLarge ->
                      css
                        { fontSize: px fontSizes.sectionHeader
                        , padding: px2 16 32
                        , height: px 64
                        , "&[data-loading]":
                            loadingStyles { radius: px 34, borderWidth: px 4 }
                        }
                    ExtraExtraLarge ->
                      css
                        { fontSize: px fontSizes.sectionHeader
                        , padding: px2 16 32
                        , height: px 64
                        , "&[data-loading]":
                            loadingStyles { radius: px 34, borderWidth: px 4 }
                        }
                ]
      }
    where
    loadingStyles { radius, borderWidth } =
      nested
        $ css
            { "&:after":
                nested
                  $ mkLoader
                      { color: var "--button-hue-loader"
                      , radius
                      , borderWidth
                      }
            }

data LinkButton = LinkButton

type LinkButtonProps
  = ( component :: LinkButton
    , autoFocus :: Boolean
    , tabIndex :: Maybe Int
    , onPress :: Aff Unit
    , type :: ButtonType
    , state :: ButtonState
    , ariaLabel :: Maybe String
    , content :: Array JSX
    , loadingContent :: Maybe (Array JSX)
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
    , loadingContent: Nothing
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
          , disabled
          , _data:
              unsafeMaybeToNullableAttr
                if loading then
                  Just (fromHomogeneous { loading: "" })
                else
                  Nothing
          , children:
              [ props.loadingContent # foldMap \lc ->
                  Box.box
                    $ _row
                    $ _justify Baseline
                    $ _ { className = "button-loading-content"
                        , content = lc
                        }
              , Box.box
                  $ _row
                  $ _justify Baseline
                  $ _ { className = "button-content"
                      , content = props.content
                      }
              ]
          }
    where
    linkButtonStyle :: StyleModifier
    linkButtonStyle =
      box
        <<< _interactive
        <<< _focusable
        <<< style_
            ( css
                { label: str "link-button"
                , appearance: none
                , outline: none
                , background: none
                , border: none
                , display: inlineFlex
                , whiteSpace: nowrap
                , textOverflow: ellipsis
                , overflow: hidden
                , fontSize: inherit -- A link button might appear in a paragraph of text
                                    -- and should inherit its size accordingly
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
                          { cursor: pointer
                          , textDecoration: underline
                          }
                , "&:disabled":
                    nested
                      $ css
                          { color: varButtonHueDisabled
                          , "&:hover, &:active":
                              nested
                                $ css
                                    { cursor: default
                                    , textDecoration: none
                                    }
                          }
                , "&[data-loading] > .button-loading-content + .button-content, &:not([data-loading]) > .button-loading-content":
                    -- Flattens the button content which is not currently active.
                    -- This preserves the button width regardless which state the
                    -- button is in.
                    nested $ css { height: px 0, overflow: hidden }
                }
            )

-- | `loadingContent` sets the content to display while a link button
-- | is in its loading state. The size of the link button will always
-- | fit the larger content, regardless which state it's in.
loadingContent :: Array JSX -> forall r. PropsModifier ( component :: LinkButton, loadingContent :: Maybe (Array JSX) | r )
loadingContent a =
  propsModifier _ { loadingContent = Just a }

recolor :: forall c. (ColorMap Color -> Color) -> ButtonModifier c
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

-- | A form submit button. This helper takes the button state
-- | as an argument because a form's buttons are generally
-- | tied to the validity and `onSubmit` behavior of the form,
-- | rather than providing an `onPress` action to the button
-- | itself.
submit :: forall c. ButtonState -> ButtonModifier c
submit s = propsModifier _ { type = Submit, state = s }

-- | A form reset button. This helper takes the button state
-- | as an argument because a form's buttons are generally
-- | tied to the validity and `onSubmit` behavior of the form,
-- | rather than providing an `onPress` action to the button
-- | itself.
reset :: forall c. ButtonState -> ButtonModifier c
reset s = propsModifier _ { type = Reset, state = s }

-- | Set the button state. Generally you'll want to use `submit` or
-- | `reset` instead, but this can still be useful for `onPress`
-- | buttons which need to be disabled while some page state is
-- | invalid or share in a collective page loading state.
-- |
-- | Note: When a page is in a loading state, any buttons which
-- |   were _not_ interacted with should be set to `Disabled`, not
-- |   `Loading`. A row of buttons containing the same spinner
-- |   looks strange. You also do not need to track which button
-- |   was interacted with, usually, because the `onPress` loading
-- |   state will override this one within the button pressed.
state :: forall c. ButtonState -> ButtonModifier c
state s = propsModifier _ { state = s }

-- | A button with customized `onPress` behavior. The button will
-- | automatically display a loading state while the action is in-progress.
-- | Using `onPress` on a button multiple times chains the effects together
-- | from the first applied, out.
onPress :: forall c. Aff Unit -> ButtonModifier c
onPress a = propsModifier \props -> props { onPress = props.onPress *> a }

-- | Like `onPress` but allows additional control over how the provided
-- | behavior interacts with any existing behavior.
onPress' :: forall c. (Aff Unit -> Aff Unit) -> ButtonModifier c
onPress' f = propsModifier \props -> props { onPress = f props.onPress }

-- | Auto-focus this button. Only one element on the page should
-- | have `autoFocus` set at a time.
autoFocus :: forall c. ButtonModifier c
autoFocus = propsModifier _ { autoFocus = true }

tabIndex :: forall c. Int -> ButtonModifier c
tabIndex i = propsModifier _ { tabIndex = Just i }

-- | Set `ariaLabel` when the button content is not legible text,
-- | for example a button that only contains an X might set this
-- | label to "Close".
ariaLabel :: forall c. String -> ButtonModifier c
ariaLabel l = propsModifier _ { ariaLabel = Just l }

----------------------------

invisibleSpace :: String
invisibleSpace = "\x2063"
