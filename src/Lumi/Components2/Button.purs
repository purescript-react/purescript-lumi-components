-- | WARNING: not production ready -- this is a demo of react-basic-emotion and LumiComponent
module Lumi.Components2.Button where

import Prelude

import Color (Color)
import Data.Array as Array
import Data.Char (fromCharCode)
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.Nullable as Nullable
import Data.String.CodeUnits (fromCharArray)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object (fromHomogeneous)
import Lumi.Components (LumiComponent, PropsModifier, lumiComponent, propsModifier)
import Lumi.Components.Size (Size(..))
import Lumi.Styles (toCSS)
import Lumi.Styles.Button (ButtonKind(..), ButtonState(..))
import Lumi.Styles.Button as Styles.Button
import Lumi.Styles.Theme (lumiThemeContext)
import Prim.Row (class Union)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)
import React.Basic.Emotion as E
import React.Basic.Hooks (JSX, ReactComponent, useContext)
import React.Basic.Hooks as React

type CommonButtonProps rest
  = ( accessibilityLabel :: Maybe String
    , onPress :: Effect Unit
    , size :: Size
    , type :: String
    , kind :: ButtonKind
    , buttonState :: ButtonState
    , color :: Maybe Color
    , testId :: Maybe String
    , content :: Array JSX
    | rest
    )

type ButtonProps
  = CommonButtonProps ()

button :: LumiComponent ButtonProps
button = unsafePerformEffect do
  lumiComponent "Button" defaults render
  where
  lumiButtonElement ::
    forall attrs attrs_.
    Union attrs attrs_ ( | R.Props_button ) =>
    ReactComponent
      { className :: String
      , "aria-label" :: Nullable.Nullable String
      | attrs
      }
  lumiButtonElement = R.unsafeCreateDOMComponent "button"

  lumiButtonLinkElement ::
    forall attrs attrs_.
    Union attrs attrs_ ( | R.Props_a ) =>
    ReactComponent
      { className :: String
      , "aria-label" :: Nullable.Nullable String
      | attrs
      }
  lumiButtonLinkElement = R.unsafeCreateDOMComponent "a"

  defaults :: Record ButtonProps
  defaults =
    { accessibilityLabel: mempty
    , onPress: mempty
    , size: Medium
    , testId: mempty
    , type: mempty
    , kind: Primary
    , buttonState: Enabled
    , color: Nothing
    , content: mempty
    }

  render props = React.do
    theme <- useContext lumiThemeContext
    let
      buttonStyle =
        Styles.Button.button props.color props.kind props.buttonState props.size
    pure
      if props.type == "link" then
        E.element lumiButtonLinkElement
          { "aria-label": Nullable.toNullable props.accessibilityLabel
          , children: props.content
          , className: props.className
          , css: toCSS theme props buttonStyle
          , onClick: capture_ props.onPress
          , role: "button"
          , _data:
            fromHomogeneous
              { testid: fold props.testId
              }
          }
      else
        E.element lumiButtonElement
          { "aria-label": Nullable.toNullable props.accessibilityLabel
          , children: props.content
          , className: props.className
          , css: toCSS theme props buttonStyle
          , onClick: capture_ props.onPress
          , type: props.type
          , _data:
            fromHomogeneous
              { testid: fold props.testId
              , size: show props.size
              , loading:
                show case props.buttonState of
                  Enabled -> false
                  Disabled -> false
                  Loading -> true
              -- , color: un ColorName props.color
              }
          , disabled:
            case props.buttonState of
              Enabled -> false
              Disabled -> true
              Loading -> false
          }
    where
    children =
      if Array.length props.content == 0 then
        [ R.text invisibleSpace ] -- preserves button size when content is empty
      else
        props.content

primary :: PropsModifier ButtonProps
primary = identity

secondary :: PropsModifier ButtonProps
secondary =
  propsModifier
    _
      { kind = Secondary
      }

linkStyle :: PropsModifier ButtonProps
linkStyle =
  propsModifier
    _
      { type = "link"
      -- , kind = LinkButton
      }

invisibleSpace :: String
invisibleSpace = fromCharArray $ Array.catMaybes [ fromCharCode 0x2063 ]
