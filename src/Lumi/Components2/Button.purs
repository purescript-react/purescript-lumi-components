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
import Foreign.Object (fromHomogeneous)
import Lumi.Components (LumiComponent, LumiComponentProps, lumiComponent)
import Lumi.Components.Size (Size(..))
import Lumi.Styles.Button (ButtonKind(..), ButtonState(..))
import Lumi.Styles.Button as Styles.Button
import Lumi.Styles.Theme (LumiTheme)
import Prim.Row (class Union)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)
import React.Basic.Emotion as E
import React.Basic.Hooks (JSX, ReactComponent, ReactContext, useContext)
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

mkButton :: ReactContext LumiTheme -> Effect (LumiComponent ButtonProps ButtonProps)
mkButton t = do
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

  render props = React.do
    theme <- useContext t
    let
      buttonStyle = Styles.Button.button theme props.color props.kind props.buttonState props.size
    pure
      if props.type == "link" then
        E.element buttonStyle lumiButtonLinkElement
          { "aria-label": Nullable.toNullable props.accessibilityLabel
          , children: props.content
          , className: props.className
          , onClick: capture_ props.onPress
          , role: "button"
          , _data:
            fromHomogeneous
              { testid: fold props.testId
              }
          }
      else
        E.element buttonStyle lumiButtonElement
          { "aria-label": Nullable.toNullable props.accessibilityLabel
          , children: props.content
          , className: props.className
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

defaults :: LumiComponentProps ButtonProps
defaults =
  { accessibilityLabel: mempty
  , onPress: mempty
  , size: Medium
  , testId: mempty
  , type: mempty
  , kind: Primary
  , buttonState: Enabled
  , color: Nothing
  , className: ""
  , content: mempty
  }

primary :: LumiComponentProps ButtonProps
primary = defaults

secondary :: LumiComponentProps ButtonProps
secondary =
  defaults
    { kind = Secondary
    }

linkStyle :: LumiComponentProps ButtonProps
linkStyle =
  defaults
    { type = "link"
    -- , kind = LinkButton
    }

invisibleSpace :: String
invisibleSpace = fromCharArray $ Array.catMaybes [ fromCharCode 0x2063 ]

