module Lumi.Components2.Button where

import Prelude

import Color (Color)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components (LumiComponent, PropsModifier, lumiComponent, propsModifier)
import Lumi.Components.Button (invisibleSpace)
import Lumi.Components.Size (Size(..))
import Lumi.Styles (toCSS)
import Lumi.Styles.Button (ButtonKind(..), ButtonState(..))
import Lumi.Styles.Button as Styles.Button
import Lumi.Styles.Theme (useTheme)
import Prim.Row (class Union)
import React.Basic.DOM as R
import React.Basic.Emotion as E
import React.Basic.Events (handler_)
import React.Basic.Hooks (JSX, ReactComponent)
import React.Basic.Hooks as React

type ButtonProps
  = ( accessibilityLabel :: Maybe String
    , onPress :: Effect Unit
    , size :: Size
    , type :: String
    , kind :: ButtonKind
    , state :: ButtonState
    , color :: Maybe Color
    , content :: Array JSX
    )

button :: LumiComponent ButtonProps
button =
  unsafePerformEffect do
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

  defaults :: Record ButtonProps
  defaults =
    { accessibilityLabel: mempty
    , onPress: mempty
    , size: Medium
    , type: mempty
    , kind: Primary
    , state: Enabled
    , color: Nothing
    , content: mempty
    }

  render props = React.do
    theme <- useTheme
    pure
      $ E.element lumiButtonElement
          { "aria-label": Nullable.toNullable props.accessibilityLabel
          , children
          , className: props.className
          , css:
            theme # toCSS (Styles.Button.button props.color props.kind props.state props.size) <> props.css
          , onClick: handler_ props.onPress
          , type: props.type
          , disabled:
            case props.state of
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

_secondary :: forall props. PropsModifier ( kind :: ButtonKind | props )
_secondary =
  propsModifier
    _
      { kind = Secondary
      }

_linkStyle :: forall props. PropsModifier ( kind :: ButtonKind | props )
_linkStyle =
  propsModifier
    _
      { kind = Link
      }
