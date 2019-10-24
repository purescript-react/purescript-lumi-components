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
import Lumi.Components (LumiComponent', LumiComponentProps')
import Lumi.Components.Size (Size(..))
import Lumi.Styles.Button (ButtonKind(..), ButtonState(..))
import Lumi.Styles.Button as Styles.Button
import Prim.Row (class Union)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)
import React.Basic.Emotion as E
import React.Basic.Hooks (ReactComponent, component, useContext)
import React.Basic.Hooks as React

type CommonButtonProps rest =
  ( accessibilityLabel :: Maybe String
  , onPress :: Effect Unit
  , size :: Size
  , type :: String
  , kind :: ButtonKind
  , buttonState :: ButtonState
  , color :: Maybe Color
  , testId :: Maybe String
  | rest
  )

type ButtonProps = CommonButtonProps ()

mkButton :: LumiComponent' ButtonProps
mkButton t = do
  component "Button" render
  where
    lumiButtonElement
      :: forall attrs attrs_
      . Union attrs attrs_ (|R.Props_button)
      => ReactComponent
        { className :: String
        , "aria-label" :: Nullable.Nullable String
        | attrs
        }
    lumiButtonElement = R.unsafeCreateDOMComponent "button"

    lumiButtonLinkElement
      :: forall attrs attrs_
      . Union attrs attrs_ (|R.Props_a)
      => ReactComponent
        { className :: String
        , "aria-label" :: Nullable.Nullable String
        | attrs
        }
    lumiButtonLinkElement = R.unsafeCreateDOMComponent "a"

    render props = React.do
      theme <- useContext t
      pure
        if props.type == "link"
        then E.element lumiButtonLinkElement
          { css: Styles.Button.button theme props.color props.kind props.buttonState props.size
          , "aria-label": Nullable.toNullable props.accessibilityLabel
          , children: props.content
          , className: props.className
          , onClick: capture_ props.onPress
          , role: "button"
          , _data: fromHomogeneous
            { testid: fold props.testId
            }
          }
        else
          E.element lumiButtonElement
            { css: Styles.Button.button theme props.color props.kind props.buttonState props.size
            , "aria-label": Nullable.toNullable props.accessibilityLabel
            , children: props.content
            , className: props.className
            , onClick: capture_ props.onPress
            , type: props.type
            , _data: fromHomogeneous
              { testid: fold props.testId
              , size: show props.size
              , loading:
                show
                  case props.buttonState of
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
          if Array.length props.content == 0
            then [ R.text invisibleSpace ] -- preserves button size when content is empty
            else props.content

defaults :: LumiComponentProps' ButtonProps
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

primary :: LumiComponentProps' ButtonProps
primary = defaults

secondary :: LumiComponentProps' ButtonProps
secondary = defaults
  { kind = Secondary
  }

linkStyle :: LumiComponentProps' ButtonProps
linkStyle = defaults
  { type = "link"
  -- , kind = LinkButton
  }

invisibleSpace :: String
invisibleSpace = fromCharArray $ Array.catMaybes [ fromCharCode 0x2063 ]

-- type IconButtonProps = CommonButtonProps
--   ( iconLeft :: Maybe IconType
--   , iconRight :: Maybe IconType
--   )

-- iconComponent :: Component IconButtonProps
-- iconComponent = createComponent "IconButton"

-- iconButton :: IconButtonProps -> JSX
-- iconButton = makeStateless iconComponent render
--   where
--     lumiButtonElement = element (unsafeCreateDOMComponent "button")
--     render props =
--       lumiButtonElement
--         { "aria-label": props.accessibilityLabel
--         , children: children
--         , className: "lumi icon-button"
--         , "data-color": props.color
--         , "data-size": show props.size
--         , "data-testid": props.testId
--         , onClick: props.onPress
--         , style: props.style
--         , type: props.type
--         , disabled:
--             case props.buttonState of
--               Enabled -> false
--               Disabled -> true
--               Loading -> true
--         , "data-loading":
--             case props.buttonState of
--               Enabled -> false
--               Disabled -> true
--               Loading -> true
--         }
--       where
--         children =
--           fold $ Array.catMaybes
--             [ props.iconLeft <#> \iconLeft ->
--                 icon
--                   { type_: iconLeft
--                   , style: R.css { marginRight: "8px", fontSize: "11px" }
--                   }
--             , Just $ R.text props.title
--             , props.iconRight <#> \iconRight ->
--                 icon
--                   { type_: iconRight
--                   , style: R.css { marginLeft: "8px", fontSize: "11px" }
--                   }
--             ]

-- iconButtonDefaults :: IconButtonProps
-- iconButtonDefaults =
--   { accessibilityLabel: Nullable.toNullable Nothing
--   , color: Nullable.toNullable Nothing
--   , onPress: mkEffectFn1 (const $ pure unit)
--   , size: Medium
--   , style: css {}
--   , testId: Nullable.toNullable Nothing
--   , title: invisibleSpace
--   , type: ""
--   , iconLeft: Nothing
--   , iconRight: Nothing
--   , buttonState: Enabled
--   }
