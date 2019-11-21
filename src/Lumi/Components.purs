module Lumi.Components where

import Prelude
import Data.String (toLower)
import Effect (Effect)
import Prim.Row (class Lacks)
import React.Basic.Emotion as Emotion
import React.Basic.Hooks (JSX, ReactComponent, Render, component)
import Record.Unsafe.Union (unsafeUnion)

type LumiProps props
  = { css :: Emotion.Style, className :: String | props }

-- type LumiModifier props
--   = LumiTheme -> LumiProps props -> LumiProps props
-- type PropsModifier props
--   = LumiModifier props -> LumiModifier props
-- propsModifier ::
--   forall props.
--   LumiModifier props ->
--   PropsModifier props
-- propsModifier f1 f2 t = f1 t <<< f2 t
type LumiModifier props
  = LumiProps props -> LumiProps props

type PropsModifier props
  = LumiModifier props -> LumiModifier props

newtype LumiComponent props
  = LumiComponent
  { name :: String
  , component :: ReactComponent { className :: String | props }
  , defaults :: { | props }
  , className :: String
  }

lumiComponent ::
  forall hooks props.
  Lacks "children" props =>
  Lacks "css" props =>
  Lacks "key" props =>
  Lacks "ref" props =>
  String ->
  { | props } ->
  ({ className :: String | props } -> Render Unit hooks JSX) ->
  Effect (LumiComponent props)
lumiComponent name defaults render = do
  c <- component name render
  pure
    $ LumiComponent
        { name
        , component: c
        , defaults
        , className: "lumi-component lumi-" <> toLower name
        }

lumiElement ::
  forall props.
  LumiComponent props ->
  LumiModifier props ->
  JSX
lumiElement (LumiComponent { component, defaults, className }) modifyProps =
  Emotion.element component
    $ appendClassName
    $ modifyProps
    $ unsafeUnion { css: mempty, className: "" }
    $ defaults
  where
  appendClassName props =
    props
      { className = className <> " " <> props.className
      }

withContent :: forall props content. content -> LumiModifier ( content :: content | props )
withContent content = _ { content = content }
