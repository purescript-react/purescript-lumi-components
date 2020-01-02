module Lumi.Components
  ( LumiProps
  , PropsModifier
  , propsModifier
  , LumiComponent
  , lumiComponent
  , lumiComponentFromHook
  , lumiElement
  ) where

import Prelude
import Data.String (toLower)
import Effect (Effect)
import Lumi.Styles.Theme (LumiTheme)
import Prim.Row (class Lacks)
import React.Basic.Emotion as Emotion
import React.Basic.Hooks (JSX, ReactComponent, Render, component, componentFromHook, element)
import Record.Unsafe.Union (unsafeUnion)

type LumiProps props
  = { css :: LumiTheme -> Emotion.Style, className :: String | props }

type PropsModifier props
  = (LumiProps props -> LumiProps props) ->
    (LumiProps props -> LumiProps props)

-- | Lift a `props -> props` function for composition with other `PropsModifier` functions.
propsModifier :: forall props. (LumiProps props -> LumiProps props) -> PropsModifier props
propsModifier = (>>>)

newtype LumiComponent props
  = LumiComponent
  { name :: String
  , component :: ReactComponent (LumiProps props)
  , defaults :: { | props }
  , className :: String
  }

-- | Create a `LumiComponent` from a name, set of defaults, and a render function.
-- | The render function behaves the same as in the [hooks API](https://github.com/spicydonuts/purescript-react-basic-hooks/blob/master/examples/counter/src/Counter.purs#L12).
lumiComponent ::
  forall hooks props.
  Lacks "children" props =>
  Lacks "key" props =>
  Lacks "ref" props =>
  String ->
  { | props } ->
  (LumiProps props -> Render Unit hooks JSX) ->
  Effect (LumiComponent props)
lumiComponent name defaults render = do
  c <- component name render
  pure
    $ LumiComponent
        { name
        , component: c
        , defaults
        , className: lumiComponentClassName name
        }

lumiComponentFromHook ::
  forall hooks props r.
  Lacks "children" props =>
  Lacks "key" props =>
  Lacks "ref" props =>
  String ->
  { render :: r -> JSX | props } ->
  (LumiProps ( render :: r -> JSX | props ) -> Render Unit hooks r) ->
  Effect (LumiComponent ( render :: r -> JSX | props ))
lumiComponentFromHook name defaults propsToHook = do
  c <- componentFromHook name propsToHook
  pure
    $ LumiComponent
        { name
        , component: c
        , defaults
        , className: lumiComponentClassName name
        }

lumiComponentClassName :: String -> String
lumiComponentClassName name = "lumi-component lumi-" <> toLower name

-- | Render a `LumiComponent`. Similar to `React.Basic.element`, except the second argument
-- | is an update function rather than a plain record for props. This helps reduce
-- | the surface area for API updates:
-- |
-- | ```purs
-- | React.Basic.element reactComponent
-- |   { relevantProp, otherProp1: mempty, otherProp2: mempty, ...etc }
-- |
-- | -- vs
-- |
-- | lumiElement lumiComponent _{ relevantProp = relevantProp }
-- | ```
lumiElement ::
  forall props.
  LumiComponent props ->
  (LumiProps props -> LumiProps props) ->
  JSX
lumiElement (LumiComponent { component, defaults, className }) modifyProps =
  element component
    $ appendClassName
    $ modifyProps
    $ unsafeUnion { css: mempty, className: "" }
    $ defaults
  where
  appendClassName props =
    props
      { className = className <> " " <> props.className
      }
