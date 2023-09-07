module Lumi.Components
  ( LumiProps
  , PropsModifier
  , PropsModifier'
  , propsModifier
  , LumiComponent
  , lumiComponent
  , lumiComponentFromHook
  , withContent, ($$$)
  , unsafeMaybeToNullableAttr
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (toNullable)
import Data.String (toLower)
import Effect (Effect)
import Lumi.Styles.Theme (LumiTheme)
import Prim.Row (class Lacks)
import React.Basic.Emotion as Emotion
import React.Basic.Hooks (Hook, JSX, ReactComponent, Render, element, reactComponent, reactComponentFromHook)
import Record.Unsafe.Union (unsafeUnion)
import Unsafe.Coerce (unsafeCoerce)

-- | A `LumiComponent` takes a function that updates its default props instead
-- | of the plain record of props itself. This helps reduce the surface area for
-- | API updates:
-- |
-- | ```purs
-- | React.Basic.element reactComponent
-- |   { relevantProp, otherProp1: mempty, otherProp2: mempty, ...etc }
-- |
-- | -- vs
-- |
-- | lumiComponent _{ relevantProp = relevantProp }
-- | ```
type LumiComponent props = (LumiProps props -> LumiProps props) -> JSX

type LumiProps props
  = { css :: LumiTheme -> Emotion.Style, className :: String | props }

type PropsModifier props = PropsModifier' props props

type PropsModifier' props props'
  = (LumiProps props -> LumiProps props) ->
    (LumiProps props' -> LumiProps props')

-- | Lift a `props -> props` function for composition with other `PropsModifier` functions.
propsModifier :: forall props. (LumiProps props -> LumiProps props) -> PropsModifier props
propsModifier = (>>>)

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
  c <- reactComponent name render
  pure
    $ lumiElement
    $ LumiInternalComponent
        { name
        , component: c
        , defaults
        , className: lumiComponentClassName name
        }

-- | Create a `LumiComponent` from a [react hook](https://github.com/spicydonuts/purescript-react-basic-hooks).
-- | This operation is useful for creating components that enhance a JSX tree
-- | with capabilities given by the Hook used.
lumiComponentFromHook ::
  forall hooks props r.
  Lacks "children" props =>
  Lacks "key" props =>
  Lacks "ref" props =>
  String ->
  { render :: r -> JSX | props } ->
  (LumiProps ( render :: r -> JSX | props ) -> Hook hooks r) ->
  Effect (LumiComponent ( render :: r -> JSX | props ))
lumiComponentFromHook name defaults propsToHook = do
  c <- reactComponentFromHook name propsToHook
  pure
    $ lumiElement
    $ LumiInternalComponent
        { name
        , component: c
        , defaults
        , className: lumiComponentClassName name
        }

-- | A convenient alias for setting the `content` property of a Lumi component
-- | if it exists.
infixr 0 withContent as $$$

withContent ::
  forall props content r.
  ((LumiProps (content :: content | props) -> LumiProps (content :: content | props)) -> r) ->
  content ->
  r
withContent m content = m _{ content = content }

-- # Internal

lumiComponentClassName :: String -> String
lumiComponentClassName name = "lumi-component lumi-" <> toLower name

newtype LumiInternalComponent props
  = LumiInternalComponent
  { name :: String
  , component :: ReactComponent (LumiProps props)
  , defaults :: { | props }
  , className :: String
  }

-- | Render a `LumiInternalComponent`. Similar to `React.Basic.element`, except
-- | the second argument is an update function rather than a plain record of props.
lumiElement :: forall props. LumiInternalComponent props -> LumiComponent props
lumiElement (LumiInternalComponent { component, defaults, className }) modifyProps =
  element component
    $ appendClassName
    $ modifyProps
    $ unsafeUnion ({ css: mempty, className: "" } :: LumiProps ())
    $ defaults
  where
  appendClassName props =
    props
      { className = className <> " " <> props.className
      }

-- | WARNING: This is for JS interop -- don't use this to unwrap Maybes!
-- |
-- | Unsafely nulls out a value so the resulting html attributes are less noisy
-- | Ex: `R.input { type: unsafeMaybeToNullableAttr Nothing }` avoids rendering
-- | the `type` attribute while still validating the type of the Maybe's content
-- | matches the type of the DOM field. It's only slightly safer than using
-- | `unsafeCreateDOMComponent` to avoid DOM type checking entirely.
unsafeMaybeToNullableAttr :: forall a. Maybe a -> a
unsafeMaybeToNullableAttr = unsafeCoerce <<< toNullable
