module Lumi.Components where

import Prelude hiding (bind, discard)
import Data.String (toLower)
import Effect (Effect)
import Lumi.Styles.Theme (LumiTheme)
import Prelude as Prelude
import Prim.Row (class Lacks)
import React.Basic.Emotion as Emotion
import React.Basic.Hooks (JSX, ReactComponent, Render, component, element)
import Record.Unsafe.Union (unsafeUnion)

type LumiProps props
  = { css :: LumiTheme -> Emotion.Style, className :: String | props }

type LumiModifier props
  = LumiProps props -> LumiProps props

type PropsModifier props
  = LumiModifier props -> LumiModifier props

propsModifier :: forall props. LumiModifier props -> PropsModifier props
propsModifier f m = m >>> f

newtype LumiComponent props
  = LumiComponent
  { name :: String
  , component :: ReactComponent (LumiProps props)
  , defaults :: { | props }
  , className :: String
  }

lumiComponent ::
  forall hooks props.
  Lacks "children" props =>
  Lacks "key" props =>
  Lacks "ref" props =>
  String ->
  { | props } ->
  (LumiProps props -> Render Unit hooks JSX) ->
  Effect (LumiComponent props)
lumiComponent name defaults render = Prelude.do
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
  element component
    $ appendClassName
    $ modifyProps
    $ unsafeUnion { css: mempty, className: "" }
    $ defaults
  where
  appendClassName :: LumiModifier props
  appendClassName props = props { className = className <> " " <> props.className }

infixr 0 lumiElement as %

withContent ::
  forall props content.
  PropsModifier ( content :: content | props ) ->
  content ->
  LumiModifier ( content :: content | props )
withContent p content = p _ { content = content }

infixr 0 withContent as %%%

bind :: forall props. PropsModifier props -> (Unit -> PropsModifier props) -> PropsModifier props
bind m f = m >>> f unit

discard :: forall props. PropsModifier props -> (Unit -> PropsModifier props) -> PropsModifier props
discard = bind
