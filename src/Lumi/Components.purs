module Lumi.Components where

import Prelude

import Data.String (toLower)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Prim.Row (class Lacks)
import React.Basic.Emotion as Emotion
import React.Basic.Hooks (JSX, ReactComponent, Render, component)
import Record as Record

type LumiProps (props :: # Type)
  = { className :: String | props }

type LumiModifier (props :: # Type)
  = { css :: Emotion.Style, className :: String | props } ->
    { css :: Emotion.Style, className :: String | props }

type PropsModifier props = LumiModifier props -> LumiModifier props

propsModifier :: forall props. LumiModifier props -> PropsModifier props
propsModifier f m = m >>> f

type StyleModifier = forall props. PropsModifier props

styleModifier :: Emotion.Style -> StyleModifier
styleModifier s m = m >>> \p -> p { css = p.css <> s }

newtype LumiComponent (props :: # Type)
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
lumiComponent name defaults render = do
  c <- component name render
  pure $
    LumiComponent
      { name
      , component: c
      , defaults
      , className: "lumi-component lumi-" <> toLower name
      }

lumiElement ::
  forall props.
  Lacks "css" props =>
  Lacks "className" props =>
  LumiComponent props ->
  LumiModifier props ->
  JSX
lumiElement (LumiComponent { component, defaults, className }) modifyProps =
  Emotion.element component
    $ appendClassName
    $ modifyProps
    $ Record.insert _css mempty
    $ Record.insert _className ""
    $ defaults
  where
    _css = SProxy :: _ "css"
    _className = SProxy :: _ "className"

    appendClassName :: LumiModifier props
    appendClassName props = props { className = className <> " " <> props.className }

infixr 0 lumiElement as %
