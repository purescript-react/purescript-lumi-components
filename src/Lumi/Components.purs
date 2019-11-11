module Lumi.Components where

import Prelude

import Data.String (joinWith, toLower)
import Effect (Effect)
import Prim.Row (class Lacks)
import React.Basic.Emotion as Emotion
import React.Basic.Hooks (JSX, ReactComponent, Render, component)
import React.Basic.Hooks as React

type Component props defaults
  = { name :: String
    , component :: ReactComponent props
    , defaults :: defaults
    , className :: String
    }

type LumiComponent props
  = LumiComponent' props props

type LumiComponent' props defaults
  = Component (LumiComponentProps props) (LumiComponentProps defaults)

type LumiComponentProps p
  = { className :: String | p }

lumiComponent ::
  forall hooks props defaults.
  Lacks "children" props =>
  Lacks "key" props =>
  Lacks "ref" props =>
  String ->
  LumiComponentProps defaults ->
  (LumiComponentProps props -> Render Unit hooks JSX) ->
  Effect (LumiComponent' props defaults)
lumiComponent name defaults render = do
  c <- component name render
  pure
    { name
    , component: c
    , defaults
    , className: "lumi lumi-" <> toLower name
    }

lumiElement ::
  forall props defaults.
  LumiComponent' ( | props ) defaults ->
  (LumiComponentProps defaults -> LumiComponentProps props) -> JSX
lumiElement { component, defaults, className } fromDefaults =
  React.element
    component
    ((appendLumiComponentClassName className <<< fromDefaults) defaults)

infixl 2 lumiElement as %

lumiElement' ::
  forall props defaults.
  Lacks "css" props =>
  Emotion.Style ->
  LumiComponent' ( | props ) defaults ->
  (LumiComponentProps defaults -> LumiComponentProps props) -> JSX
lumiElement' style { component, defaults, className } fromDefaults =
  Emotion.element
    style
    component
    ((appendLumiComponentClassName className <<< fromDefaults) defaults)

appendLumiComponentClassName ::
  forall props.
  String ->
  LumiComponentProps props ->
  LumiComponentProps props
appendLumiComponentClassName n r =
  r
    { className = joinWith " " [ n, r.className ]
    }
