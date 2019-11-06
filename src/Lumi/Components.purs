module Lumi.Components where

import Prelude

import Effect (Effect)
import Prim.Row (class Lacks)
import React.Basic.Hooks (JSX, ReactComponent, Render, component)
import React.Basic.Hooks as React

type Component props defaults
  = { component :: ReactComponent props
    , defaults :: defaults
    }

type LumiComponent props defaults
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
  Effect (LumiComponent props defaults)
lumiComponent name defaults render = do
  c <- component name render
  pure
    { component: c
    , defaults
    }

lumiElement ::
  forall props defaults.
  LumiComponent ( | props )
    defaults ->
  (LumiComponentProps defaults -> LumiComponentProps props) -> JSX
lumiElement { component, defaults } fromDefaults = React.element component (fromDefaults defaults)
