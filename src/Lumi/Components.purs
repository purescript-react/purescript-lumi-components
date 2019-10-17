module Lumi.Components where

import Effect (Effect)
import Lumi.Styles.Theme (LumiTheme)
import React.Basic (JSX, ReactComponent, ReactContext)

type LumiComponent p
  = ReactContext LumiTheme ->
    Effect (ReactComponent (LumiComponentProps p))

type LumiComponent' p
  = ReactContext LumiTheme ->
    Effect (ReactComponent (LumiComponentProps' p))

type LumiComponentProps p
  = { className :: String | p }

type LumiComponentProps' p
  = LumiComponentProps ( content :: Array JSX | p )
