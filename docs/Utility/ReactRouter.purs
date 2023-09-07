module Lumi.Components.Utility.ReactRouter where

import Prelude

import Effect.Uncurried (EffectFn1)
import React.Basic.Classic (ReactComponent)
import Web.HTML.History (URL)

type RouterProps props =
  { history :: { push :: EffectFn1 URL Unit }
  , location :: { pathname :: String, search :: String, hash :: String }
  | props
  }

foreign import withRouter
  :: forall props
   . ReactComponent (RouterProps props)
  -> ReactComponent { | props }
