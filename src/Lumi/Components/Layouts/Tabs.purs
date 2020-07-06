module Lumi.Components.Layouts.Tabs where

import Prelude

import Data.Maybe (Maybe(..))
import Data.NonEmpty (NonEmpty, oneOf)
import Effect (Effect)
import Lumi.Components.Tab (TabId, TabKey)
import Lumi.Components.Tab as Tab
import React.Basic.Classic (Component, JSX, createComponent, makeStateless)
import React.Basic.DOM as R
import Web.HTML.History (URL)

component :: Component TabLayoutProps
component = createComponent "TabsLayout"

type TabLayoutProps =
  { currentLocation :: URL
  , queryKey :: TabKey
  , useHash :: Boolean
  , navigate :: Maybe (URL -> Effect Unit)
  , tabs :: NonEmpty Array
      { id :: TabId
      , label :: String
      , count :: Maybe Int
      , content :: Unit -> JSX
      }
  }

tabLayout :: TabLayoutProps -> JSX
tabLayout = makeStateless component render
  where
    render { currentLocation, navigate, queryKey, tabs, useHash } =
      Tab.tabs
        { style: R.css { "paddingLeft": "24px" }
        , tabStyle: R.css {}
        , selectedTabStyle: R.css {}
        , currentLocation
        , queryKey
        , useHash
        , navigate
        , tabs: oneOf tabs <#> \t ->
            { content: t.content
            , id: t.id
            , label: t.label
            , count: t.count
            , testId: Nothing
            }
        }
