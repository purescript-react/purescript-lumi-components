module Lumi.Components.Examples.Tab where

import Prelude

import Control.MonadZero (guard)
import Data.Array (index, (..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split, toLower)
import Effect.Uncurried (runEffectFn1)
import Lumi.Components.Column (column_)
import Lumi.Components.Tab (TabId(..), TabKey(..), tabs, urlParts)
import Lumi.Components.Text (h2_)
import Lumi.Components.Utility.ReactRouter (RouterProps, withRouter)
import Lumi.Components.Example (example)
import React.Basic (Component, JSX, createComponent, element, empty, toReactComponent)
import React.Basic.DOM as R
import Web.HTML.History (URL(..))

component :: Component (RouterProps ())
component = createComponent "TabExample"

docs :: JSX
docs = flip element {} $ withRouter $ toReactComponent identity component { render: \{ props } -> render props }
  where
    render props =
      column_
        [ h2_ "Demo 1"
        , example $
            R.div
              { style: R.css { maxWidth: "800px" }
              , children:
                  [ tabs
                      { currentLocation: URL $ "#" <> props.location.pathname <> props.location.search <> props.location.hash
                      , useHash: true
                      , navigate: Just \url ->
                          let
                            parts = urlParts url
                            newUrl = parts.path <> parts.query <> parts.hash.path <> parts.hash.query
                            newUrlNoHash = fromMaybe "" $ flip index 1 $ split (Pattern "#") newUrl
                          in
                            runEffectFn1 props.history.push $ URL $ newUrlNoHash
                      , queryKey: TabKey "demo1"
                      , style: R.css {}
                      , tabStyle: R.css {}
                      , selectedTabStyle: R.css {}
                      , tabs: 1 .. 10 <#> \i ->
                          let
                            label = "Tab with a long title " <> show i
                          in
                            { content: \_ -> empty
                            , id: TabId (toLower label)
                            , label
                            , count: (7*(i - 1) * (i - 1) `mod` 4) * 7 <$ guard (i `mod` 3 /= 1)
                            , testId: Nothing
                            }
                      }
                  ]
              }

        , h2_ "Demo 2"
        , example $
            tabs
              { currentLocation: URL $ "#" <> props.location.pathname <> props.location.search <> props.location.hash
              , useHash: true
              , navigate: Just \url ->
                  let
                    parts = urlParts url
                    newUrl = parts.path <> parts.query <> parts.hash.path <> parts.hash.query
                    newUrlNoHash = fromMaybe "" $ flip index 1 $ split (Pattern "#") newUrl
                  in
                    runEffectFn1 props.history.push $ URL $ newUrlNoHash
              , queryKey: TabKey "demo2"
              , style: R.css {}
              , tabStyle: R.css {}
              , selectedTabStyle: R.css {}
              , tabs: 1 .. 6 <#> \i ->
                  let
                    label = "Tab" <> show i
                  in
                    { content: \_ -> empty
                    , id: TabId (toLower label)
                    , label
                    , count: (i `div` 2) <$ guard (i `mod` 4 == 1)
                    , testId: Nothing
                    }
              }
        ]
