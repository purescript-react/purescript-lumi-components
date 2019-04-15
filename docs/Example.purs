module Lumi.Components.Example
  ( exampleStyleToggle
  , example
  , exampleCode
  ) where

import Prelude

import Color (cssStringHSLA)
import Data.Array (index)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), split, toLower)
import Effect.Uncurried (runEffectFn1)
import Lumi.Components.Color (colors)
import Lumi.Components.Column (column)
import Lumi.Components.Tab (TabId(..), TabKey(..), tabs, urlParts)
import Lumi.Components.Text (body, text)
import Lumi.Components.Utility.ReactRouter (RouterProps, withRouter)
import React.Basic (Component, JSX, createComponent, element, empty, toReactComponent)
import React.Basic.DOM (css, mergeStyles)
import React.Basic.DOM as R
import Web.HTML.History (URL(..))

type Lang = String

data ExampleTabs = Demo | Boundaries

derive instance eqExampleTabs :: Eq ExampleTabs

renderTabLabel :: ExampleTabs -> String
renderTabLabel = case _ of
  Demo -> "Demo"
  Boundaries -> "Boundaries"

parseTabLabel :: String -> Maybe ExampleTabs
parseTabLabel = case _ of
  "Demo" -> Just Demo
  "Boundaries" -> Just Boundaries
  _ -> Nothing

exampleStyleToggle :: JSX
exampleStyleToggle = (\c -> element c { children: empty }) (withRouter $ toReactComponent identity component { render: render <<< _.props })
  where
    render props =
      tabs
        { currentLocation: getCurrentLocation props
        , navigate: Just \url ->
            let
              parts = urlParts url
              newUrl = parts.path <> parts.query <> parts.hash.path <> parts.hash.query
              newUrlNoHash = fromMaybe "" $ flip index 1 $ split (Pattern "#") newUrl
            in
              runEffectFn1 props.history.push $ URL $ newUrlNoHash
        , queryKey: TabKey "example"
        , selectedTabStyle: css { boxShadow: "none" }
        , style: css { boxShadow: "none" }
        , tabStyle: css {}
        , tabs:
            [ mkTab Demo
            , mkTab Boundaries
            ]
        , useHash: true
        }

    mkTab tab =
      { content: \_ -> empty
      , id: TabId $ toLower $ renderTabLabel tab
      , label: renderTabLabel tab
      , count: Nothing
      , testId: Nothing
      }

component :: Component (RouterProps ( children :: JSX ))
component = createComponent "Example"

example :: JSX -> JSX
example = (\c children -> element c { children }) (withRouter $ toReactComponent identity component { render: render <<< _.props })
  where
    render props =
      tabs
        { currentLocation: getCurrentLocation props
        , navigate: Nothing
        , queryKey: TabKey "example"
        , selectedTabStyle: css {}
        , style: css { height: 0, boxShadow: "none" }
        , tabStyle: css { display: "none" }
        , tabs:
            [ mkTab Demo props.children
            , mkTab Boundaries props.children
            ]
        , useHash: true
        }

    mkTab tab children =
      { content: \_ -> tabContent tab children
      , id: TabId $ toLower $ renderTabLabel tab
      , label: renderTabLabel tab
      , count: Nothing
      , testId: Nothing
      }

    tabContent tab children =
      outerRow tab
        [ innerRow tab
            case tab of
              Demo -> children
              Boundaries -> children
        ]

    outerRow tab children =
      column
        { children
        , style:
            case tab of
              Demo -> outerRowStyle
              Boundaries -> mergeStyles
                [ outerRowStyle
                , outerRowStyle_Boundaries
                ]
        }

    outerRowStyle = css
      { justifyContent: "center"
      , boxSizing: "border-box"
      , marginBottom: "32px"
      }

    outerRowStyle_Boundaries = css
      { backgroundColor: "#f7f6f4"
      }

    innerRow tab child =
      column
        { style: mergeStyles
            [ innerRowStyle
            , case tab of
                Boundaries -> innerRowStyle_Boundaries
                _ -> css {}
            ]
        , children: [ child ]
        }

    innerRowStyle = css
      { margin: 20
      , background: cssStringHSLA colors.white
      , border: "1px solid rgba(0, 0, 255, 0)"
      , alignItems: "flex-start"
      }

    innerRowStyle_Boundaries = css
      { border: "1px solid rgba(0, 0, 255, 0.6)"
      , boxShadow: "0 0 40px rgba(0, 0, 255, 0.4)"
      }

getCurrentLocation
  :: forall props
   . { location :: { pathname :: String
                   , search :: String
                   , hash :: String
                   }
     | props
     }
  -> URL
getCurrentLocation props = URL $ "#" <> props.location.pathname <> props.location.search <> props.location.hash

exampleCode :: String -> JSX
exampleCode code = text body { children = [ R.pre_ [ R.code_ [ R.text code ] ] ] }
