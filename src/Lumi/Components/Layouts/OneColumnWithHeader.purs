module Lumi.Components.Layouts.OneColumnWithHeader where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components.Text (h2, text)
import React.Basic.Classic (Component, JSX, createComponent, element, fragment, makeStateless)
import React.Basic.DOM (unsafeCreateDOMComponent)
import React.Basic.DOM as R

component :: Component LayoutProps
component = createComponent "OneColumnWithHeader"

type LayoutProps =
  { titleContent :: JSX
  , additionalHeaderContent :: JSX
  , actionContent :: JSX
  , mainContent :: JSX
  , sidebarContent :: Maybe JSX
  }

layout :: LayoutProps -> JSX
layout = makeStateless component render
  where
    render { titleContent, additionalHeaderContent, actionContent, mainContent, sidebarContent } =
      lumiLayout
        { children:
            [ lumiLayoutViewHead
              { children:
                  [ text $ h2
                      { children = [titleContent]
                      , style = R.css
                          { paddingBottom: "0"
                          , fontWeight: "normal"
                          , marginRight: "12px"
                          , flex: "none"
                          }
                      }
                  , R.span
                      { style: R.css
                          { overflow: "visible"
                          , flex: "1 1 70%"
                          , wordWrap: "break-word"
                          , padding: "0"
                          }
                      , children: [additionalHeaderContent]
                      }
                  , R.div
                      { style: R.css { flex: "1 1 10px" }
                      }
                  , R.div
                      { style: R.css { flex: "none" }
                      , children: [actionContent]
                      }
                  ]
              }
            , lumiLayoutViewBody
                { className: "view-body view-scroll"
                , children: case sidebarContent of
                    Nothing  ->
                      [ mainContent ]
                    Just sbc ->
                      [ R.div
                          { className: "row row-justify-between row-no-padding"
                          , children:
                              [ R.div { className: "column view-scroll", children: [ mainContent]}
                              , sidebarLayout { content: sbc }
                              ]
                          }
                      ]
                }
            ]
        }

    lumiLayout = element (unsafePerformEffect $ unsafeCreateDOMComponent "lumi-layout")
    lumiLayoutViewHead = element (unsafePerformEffect $ unsafeCreateDOMComponent "lumi-layout-view-head")
    lumiLayoutViewBody = element (unsafePerformEffect $ unsafeCreateDOMComponent "lumi-layout-view-body")

sidebarLayoutComponent :: Component SidebarLayoutProps
sidebarLayoutComponent = createComponent "Sidebar"

type SidebarLayoutProps =
  { content :: JSX
  }

sidebarLayout :: SidebarLayoutProps -> JSX
sidebarLayout = makeStateless sidebarLayoutComponent render where
  render { content } =
    fragment $
      [ R.div
         { style: R.css { "flex": "0 0 30%" }
         , className: "column bl view-scroll"
         , maxLength: 400
         , children: [
           R.div { className: "ppa", children: [ content ]}
           ]
         }
      ]
