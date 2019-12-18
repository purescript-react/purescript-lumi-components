module Lumi.Components.Examples.Layouts where

import Prelude

import Data.Array (index)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un)
import Data.NonEmpty ((:|))
import Data.Nullable (notNull, toNullable)
import Data.String (Pattern(..), split)
import Effect.Console (log)
import Effect.Uncurried (mkEffectFn1, mkEffectFn2, runEffectFn1)
import Lumi.Components.Button (button, defaults)
import Lumi.Components.Column (column_)
import Lumi.Components.Example (example)
import Lumi.Components.Images (avatar_, productThumb_)
import Lumi.Components.Layouts.Centered as Centered
import Lumi.Components.Layouts.OneColumnWithHeader as OneColumnWithHeader
import Lumi.Components.Layouts.Tabs (tabLayout)
import Lumi.Components.Link as Link
import Lumi.Components.List (compactList, list, defaultList)
import Lumi.Components.Lockup (lockup)
import Lumi.Components.Size (Size(..))
import Lumi.Components.Tab (TabId(..), TabKey(..), urlParts)
import Lumi.Components.Table (ColumnName(..), table)
import Lumi.Components.Text (body_, h2_, p_, sectionHeader_)
import Lumi.Components.Utility.ReactRouter (RouterProps, withRouter)
import React.Basic (Component, JSX, createComponent, element, toReactComponent)
import React.Basic.DOM as R
import Web.HTML.History (URL(..))

component :: Component (RouterProps ())
component = createComponent "LayoutExample"

docs :: JSX
docs = (\c -> element c {}) $ withRouter $ toReactComponent identity component { render: \{ props } -> render props }
  where
  render props =
    column_
      [ h2_ "Centered layout"
      , example
          $ Centered.layout
          $ R.div
              { children:
                [ h2_ "lorem ipsum"
                ]
              }
      , h2_ "Tabs layout"
      , example
          $ tabLayout
          $ { currentLocation: URL $ "#" <> props.location.pathname <> props.location.search <> props.location.hash
            , useHash: true
            , navigate:
              Just \url ->
                let
                  parts = urlParts url

                  newUrl = parts.path <> parts.query <> parts.hash.path <> parts.hash.query

                  newUrlNoHash = fromMaybe "" $ flip index 1 $ split (Pattern "#") newUrl
                in
                  runEffectFn1 props.history.push $ URL $ newUrlNoHash
            , queryKey: TabKey "layout-demo-1"
            , tabs:
              { id: TabId "details"
              , label: "Details"
              , count: Nothing
              , content:
                \_ ->
                  R.div
                    { children: [ p_ "lorem ipsum" ]
                    , style: R.css { padding: "5px" }
                    }
              }
                :| [ { id: TabId "shipment"
                    , label: "Shipment"
                    , count: Just 3
                    , content:
                      \_ ->
                        R.div
                          { children: [ p_ "dolor sit amet" ]
                          , style: R.css { padding: "5px" }
                          }
                    }
                  ]
            }
      , h2_ "Centered layout (full width)"
      , example
          $ Centered.layoutFullWidth
          $ R.div
              { children:
                [ sectionHeader_ "Details"
                , list
                    compactList
                      { rows = detailsTabData
                      }
                , sectionHeader_ "Foobar"
                , list
                    compactList
                      { rows = detailsTabData
                      }
                ]
              }
      , h2_ "One Column w/ Header layout"
      , example
          $ OneColumnWithHeader.layout
          $ { titleContent: R.text "Title"
            , additionalHeaderContent: p_ "subheader"
            , actionContent: button defaults { title = "Action" }
            , mainContent: overviewTable
            , sidebarContent: Nothing
            }
      , h2_ "One Column w/ Header layout"
      , example
          $ OneColumnWithHeader.layout
          $ { titleContent: R.text "Title"
            , additionalHeaderContent: p_ "subheader"
            , actionContent: button defaults { title = "Action" }
            , mainContent:
              R.div
                { children:
                  [ tabLayout
                      { currentLocation: URL $ "#" <> props.location.pathname <> props.location.search <> props.location.hash
                      , useHash: true
                      , navigate:
                        Just \url ->
                          let
                            parts = urlParts url

                            newUrl = parts.path <> parts.query <> parts.hash.path <> parts.hash.query

                            newUrlNoHash = fromMaybe "" $ flip index 1 $ split (Pattern "#") newUrl
                          in
                            runEffectFn1 props.history.push $ URL $ newUrlNoHash
                      , queryKey: TabKey "layout-demo-2"
                      , tabs:
                        { id: TabId "foo"
                        , label: "Items"
                        , count: Just 2
                        , content: \_ -> itemsTab
                        }
                          :| [ { id: TabId "bar"
                              , label: "Details"
                              , count: Nothing
                              , content: \_ -> detailsTab
                              }
                            ]
                      }
                  ]
                , style: R.css {}
                }
            , sidebarContent: Nothing
            }
      ]

  itemsTab =
    Centered.layout
      $ R.div
          { children:
            [ sectionHeader_ "Items"
            , list
                defaultList
                  { rows = itemsTabData
                  }
            , sectionHeader_ "BarFoo"
            , list
                defaultList
                  { rows = itemsTabData
                  }
            ]
          }

  itemsTabData =
    [ [ body_ "User"
      , lockup
          { title: R.text "Flexo R."
          , subtitle: Just $ R.text "Lumi"
          , image:
            Just
              $ avatar_
                  { size: Large
                  , image:
                    R.img
                      { src: "https://s3.amazonaws.com/lumi-flapjack-staging/mockups/9e7f08b801e6bb3a428ef72e93c49fe5.jpg"
                      }
                  }
          }
      ]
    , [ body_ "ID"
      , Link.link
          Link.defaults
            { href = URL "/"
            , text = body_ "12345"
            , style = R.css {}
            }
      ]
    , [ body_ "Created"
      , body_ "2018-09-02"
      ]
    ]

  detailsTab =
    Centered.layout
      $ R.div
          { children:
            [ sectionHeader_ "Details"
            , list
                defaultList
                  { rows = detailsTabData
                  }
            , sectionHeader_ "Foobar"
            , list
                defaultList
                  { rows = detailsTabData
                  }
            ]
          }

  detailsTabData =
    [ [ body_ "ID"
      , Link.link
          Link.defaults
            { href = URL "/1234"
            , text = body_ "1234"
            , style = R.css {}
            }
      ]
    , [ body_ "Created"
      , body_ "2018-09-02"
      ]
    ]

  overviewTable =
    table
      { name: "Items"
      , dropdownMenu: true
      , sortable: true
      , sort: toNullable Nothing
      , sortBy: toNullable Nothing
      , updateSort:
        mkEffectFn2 \sort sortBy -> do
          log "update sort click"
      , selectable: true
      , selected: toNullable Nothing
      , onSelect: mkEffectFn1 (log <<< show)
      , rows: overviewTableData
      , getRowKey: _.id
      , rowEq: eq
      , onNavigate:
        mkEffectFn1 \href ->
          log $ "Should navigate to: " <> un URL href
      , variant: toNullable Nothing
      , primaryColumn:
        toNullable
          $ Just
              { name: ColumnName "product"
              , label: toNullable $ Just "Product title"
              , filterLabel: toNullable $ Just "Product title"
              , sortBy: toNullable Nothing
              , style: R.css {}
              , getLink: notNull _.link
              , sticky: false
              , renderCell:
                \rowData ->
                  lockup
                    { image: Just $ productThumb_ { size: Small, image: R.img { src: rowData.imgSrc } }
                    , title: R.text rowData.title
                    , subtitle: Nothing
                    }
              }
      , columns:
        [ { required: true
          , name: ColumnName "product-type"
          , label: toNullable $ Just "Product type"
          , filterLabel: toNullable Nothing
          , sortBy: toNullable $ Just $ ColumnName "title"
          , style: R.css {}
          , hidden: false
          , sticky: false
          , renderCell:
            \rowData ->
              Link.link
                Link.defaults
                  { href = rowData.link
                  , text = R.text rowData.title
                  }
          }
        , { required: true
          , name: ColumnName "created-date"
          , label: toNullable $ Just "Created date"
          , filterLabel: toNullable Nothing
          , sortBy: toNullable $ Just $ ColumnName "createdDate"
          , style: R.css {}
          , hidden: false
          , sticky: false
          , renderCell: \rowData -> R.text rowData.createdDate
          }
        ]
      , onColumnChange: toNullable Nothing
      }

  overviewTableData =
    [ { rowLink: "/#/color"
      , imgSrc: "https://s3.amazonaws.com/lumi-flapjack-staging/mockups/9e7f08b801e6bb3a428ef72e93c49fe5.jpg"
      , title: "Gummed Paper Tape"
      , link: URL "/#/color"
      , dimensions: "10\" x 12\" x 13\""
      , createdDate: "2018-02-12"
      , id: "123"
      }
    , { rowLink: "/#/text"
      , imgSrc: "https://s3.amazonaws.com/lumi-flapjack-staging/mockups/4c5fe52b40a56afbfde417189677f30b.jpg"
      , title: "Packing Tape Packing Tape Packing Tape Packing Tape Packing Tape Packing Tape Packing Tape Packing Tape"
      , link: URL "/#/text"
      , dimensions: "2\" Lightweight Poly"
      , createdDate: "2018-02-12"
      , id: "456"
      }
    , { rowLink: "/#/input"
      , imgSrc: "https://s3.amazonaws.com/lumi-flapjack-staging/mockups/14fdd720a7dc2373cc833eb8dd471784.jpg"
      , title: "Poly Mailers"
      , link: URL "/#/input"
      , dimensions: "7.50\" x 10.50\""
      , createdDate: "2018-02-12"
      , id: "789"
      }
    ]
