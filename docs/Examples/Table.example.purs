module Lumi.Components.Examples.Table where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (un)
import Data.Nullable (notNull, null, toMaybe, toNullable)
import Effect.Console (log)
import Effect.Uncurried (mkEffectFn1, mkEffectFn2)
import Foreign (readString, unsafeToForeign)
import Foreign.Index (readProp)
import Lumi.Components.Column (column, column_)
import Lumi.Components.Example (example)
import Lumi.Components.Images (productThumb_)
import Lumi.Components.Input (input, text_)
import Lumi.Components.Link as Link
import Lumi.Components.Lockup (lockup)
import Lumi.Components.NativeSelect (defaults, nativeSelect)
import Lumi.Components.Size (Size(..))
import Lumi.Components.Table (ColumnName(..), SortString(..), table)
import Lumi.Components.Text (p_)
import React.Basic (Component, JSX, createComponent, make)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)
import Web.HTML.History (URL(..))

component :: Component Unit
component = createComponent "TableExample"

docs :: JSX
docs = unit # make component { initialState, render }
  where
    initialState =
      { sort: SortString "asc"
      , sortBy: Just (ColumnName "createdDate")
      , selected: ["10cms9", "0mf7w"]
      , selectedOption: ""
      , ex2Columns:
          [ { required: true
            , name: ColumnName "product-type"
            , label: notNull "Product type"
            , filterLabel: null
            , sortBy: notNull $ ColumnName "title"
            , style: css {}
            , hidden: false
            , sticky: false
            , renderCell: \rowData ->
                Link.link Link.defaults
                  { href = rowData.link
                  , text = R.text rowData.title
                  }
            }
          , { required: true
            , name: ColumnName "created-date"
            , label: notNull "Created date"
            , filterLabel: null
            , sortBy: notNull $ ColumnName "createdDate"
            , style: css {}
            , hidden: false
            , sticky: false
            , renderCell: \rowData -> R.text rowData.createdDate
            }
          ]
      , ex3Columns:
          [ { required: true
            , name: ColumnName "input"
            , label: notNull "Input"
            , filterLabel: null
            , sortBy: null
            , style: css {}
            , hidden: false
            , sticky: false
            , renderCell: \rowData ->
                input text_ { placeholder = "Placeholder..." }
            }
          ]
      }

    render self =
      column_
        [ p_ "*Right click on table header to see FilterDropdownMenu"
        , example $
            column
              { style: css { alignSelf: "stretch" , height: 150, width: 400 }
              , children:
                  [ table
                      { name: "Items"
                      , dropdownMenu: true
                      , sortable: true
                      , sort: toNullable (Just self.state.sort)
                      , sortBy: toNullable self.state.sortBy
                      , updateSort: mkEffectFn2 \sort sortBy -> do
                          self.setState _ { sort = sort, sortBy = toMaybe sortBy }
                      , selectable: true
                      , selected: null -- notNull self.state.selected -- use this to test parent-controlled selection
                      , onSelect: mkEffectFn1 mempty -- \selected -> self.setState _ { selected = selected }
                      , rows: (if self.state.sort == SortString "desc" then Array.reverse else identity)
                          let tableFieldSort = comparing \row -> do
                                ColumnName sortByCol <- self.state.sortBy
                                case runExcept $ readString =<< readProp sortByCol (unsafeToForeign row) of
                                  Right value -> Just value
                                  _           -> Nothing
                          in Array.sortBy tableFieldSort tableData
                      , getRowKey: _.id
                      , rowEq: eq
                      , onNavigate: mkEffectFn1 \href ->
                          log $ "navigate to: " <> un URL href
                      , variant: null
                      , primaryColumn: notNull
                          { name: ColumnName "product-lockup"
                          , label: null
                          , filterLabel: notNull "Product lockup"
                          , sortBy: null
                          , style: css {}
                          , getLink: notNull _.link
                          , renderCell: \rowData ->
                              lockup
                                { image: Just $ productThumb_ { image: R.img { src: rowData.imgSrc }, size: Small }
                                , title: R.text rowData.title
                                , subtitle: Just $ R.text rowData.dimensions
                                }
                          , sticky: true
                          }
                      , columns:
                          [ { required: true
                            , name: ColumnName "product-type"
                            , label: notNull "Product type"
                            , filterLabel: null
                            , sortBy: notNull $ ColumnName "title"
                            , style: css {}
                            , hidden: false
                            , sticky: false
                            , renderCell: \rowData ->
                                Link.link Link.defaults
                                  { href = rowData.link
                                  , className = pure "action"
                                  , text = R.text rowData.title
                                  }
                            }
                          , { required: true
                            , name: ColumnName "created-date"
                            , label: notNull "Created date"
                            , filterLabel: null
                            , sortBy: notNull $ ColumnName "createdDate"
                            , style: css {}
                            , hidden: false
                            , sticky: false
                            , renderCell: \rowData -> R.text rowData.createdDate
                            }
                          ]
                      , onColumnChange: null
                      }
                  ]
              }

        , p_ "*Does not include FilterDropdownMenu"
        , example $
            column
              { style: css { alignSelf: "stretch" }
              , children:
                  [ table
                      { name: "Items"
                      , dropdownMenu: false
                      , sortable: true
                      , sort: notNull self.state.sort
                      , sortBy: toNullable self.state.sortBy
                      , updateSort: mkEffectFn2 \sort sortBy -> do
                          self.setState _ { sort = sort, sortBy = toMaybe sortBy }
                      , selectable: true
                      , selected: notNull self.state.selected -- use this to test parent-controlled selection
                      , onSelect: mkEffectFn1 \selected -> self.setState _ { selected = selected }
                      , rows: (if self.state.sort == SortString "desc" then Array.reverse else identity)
                          let tableFieldSort = comparing \row -> do
                                ColumnName sortByCol <- self.state.sortBy
                                case runExcept $ readString =<< readProp sortByCol (unsafeToForeign row) of
                                  Right value -> Just value
                                  _           -> Nothing
                          in Array.sortBy tableFieldSort tableData
                      , getRowKey: _.id
                      , rowEq: eq
                      , onNavigate: mkEffectFn1 \href ->
                          log $ "navigate to: " <> un URL href
                      , variant: notNull "compact"
                      , primaryColumn: notNull
                          { name: ColumnName "product-title"
                          , label: notNull "Items"
                          , filterLabel: notNull "Product title"
                          , sortBy: null
                          , style: css {}
                          , getLink: null -- notNull _.link
                          , renderCell: R.text <<< _.title
                          , sticky: false
                          }
                      , columns: self.state.ex2Columns
                      , onColumnChange: notNull $ mkEffectFn1 \columns ->
                          self.setState _ { ex2Columns = columns }
                      }
                  ]
              }

        , p_ "*Non selectable variant w/ inputs & selects"
        , example $
            column
              { style: css { alignSelf: "stretch" }
              , children:
                  [ table
                      { name: "Items"
                      , dropdownMenu: true
                      , sortable: true
                      , sort: notNull self.state.sort
                      , sortBy: toNullable self.state.sortBy
                      , updateSort: mkEffectFn2 \sort sortBy -> do
                          self.setState _ { sort = sort, sortBy = toMaybe sortBy }
                      , selectable: false
                      , selected: null
                      , onSelect: mkEffectFn1 \selected -> self.setState _ { selected = selected }
                      , rows: (if self.state.sort == SortString "desc" then Array.reverse else identity)
                          let tableFieldSort = comparing \row -> do
                                ColumnName sortByCol <- self.state.sortBy
                                case runExcept $ readString =<< readProp sortByCol (unsafeToForeign row) of
                                  Right value -> Just value
                                  _           -> Nothing
                          in Array.sortBy tableFieldSort tableData
                      , getRowKey: _.id
                      , rowEq: eq
                      , onNavigate: mkEffectFn1 \href ->
                          log $ "navigate to: " <> un URL href
                      , variant: notNull "compact"
                      , primaryColumn: notNull
                          { name: ColumnName "product-title"
                          , label: notNull "Items"
                          , filterLabel: notNull "Product title"
                          , sortBy: null
                          , style: css {}
                          , getLink: notNull _.link
                          , renderCell: R.text <<< _.title
                          , sticky: false
                          }
                      , columns:
                          Array.concat
                            [ self.state.ex2Columns
                            , self.state.ex3Columns
                            , [ { required: true
                                , name: ColumnName "select"
                                , label: notNull "Select"
                                , filterLabel: null
                                , sortBy: null
                                , style: css {}
                                , hidden: false
                                , sticky: false
                                , renderCell: \rowData ->
                                    nativeSelect defaults
                                        { options =
                                            [ { label: "Select your car...", value: "" }
                                            , { label: "Volvo", value: "volvo" }
                                            , { label: "Saab", value: "saab" }
                                            , { label: "Mercedes", value: "mercedes" }
                                            , { label: "Audi", value: "audi" }
                                            ]
                                        , onChange = handler targetValue \value ->
                                                      self.setState _ { selectedOption = fromMaybe "" value }
                                        , value = self.state.selectedOption
                                        }
                                }
                              ]
                            ]
                      , onColumnChange: notNull $ mkEffectFn1 \columns ->
                          self.setState _ { ex2Columns = columns }
                      }
                  ]
              }
        ]

    tableData =
      [ { rowLink: "http://google.com"
        , imgSrc: "https://s3.amazonaws.com/lumi-flapjack-staging/mockups/9e7f08b801e6bb3a428ef72e93c49fe5.jpg"
        , title: "Gummed Paper Tape"
        , link: URL "/#/color"
        , dimensions: "10\" x 12\" x 13\""
        , createdDate: "2/12/2018"
        , id: "f0pkl1"
        }
      , { rowLink: "http://nytimes.com"
        , imgSrc: "https://s3.amazonaws.com/lumi-flapjack-staging/mockups/4c5fe52b40a56afbfde417189677f30b.jpg"
        , title: "Packing Tape"
        , link: URL "/#/text"
        , dimensions: "2\" Lightweight Poly"
        , createdDate: "2/01/2018"
        , id: "10cms9"
        }
      , { rowLink: "http://foobar.com"
        , imgSrc: "https://s3.amazonaws.com/lumi-flapjack-staging/mockups/4c5fe52b40a56afbfde417189677f30b.jpg"
        , title: "Packing Tape"
        , link: URL "/#/text"
        , dimensions: "2\" Lightweight Poly"
        , createdDate: "2/01/2018"
        , id: "f2982"
        }
      , { rowLink: "http://facebook.com"
        , imgSrc: "https://s3.amazonaws.com/lumi-flapjack-staging/mockups/14fdd720a7dc2373cc833eb8dd471784.jpg"
        , title: "Poly Mailers"
        , link: URL "/#/input"
        , dimensions: "7.50\" x 10.50\""
        , createdDate: "1/15/2018"
        , id: "0mf7w"
        }
      ]
