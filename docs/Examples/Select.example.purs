module Lumi.Components.Examples.Select where

import Prelude

import Data.Array (filter)
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), contains, toLower)
import Effect.Aff (Milliseconds(..), delay)
import Lumi.Components.Column (column, column_)
import Lumi.Components.Example (example)
import Lumi.Components.Input as Input
import Lumi.Components.LabeledField (labeledField, RequiredField(..))
import Lumi.Components.Select (asyncMultiSelect, asyncSingleSelect, multiSelect, singleSelect)
import Lumi.Components.Text (h2_)
import React.Basic (Component, JSX, createComponent, make)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetChecked)
import React.Basic.Events (handler)

component :: Component Unit
component = createComponent "SelectExample"

type Opt = { label :: String, value :: String }

type State =
  { disabled :: Boolean
  , example1 :: Maybe Opt
  , example2 :: Array Opt
  , example3 :: Maybe Opt
  , example4 :: Array Opt
  }

docs :: JSX
docs = unit # make component { initialState, render }
  where
    initialState =
      { disabled: false
      , example1: Nothing
      , example2: []
      , example3: Nothing
      , example4: []
      }

    defaultOptions =
      [ { label: "Volvo", value: "volvo" }
      , { label: "Saab", value: "saab" }
      , { label: "Mercedes", value: "mercedes" }
      , { label: "Audi", value: "audi" }
      , { label: "BMW", value: "bmw" }
      , { label: "Chevy", value: "chevy" }
      , { label: "Ford", value: "ford" }
      , { label: "Tesla", value: "tesla" }
      , { label: "Fiat", value: "fiat" }
      , { label: "Honda", value: "honda" }
      , { label: "Toyota", value: "toyota" }
      , { label: "Porsche", value: "porsche" }
      , { label: "Ferrari", value: "ferrari" }
      , { label: "Subaru", value: "subaru" }
      , { label: "Dodge", value: "dodge" }
      , { label: "Chrysler", value: "chrysler" }
      , { label: "Cadillac", value: "cadillac" }
      ]

    render self@{ state: { disabled, example1, example2, example3, example4 } } =
      column_
        [ column
            { style: css { maxWidth: 500, padding: "20px 0" }
            , children:
                [ labeledField
                    { label: R.text "Disabled"
                    , value: Input.input Input.switch
                        { checked = if disabled then Input.On else Input.Off
                        , onChange = handler targetChecked \v -> self.setState _ { disabled = fromMaybe false v }
                        }
                    , validationError: Nothing
                    , required: Neither
                    , forceTopLabel: false
                    , style: css {}
                    }
                ]
            }

        , h2_ "SingleSelect"
        , example $
            column
              { style: css { alignSelf: "stretch" }
              , children:
                  [ singleSelect
                      { value: example1
                      , options: defaultOptions
                      , optionSort: Nothing
                      , onChange: \v -> self.setState _ { example1 = v }
                      , className: ""
                      , style: css {}
                      , searchable: true
                      , id: ""
                      , name: ""
                      , noResultsText: "No results"
                      , placeholder: "Select a value..."
                      , disabled
                      , loading: false
                      , optionRenderer: R.text <<< _.label
                      , toSelectOption: identity
                      }
                  ]
              }

        , h2_ "MultiSelect"
        , example $
            column
              { style: css { alignSelf: "stretch" }
              , children:
                  [ multiSelect
                      { value: example2
                      , options: defaultOptions
                      , optionSort: Nothing
                      , onChange: \v -> self.setState _ { example2 = v }
                      , className: ""
                      , style: css {}
                      , searchable: true
                      , id: ""
                      , name: ""
                      , noResultsText: "No results"
                      , placeholder: "Select a value..."
                      , disabled
                      , loading: false
                      , optionRenderer: R.text <<< _.label
                      , toSelectOption: identity
                      }
                  ]
              }

        , h2_ "AsyncSingleSelect"
        , example $
            column
              { style: css { alignSelf: "stretch" }
              , children:
                  [ asyncSingleSelect
                      { value: example3
                      , loadOptions: \terms -> do
                          delay (Milliseconds 200.0)
                          pure $
                            filter
                              (\opt -> contains (Pattern $ toLower terms) (toLower opt.label))
                              defaultOptions
                      , optionSort: Nothing
                      , onChange: \v -> self.setState _ { example3 = v }
                      , className: ""
                      , style: css {}
                      , searchable: true
                      , id: ""
                      , name: ""
                      , noResultsText: "No results"
                      , placeholder: "Select a value..."
                      , disabled
                      , loading: false
                      , optionRenderer: R.text <<< _.label
                      , toSelectOption: identity
                      }
                  ]
              }

        , h2_ "AsyncMultiSelect"
        , example $
            column
              { style: css { alignSelf: "stretch" }
              , children:
                  [ asyncMultiSelect
                      { value: example4
                      , loadOptions: \terms -> do
                          delay (Milliseconds 200.0)
                          pure $
                            filter
                              (\opt -> contains (Pattern $ toLower terms) (toLower opt.label))
                              defaultOptions
                      , optionSort: Nothing
                      , onChange: \v -> self.setState _ { example4 = v }
                      , className: ""
                      , style: css {}
                      , searchable: true
                      , id: ""
                      , name: ""
                      , noResultsText: "No results"
                      , placeholder: "Select a value..."
                      , disabled
                      , loading: false
                      , optionRenderer: R.text <<< _.label
                      , toSelectOption: identity
                      }
                  ]
              }

        , h2_ "Truncating long values"
        , example $
            column
              { style: css { alignSelf: "stretch", maxWidth: "300px" }
              , children:
                  [ let
                      options =
                        [ "80104 Somestreet Rd Ste 1124, San Diego, California, United States"
                        , "1211 Blah blah Blvd, Charlotrte, North Carolina, United States"
                        , "3031 Fall Ave, Colorado Springs, Colorado, United States"
                        , "12345 Lorem Ave, Sioux Falls, South Dakota, United States"
                        , "100 Phillips Ave, Madill, Oklahoma, United States"
                        , "2453 Frosty Lane, Binghamton, New York, United States"
                        , "4566 Random Ave, Grand Forks, North Dakota, United States"
                        , "4702 Hart Rd, Northbrook, Illinois, United States"
                        , "567 Fake Way, San Diego, California, United States"
                        ]
                    in
                      singleSelect
                        { value: Array.head options
                        , options
                        , optionSort: Nothing
                        , onChange: mempty
                        , className: ""
                        , style: css {}
                        , searchable: true
                        , id: ""
                        , name: ""
                        , noResultsText: "No results"
                        , placeholder: "Select a value..."
                        , disabled
                        , loading: false
                        , optionRenderer: R.text
                        , toSelectOption: \a -> { value: a, label: a }
                        }
                  ]
              }

        ]
