module Lumi.Components.Examples.EditableTable where

import Prelude

import Data.Array.NonEmpty (filter, fromArray, fromNonEmpty, length, snoc)
import Data.BigInt (fromInt, fromString, toNumber, toString)
import Data.Either (Either(..))
import Data.Foldable (sum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty ((:|))
import Data.Number (isNaN)
import Data.Number.Format (fixed, toStringWith)
import Global (readInt)
import Lumi.Components.Column (column_)
import Lumi.Components.EditableTable (editableTable)
import Lumi.Components.Input as Input
import Lumi.Components.Row (row_)
import Lumi.Components.Text (body, body_, nbsp, text)
import Lumi.Components.Example (example)
import React.Basic (Component, JSX, createComponent, make)
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler)

component :: Component Unit
component = createComponent "EditableTableExample"

docs :: JSX
docs = unit # make component
  { initialState:
      { rows: fromNonEmpty $
          { id: 0
          , description: "Boxes"
          , quantity: fromInt 1000
          , price: 1.32
          } :|
            [ { id: 1
              , description: "Tape"
              , quantity: fromInt 100
              , price: 0.23
              }
            ]
      }

  , render: \self ->
      column_
        [ example $
            editableTable
              { addLabel: "Add another row"
              , columns:
                  [ { label: "Descriptionxxx"
                    , renderCell: \row -> Input.input Input.text_
                        { value = row.description
                        , onChange = handler targetValue \value ->
                            updateRow self row { description = fromMaybe row.description value }
                        , style = R.css { width: "100%" }
                        }
                    }
                  , { label: "Quantity"
                    , renderCell: \row -> Input.input Input.number
                        { value = toString row.quantity
                        , onChange = handler targetValue \value ->
                            updateRow self row { quantity = fromMaybe row.quantity $ fromString =<< value }
                        }
                    }
                  , { label: "foobar"
                    , renderCell: \row -> R.text "hello"
                    }
                  , { label: "Price"
                    , renderCell: \row -> Input.input Input.number
                        { value = show row.price
                        , onChange = handler targetValue \value ->
                            updateRow self $ fromMaybe row do
                              value' <- readInt 10 <$> value
                              pure if isNaN value'
                                then row
                                else row { price = value' }
                        }
                    }
                  , { label: "Total"
                    , renderCell: \row -> R.text $ toMoneyString (calculateTotal row)
                    }
                  ]
              , maxRows: 5
              , onRowAdd: addRow self
              , onRowRemove: removeRow self
              , readonly: false
              , rowEq: eq
              , rows: Right self.state.rows
              , summary:
                  row_
                    [ text body
                        { children = [ R.text "Total:" ]
                        , style = R.css { fontWeight: "bold" }
                        }
                    , body_ nbsp
                    , body_ nbsp
                    , body_ nbsp
                    , body_ nbsp
                    , body_ $ toMoneyString $ sum $ calculateTotal <$> self.state.rows
                    ]
              }
        ]
  }
  where
    updateRow self row = do
      self.setState _
        { rows = self.state.rows <#> \r -> if r.id == row.id then row else r
        }

    addRow self = do
      self.setState _
        { rows = self.state.rows `snoc` emptyRow { id = length self.state.rows }
        }

    removeRow self row = do
      self.setState _
        { rows = case fromArray $ filter (not eq row) self.state.rows of
            Nothing -> self.state.rows
            Just a -> a
        }

    emptyRow =
      { id: -1
      , description: ""
      , quantity: fromInt 0
      , price: 0.0
      }

    calculateTotal row =
      toNumber row.quantity * row.price

    toMoneyString value =
      "$" <> toStringWith (fixed 2) value
