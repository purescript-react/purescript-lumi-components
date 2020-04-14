module Lumi.Components.Examples.EditableTable where

import Prelude

import Data.Array.NonEmpty (filter, fromArray, fromNonEmpty, length, snoc)
import Data.BigInt (fromInt, fromString, toNumber, toString)
import Data.Either (Either(..))
import Data.Foldable (sum, traverse_)
import Data.Foldable as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty ((:|))
import Data.Nullable as Nullable
import Data.Number (isNaN)
import Data.Number.Format (fixed, toStringWith)
import Effect.Console (log)
import Global (readInt)
import Lumi.Components.Column (column_)
import Lumi.Components.DropdownButton (dropdownIcon, dropdownIconDefaults)
import Lumi.Components.EditableTable (editableTable, editableTableDefaults)
import Lumi.Components.Example (example)
import Lumi.Components.Input (CheckboxState(..), alignToInput, input, switch)
import Lumi.Components.Input as Input
import Lumi.Components.Link as Link
import Lumi.Components.Row (row_)
import Lumi.Components.Spacing (Space(..), hspace)
import Lumi.Components.Text (body, body_, nbsp, p_, text)
import React.Basic (Component, JSX, createComponent, make)
import React.Basic.DOM as R
import React.Basic.DOM.Events (stopPropagation, targetChecked, targetValue)
import React.Basic.Events (handler)
import React.Basic.Events (handler) as Events

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
      , customRemoveCell: false
      }

  , render: \self ->
      column_ $ pure $ example $ column_
        [ row_
            [ alignToInput $ body_ "Custom remove cell?"
            , input switch
                { checked = if self.state.customRemoveCell then On else Off
                , onChange = Events.handler (stopPropagation >>> targetChecked)
                               (traverse_ (\checked -> self.setState (_ { customRemoveCell = checked })))
                }
            ]
        , editableTable
            { addLabel: "Add another row"
            , columns:
                [ { label: "Description"
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
            , removeCell: removeCell self
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

    removeCell self =
      if self.state.customRemoveCell
        then \onRemove row ->
          row_
            [ hspace S16
            , dropdownIcon dropdownIconDefaults
                { alignment = Nullable.notNull "right"
                , content = \closeSelf -> R.div
                    { style: R.css { width: "328px", padding: "12px" }
                    , children:
                        [ Link.link Link.defaults
                            { className = pure "lumi-dropdown-menu-item"
                            , text = p_ "Do something with this row"
                            , navigate = Just do
                                closeSelf
                                log $ "Did something: " <> show row
                            }
                        , onRemove # Array.foldMap \onRemove' ->
                            Link.link Link.defaults
                              { className = pure "lumi-dropdown-menu-item"
                              , text = p_ "Remove this row"
                              , navigate = Just do
                                  closeSelf
                                  onRemove' row
                              }
                        ]
                    }
                }
            ]
        else editableTableDefaults.removeCell
