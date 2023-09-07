module Lumi.Components.Form.Table
  ( TableFormBuilder
  , revalidate
  , editableTable
  , nonEmptyEditableTable
  , defaultRowMenu
  , column
  , column_
  , infoColumn
  , infoColumn_
  , withProps
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Foldable (foldMap, intercalate)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens.Index (ix)
import Data.Maybe (Maybe, fromMaybe, isNothing, maybe)
import Data.Monoid (guard)
import Data.Newtype (class Newtype, un)
import Data.Nullable as Nullable
import Data.Traversable (for_, traverse, traverse_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Lumi.Components.Column as Column
import Lumi.Components.EditableTable as EditableTable
import Lumi.Components.Form (static)
import Lumi.Components.Form.Internal (FormBuilder, FormBuilder'(..), Tree(..), Forest, formBuilder)
import Lumi.Components.LabeledField (ValidationMessage(..))
import Lumi.Components.Orientation (Orientation(..))
import Lumi.Components.Row as Row
import Lumi.Components.Spacing (Space(..), hspace, vspace)
import Lumi.Components.Text as T
import React.Basic.Classic (JSX, keyed)
import React.Basic.DOM as R
import Unsafe.Reference (unsafeRefEq)

-- | An applicative functor used to build editable tables out of `FormBuilder`s.
-- | `FormBuilder`s can be turned into `TableFormBuilder`s with the `column`
-- | function. `TableFormBuilder`s can, then, be used inside forms with the
-- | `editableTable` function.
newtype TableFormBuilder props row result =
  TableFormBuilder
    ( props
      -> { columns :: Array
             { label :: String
             , render :: row -> ((row -> row) -> Effect Unit) -> JSX
             }
         , infoColumns :: Array
             { render :: row -> ((row -> row) -> Effect Unit) -> JSX
             }
         , validate :: row -> Maybe result
         }
    )

derive instance newtypeTableFormBuilder :: Newtype (TableFormBuilder props row a) _
derive instance functorTableFormBuilder :: Functor (TableFormBuilder props row)
instance applyTableFormBuilder :: Apply (TableFormBuilder props row) where
  apply (TableFormBuilder ff) (TableFormBuilder fa) =
    TableFormBuilder \props ->
      let
        { columns: columnsF, infoColumns: infoColsF, validate: validateF } = ff props
        { columns: columnsA, infoColumns: infoColsA, validate: validateA } = fa props
      in
        { columns: columnsF <> columnsA
        , infoColumns: infoColsF <> infoColsA
        , validate: \row -> validateF row <*> validateA row
        }
instance applicativeTableFormBuilder :: Applicative (TableFormBuilder props row) where
  pure a =
    TableFormBuilder \_ ->
      { columns: []
      , infoColumns: []
      , validate: \_ -> pure a
      }

-- | Revalidate the table form, in order to display error messages or create
-- | a validated result.
revalidate
  :: forall props row result
   . TableFormBuilder props row result
  -> props
  -> row
  -> Maybe result
revalidate form props row = (un TableFormBuilder form props).validate row

-- | A `TableFormBuilder` makes a `FormBuilder` for an array where each row has
-- | columns defined by it.
editableTable
  :: forall props row result
   . { addLabel :: String
       -- | Controls the action that is performed when the button for adding a
       -- | new row is clicked. If this is `Nothing`, the button is not
       -- | displayed. The async effect wrapped in `Maybe` produces the new row
       -- | that will be inserted in the table, and, if it's result is
       -- | `Nothing`, then no rows will be added.
     , addRow :: Maybe (Aff (Maybe row))
     , formBuilder :: TableFormBuilder { readonly :: Boolean | props } row result
     , maxRows :: Int
       -- | Controls what is displayed in the last cell of an editable table row,
       -- | providing access to callbacks that delete or update the current row.
     , rowMenu
        :: { remove :: Maybe (Effect Unit)
           , update :: (row -> row) -> Effect Unit
           }
        -> row
        -> Maybe result
        -> JSX
     , summary
        :: Array row
        -> Maybe (Array result)
        -> JSX
     }
  -> FormBuilder
      { readonly :: Boolean | props }
      (Array row)
      (Array result)
editableTable { addLabel, addRow, formBuilder: builder, maxRows, rowMenu, summary } =
  formBuilder \props rows ->
    let
      { columns, infoColumns, validate } = (un TableFormBuilder builder) props
      validateRows = traverse validate rows
    in
      { edit: \onChange ->
          EditableTable.editableTable
            { addLabel
            , maxRows
            , readonly: isNothing addRow || props.readonly
            , rowEq: unsafeRefEq
            , summary:
                Row.row
                  { style: R.css
                      { flex: "1"
                      , flexWrap: "wrap"
                      , justifyContent: "flex-end"
                      }
                  , children: [ summary rows validateRows ]
                  }
            , rows: Left $ mapWithIndex Tuple rows
            , onRowAdd:
                for_ addRow \addRow' -> launchAff_ do
                  rowM <- addRow'
                  traverse_ (liftEffect <<< onChange <<< flip Array.snoc) rowM
            , onRowRemove: \(Tuple index _) ->
                onChange \rows' -> fromMaybe rows' (Array.deleteAt index rows')
            , removeCell: \onRowRemoveM (Tuple index row) ->
                rowMenu
                  { remove: onRowRemoveM <@> Tuple index row
                  , update: onChange <<< ix index
                  }
                  row
                  (validate row)
            , columns:
                columns <#> \{ label, render } ->
                  { label
                  , renderCell: \(Tuple i r) ->
                      render r (onChange <<< ix i)
                  , renderHeader: R.text
                  , headerStyle: mempty
                  }
            , infoColumns:
                infoColumns <#> \{ render } ->
                  { renderCell: \(Tuple i r) ->
                      render r (onChange <<< ix i)
                  }
            }
      , validate: validateRows
      }

-- | A `TableFormBuilder` makes a `FormBuilder` for a non-empty array where each
-- | row has columns defined by it.
nonEmptyEditableTable
  :: forall props row result
   . { addLabel :: String
     , addRow :: Maybe (Aff (Maybe row))
     , formBuilder :: TableFormBuilder { readonly :: Boolean | props } row result
     , maxRows :: Int
     , rowMenu
        :: { remove :: Maybe (Effect Unit)
           , update :: (row -> row) -> Effect Unit
           }
        -> row
        -> Maybe result
        -> JSX
     , summary
        :: NEA.NonEmptyArray row
        -> Maybe (NEA.NonEmptyArray result)
        -> JSX
     }
  -> FormBuilder
      { readonly :: Boolean | props }
      (NEA.NonEmptyArray row)
      (NEA.NonEmptyArray result)
nonEmptyEditableTable { addLabel, addRow, formBuilder: builder, maxRows, rowMenu, summary } =
  formBuilder \props rows ->
    let
      { columns, infoColumns, validate } = (un TableFormBuilder builder) props
      validateRows = traverse validate rows
    in
      { edit: \onChange ->
          EditableTable.editableTable
            { addLabel
            , maxRows
            , readonly: isNothing addRow || props.readonly
            , rowEq: unsafeRefEq
            , summary:
                Row.row
                  { style: R.css
                      { flex: "1"
                      , flexWrap: "wrap"
                      , justifyContent: "flex-end"
                      }
                  , children: [ summary rows validateRows ]
                  }
            , rows: Right $ mapWithIndex Tuple rows
            , onRowAdd:
                for_ addRow \addRow' -> launchAff_ do
                  rowM <- addRow'
                  traverse_ (liftEffect <<< onChange <<< flip NEA.snoc) rowM
            , onRowRemove: \(Tuple index _) ->
                onChange \rows' -> fromMaybe rows' (NEA.fromArray =<< NEA.deleteAt index rows')
            , removeCell: \onRowRemoveM (Tuple index row) ->
                rowMenu
                  { remove: onRowRemoveM <@> Tuple index row
                  , update: onChange <<< ix index
                  }
                  row
                  (validate row)
            , columns:
                columns <#> \{ label, render } ->
                  { label
                  , renderCell: \(Tuple i r) ->
                      render r (onChange <<< ix i)
                  , renderHeader: R.text
                  , headerStyle: mempty
                  }
            , infoColumns:
                infoColumns <#> \{ render } ->
                  { renderCell: \(Tuple i r) ->
                      render r (onChange <<< ix i)
                  }
            }
      , validate: validateRows
      }

-- | Default row menu that displays a bin icon, which, when clicked, deletes the
-- | current row.
defaultRowMenu
  :: forall row result
   . { remove :: Maybe (Effect Unit)
     , update :: (row -> row) -> Effect Unit
     }
  -> row
  -> Maybe result
  -> JSX
defaultRowMenu { remove } row _ =
  EditableTable.defaultRemoveCell (map const remove) row

-- | Convert a `FormBuilder` into a column of a table form with the specified
-- | label where all fields are laid out horizontally.
column_
  :: forall props row
   . String
  -> FormBuilder { readonly :: Boolean | props } row
  ~> TableFormBuilder { readonly :: Boolean | props } row
column_ label = column label Horizontal

-- | Convert a `FormBuilder` into a column of a table form with the specified
-- | label and orientation.
column
  :: forall props row
   . String
  -> Orientation
  -> FormBuilder { readonly :: Boolean | props } row
  ~> TableFormBuilder { readonly :: Boolean | props } row
column label orientation (FormBuilder f) =
  TableFormBuilder \props ->
    { columns:
        [ { label
          , render: \row onChange ->
              renderer orientation props.readonly $ (f props row).edit onChange
          }
        ]
    , infoColumns: []
    , validate: \row ->
        (f props row).validate
    }

-- | Convert a `FormBuilder` into a readonly info column of a table form with
-- | the specified label where all fields are laid out horizontally.
infoColumn_
  :: forall props row
   . FormBuilder { readonly :: Boolean | props } row
  ~> TableFormBuilder { readonly :: Boolean | props } row
infoColumn_ = infoColumn Horizontal

-- | Convert a `FormBuilder` into a readonly info column of a table form with
-- | the specified orientation.
infoColumn
  :: forall props row
   . Orientation
  -> FormBuilder { readonly :: Boolean | props } row
  ~> TableFormBuilder { readonly :: Boolean | props } row
infoColumn orientation (FormBuilder f) =
  TableFormBuilder \props ->
    let props' = props { readonly = true }
     in { columns: []
        , infoColumns:
            [ { render: \row onChange ->
                  renderer orientation props'.readonly $ (f props' row).edit onChange
              }
            ]
        , validate: \row ->
            (f props' row).validate
        }

renderer :: Orientation -> Boolean -> Array Tree -> JSX
renderer orientation =
  case orientation of
    Horizontal ->
      horizontalRenderer
    Vertical ->
      verticalRenderer

innerColumn_ :: Array JSX -> JSX
innerColumn_ children =
  Column.column
    { style: R.css { maxWidth: "100%" }
    , children
    }

innerRow_ :: Array JSX -> JSX
innerRow_ children =
  Row.row
    { style: R.css { maxWidth: "100%" }
    , children
    }

horizontalRenderer :: Boolean -> Forest -> JSX
horizontalRenderer readonly forest =
  innerRow_
    [ intercalate (hspace S12) (map (toColumn readonly) forest)
    ]

toColumn :: Boolean -> Tree -> JSX
toColumn readonly =
  case _ of
    Child { key, child } ->
      maybe identity keyed key $ child
    Wrapper { key, wrap, children } ->
      maybe identity keyed key $ wrap
        [ innerRow_
            [ intercalate (hspace S12) (map (toColumn readonly) children)
            ]
        ]
    Node { key, validationError, children } ->
      maybe identity keyed key $
        innerColumn_
          [ innerRow_
              [ intercalate (hspace S12) (map (toColumn readonly) children)
              ]
          , guard (not readonly) $
              foldMap errLine validationError
          ]

verticalRenderer :: Boolean -> Forest -> JSX
verticalRenderer readonly forest =
  innerColumn_
    [ intercalate (vspace S8) (map (toRow readonly) forest)
    ]

toRow :: Boolean -> Tree -> JSX
toRow readonly =
  case _ of
    Child { key, child } ->
      maybe identity keyed key $ child
    Wrapper { key, wrap, children } ->
      maybe identity keyed key $ wrap
        [ innerColumn_
            [ intercalate (vspace S8) (map (toColumn readonly) children)
            ]
        ]
    Node { key, validationError, children } ->
      maybe identity keyed key $
        innerColumn_
          [ intercalate (vspace S8) (map (toColumn readonly) children)
          , guard (not readonly) $
              foldMap errLine validationError
          ]

errLine :: ValidationMessage -> JSX
errLine =
  case _ of
    Error e ->
      T.text T.subtext
        { className = Nullable.notNull "labeled-field--validation-error"
        , children = [ R.text e ]
        }
    Warning w ->
      T.text T.subtext
        { className = Nullable.notNull "labeled-field--validation-warning"
        , children = [ R.text w ]
        }

-- | Make the props available. This allows for changing the structure of a table
-- | form builder based on the current props.
withProps
  :: forall props row result
   . (props -> TableFormBuilder props row result)
  -> TableFormBuilder props row result
withProps f = TableFormBuilder \props -> un TableFormBuilder (f props) props
