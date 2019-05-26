module Lumi.Components.Form.Hook where

import Prelude

import Data.Foldable (fold, intercalate, surround)
import Data.Maybe (Maybe, fromMaybe, maybe)
import Data.Monoid (guard)
import Data.Newtype (un)
import Data.Nullable (toNullable)
import Data.String as String
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Heterogeneous.Mapping (class HMap)
import Lumi.Components.Form.Internal (FormBuilder(..), Tree(..), Forest)
import Lumi.Components.Form.Validation (ModifyValidated, setModified)
import Lumi.Components.LabeledField (labeledField)
import Lumi.Components.Text (body, text)
import Prim.RowList (class RowToList)
import React.Basic.DOM as R
import React.Basic.Hooks (Hook, JSX, ReactComponent, UseState, component, element, keyed, useState, (/\))
import React.Basic.Hooks as React
import Unsafe.Coerce (unsafeCoerce)


useForm
  :: forall props unvalidated result xs
   . RowToList unvalidated xs
  => HMap ModifyValidated { | unvalidated } { | unvalidated }
  => FormBuilder
       { initialState :: { | unvalidated }
       , readonly :: Boolean
       , inlineTable :: Boolean
       , forceTopLabels :: Boolean
       | props
       }
       { | unvalidated }
       result
  -> { initialState :: { | unvalidated }
     , readonly :: Boolean
     , inlineTable :: Boolean
     , forceTopLabels :: Boolean
     | props
     }
  -> Hook (UseState { | unvalidated })
      { formData :: { | unvalidated }
      , setFormData :: ({ | unvalidated } -> { | unvalidated }) -> Effect Unit
      , setModified :: Effect Unit
      , reset :: Effect Unit
      , validated :: Maybe result
      , form :: JSX
      }
useForm editor props = React.do
  let
    renderer = renderForm
      { readonly: props.readonly
      , inlineTable: props.inlineTable
      , forceTopLabels: props.forceTopLabels
      }

  useForm' editor props renderer


useForm'
  :: forall props unvalidated unvalidated_ result
   . RowToList unvalidated unvalidated_
  => HMap ModifyValidated { | unvalidated } { | unvalidated }
  => FormBuilder
       { initialState :: { | unvalidated }
       | props
       }
       { | unvalidated }
       result
  -> { initialState :: { | unvalidated }
     | props
     }
  -> (Forest JSX -> JSX)
  -> Hook (UseState { | unvalidated })
      { formData :: { | unvalidated }
      , setFormData :: ({ | unvalidated } -> { | unvalidated }) -> Effect Unit
      , setModified :: Effect Unit
      , reset :: Effect Unit
      , validated :: Maybe result
      , form :: JSX
      }
useForm' editor props renderer = React.do
  formData /\ setFormData <- useState props.initialState

  let
    { edit, validate: validated } = un FormBuilder editor props formData
    forest = edit setFormData

  pure
    { formData
    , setFormData
    , setModified: setFormData \f -> setModified f
    , reset: setFormData \_ -> props.initialState
    , validated
    , form: renderer forest
    }


renderForm
  :: { forceTopLabels :: Boolean
     , readonly :: Boolean
     , inlineTable :: Boolean
     }
  -> Forest JSX
  -> JSX
renderForm { inlineTable, readonly, forceTopLabels } forest =
  element (R.unsafeCreateDOMComponent "lumi-form")
    { class:
        String.joinWith " " $ fold
          [ guard inlineTable ["inline-table"]
          , guard readonly ["readonly"]
          ]
    , children:
        surround fieldDivider (map toRow forest)
    }
  where
    fieldDivider = R.hr { className: "lumi field-divider" }

    toRow = case _ of
      Child { key, child } ->
        maybe identity keyed key $ child
      Wrapper { key, children } ->
        R.div
          { key: fromMaybe "" key
          , children: [ intercalate fieldDivider (map toRow children) ]
          }
      Node { label, key, required, validationError, children } ->
        maybe identity keyed key $ labeledField
          { label: text body
              { children = [ label ]
              , className = toNullable (pure "field-label")
              }
          , value: intercalate fieldDivider (map toRow children)
          , validationError
          , required
          , forceTopLabel: forceTopLabels
          , style: R.css {}
          }


-- | Consume `useForm` as a render-prop component.
-- |
-- | Note: `editor` must be fully applied outside only once,
-- |   outside your render function.
formState
  :: forall props unvalidated result xs
   . RowToList unvalidated xs
  => HMap ModifyValidated { | unvalidated } { | unvalidated }
  => FormBuilder
       { initialState :: { | unvalidated }
       , readonly :: Boolean
       , inlineTable :: Boolean
       , forceTopLabels :: Boolean
       | props
       }
       { | unvalidated }
       result
  -> ReactComponent
      { initialState :: { | unvalidated }
      , render
          :: { formData :: { | unvalidated }
             , setFormData :: ({ | unvalidated } -> { | unvalidated }) -> Effect Unit
             , setModified :: Effect Unit
             , reset :: Effect Unit
             , validated :: Maybe result
             , form :: JSX
             }
          -> JSX
      | props
      }
formState editor = unsafePerformEffect do
  component "FormState" \props -> React.do
    state <- useForm editor (unsafeCoerce props)
    pure (props.render state)
