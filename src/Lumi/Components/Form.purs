module Lumi.Components.Form
  ( module Defaults
  , module Internal
  , module Validation
  , build
  , build'
  , defaultRenderForm
  , defaultRenderForest
  , useForm
  , useForm'
  , formState
  , static
  , section
  , inputBox
  , textbox
  , passwordBox
  , textarea
  , textarea_
  , switch
  , switch'
  , checkbox
  , labeledCheckbox
  , radioGroup
  , file
  , genericSelect
  , class GenericSelect
  , fromString
  , toString
  , select
  , SelectConfig
  , selectWith
  , multiSelect
  , asyncSelect
  , asyncSelectByKey
  , number
  , array
  , fixedSizeArray
  , arrayModal
  , fetch
  , fetchWithDebounce
  , fetch_
  , asyncEffect
  , effect
  , initializer
  , via
  , focus
  , match
  , match_
  , withProps
  , withValue
  , mapProps
  , mapUI
  , mapUI_
  , indent
  , jsxIndent
  , wrap
  , filterWithProps
  , withKey
  , styles
  ) where

import Prelude

import Color (cssStringHSLA)
import Data.Array ((:))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (fold, surround)
import Data.Generic.Rep (class Generic, Constructor(..), NoArguments(..), Sum(..), to, from)
import Data.Lens (Iso', Lens', Prism, Prism', matching, review, view)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Monoid (guard)
import Data.Newtype (un)
import Data.Nullable (notNull, null, toNullable)
import Data.String as String
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Time.Duration (Milliseconds)
import Data.Traversable (intercalate, traverse, traverse_)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Heterogeneous.Mapping (class Mapping)
import JSS (JSS, jss)
import Lumi.Components.Color (colors)
import Lumi.Components.Column (column)
import Lumi.Components.FetchCache as FetchCache
import Lumi.Components.Form.Defaults (formDefaults) as Defaults
import Lumi.Components.Form.Internal (Forest, FormBuilder'(..), FormBuilder, SeqFormBuilder, Tree(..), formBuilder, formBuilder_, invalidate, pruneTree, sequential)
import Lumi.Components.Form.Internal (Forest, FormBuilder', FormBuilder, SeqFormBuilder', SeqFormBuilder, Tree(..), formBuilder, formBuilder_, invalidate, listen, parallel, revalidate, sequential) as Internal
import Lumi.Components.Form.Validation (ModifyValidated(..), Validated(..), Validator, ModifyValidatedProxy(..), class CustomModifyValidated, customModifyValidated, _Validated, fromValidated, mustBe, mustEqual, nonEmpty, nonEmptyArray, nonNull, nonEmpty', nonEmptyArray', nonNull', optional, setFresh, setModified, validDate, validInt, validNumber, validDate', validInt', validNumber', validated, warn) as Validation
import Lumi.Components.Input (alignToInput)
import Lumi.Components.Input as Input
import Lumi.Components.LabeledField (RequiredField(..), labeledField, labeledFieldValidationErrorStyles, labeledFieldValidationWarningStyles)
import Lumi.Components.Link as Link
import Lumi.Components.Loader (loader)
import Lumi.Components.Modal (modalLink, modalTitle)
import Lumi.Components.NativeSelect as NativeSelect
import Lumi.Components.Orientation (Orientation(..))
import Lumi.Components.Row (row)
import Lumi.Components.Select as Select
import Lumi.Components.Text (body, body_, subsectionHeader, text)
import Lumi.Components.Textarea as Textarea
import Lumi.Components.Upload as Upload
import Prim.Row (class Nub, class Union)
import React.Basic.Classic (JSX, createComponent, element, empty, fragment, keyed, makeStateless)
import React.Basic.Classic.Components.Async (async, asyncWithLoader)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture, stopPropagation, targetChecked, targetValue)
import React.Basic.Events as Events
import React.Basic.Hooks as Hooks
import Record (merge)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- | Create a React component for a form from a `FormBuilder`.
-- |
-- | _Note_: this function should be fully applied, to avoid remounting
-- | the component on each render.

build
  :: forall props unvalidated result
   . FormBuilder { readonly :: Boolean | props } unvalidated result
  -> { value :: unvalidated
     , onChange :: (unvalidated -> unvalidated) -> Effect Unit
     , forceTopLabels :: Boolean
     , inlineTable :: Boolean
     , formProps :: { readonly :: Boolean | props }
     }
  -> JSX
build = build' defaultRenderForm

-- | Create a React component for a form from a `FormBuilder'` and a custom
-- | rendering function.
-- |
-- | _Note_: this function should be fully applied, to avoid remounting
-- | the component on each render.
build'
  :: forall ui renderProps formProps unvalidated result
   . ({| renderProps } -> formProps -> ui -> JSX)
  -> FormBuilder' ui formProps unvalidated result
  -> { value :: unvalidated
     , onChange :: (unvalidated -> unvalidated) -> Effect Unit
     , formProps :: formProps
     | renderProps
     }
  -> JSX
build' render editor =
  makeStateless (createComponent "Form") \props@{ value, onChange, formProps } ->
    let
      { edit } = un FormBuilder editor formProps value
    in
      render (contractRenderProps props) formProps (edit onChange)
  where
    contractRenderProps
      :: { value :: unvalidated
         , onChange :: (unvalidated -> unvalidated) -> Effect Unit
         , formProps :: formProps
         | renderProps
         }
      -> {| renderProps }
    contractRenderProps = unsafeCoerce


-- | The default Lumi implementation for rendering a forest of JSX
-- | form fields.
defaultRenderForm
  :: forall props
   . { forceTopLabels :: Boolean
     , inlineTable :: Boolean
     }
  -> { readonly :: Boolean
     | props
     }
  -> Forest
  -> JSX
defaultRenderForm renderProps@{ inlineTable, forceTopLabels } { readonly } forest =
  R.div
    { className:
        String.joinWith " " $ fold
          [ [ "lumi-form" ]
          , guard inlineTable ["inline-table"]
          , guard readonly ["readonly"]
          ]
    , children:
        [ surround fieldDivider
          $ defaultRenderForest { forceTopLabels }
          $ Array.mapMaybe pruneTree
          $ forest
        ]
    }
  where
    fieldDivider = R.hr { className: "lumi field-divider" }

defaultRenderForest
  :: { forceTopLabels :: Boolean
     }
  -> Forest
  -> Array JSX
defaultRenderForest renderProps@{ forceTopLabels } = map case _ of
  Child { key, child } ->
    maybe identity keyed key $ child
  Wrapper { key, wrap: f, children } ->
    maybe identity keyed key
      $ f
      $ intercalate [fieldDivider]
      $ map pure
      $ defaultRenderForest renderProps
      $ children
  Node { label, key, required, validationError, children } ->
    maybe identity keyed key $
      labeledField
        { label: text body
            { children = [ label ]
            , className = toNullable (pure "field-label")
            }
        , value: intercalate fieldDivider (defaultRenderForest renderProps children)
        , validationError
        , required
        , forceTopLabel: forceTopLabels
        , style: R.css {}
        }
  where
    fieldDivider = R.hr { className: "lumi field-divider" }

-- | Render a form with state managed automatically.
useForm
  :: forall props unvalidated result
   . Mapping Validation.ModifyValidated unvalidated unvalidated
  => FormBuilder
       { readonly :: Boolean
       | props
       }
       unvalidated
       result
  -> { initialState :: unvalidated
     , inlineTable :: Boolean
     , forceTopLabels :: Boolean
     , formProps :: { readonly :: Boolean | props }
     }
  -> Hooks.Hook (Hooks.UseState unvalidated)
      { formData :: unvalidated
      , setFormData :: (unvalidated -> unvalidated) -> Effect Unit
      , setModified :: Effect Unit
      , reset :: Effect Unit
      , validated :: Maybe result
      , form :: JSX
      }
useForm editor props = Hooks.do
  let
    renderer = defaultRenderForm
      { inlineTable: props.inlineTable
      , forceTopLabels: props.forceTopLabels
      }
      props.formProps

  f <- useForm' editor props.initialState props.formProps
  pure f { form = renderer f.form }


-- | Like `useForm`, but allows an alternative render implementation
-- | to be provided as an additional argument.
useForm'
  :: forall ui props unvalidated result
   . Mapping Validation.ModifyValidated unvalidated unvalidated
  => FormBuilder' ui props unvalidated result
  -> unvalidated
  -> props
  -> Hooks.Hook (Hooks.UseState unvalidated)
      { formData :: unvalidated
      , setFormData :: (unvalidated -> unvalidated) -> Effect Unit
      , setModified :: Effect Unit
      , reset :: Effect Unit
      , validated :: Maybe result
      , form :: ui
      }
useForm' editor initialState props = Hooks.do
  formData /\ setFormData <- Hooks.useState initialState

  let
    { edit, validate: validated } = un FormBuilder editor props formData
    ui = edit setFormData

  pure
    { formData
    , setFormData
    , setModified: setFormData Validation.setModified
    , reset: setFormData \_ -> initialState
    , validated
    , form: ui
    }


-- | Consume `useForm` as a render-prop component. Useful when `useForm`
-- | would be preferred but you don't want to migrate an entire component
-- | to React's hooks API.
-- |
-- | _Note_: this function should be fully applied, to avoid remounting
-- | the component on each render.
formState ::
  forall props unvalidated result.
  Mapping Validation.ModifyValidated unvalidated unvalidated =>
  { initialState :: unvalidated
  , form :: FormBuilder { readonly :: Boolean | props } unvalidated result
  , inlineTable :: Boolean
  , forceTopLabels :: Boolean
  , formProps :: { readonly :: Boolean | props }
  , render
      :: { formData :: unvalidated
          , setFormData :: (unvalidated -> unvalidated) -> Effect Unit
          , setModified :: Effect Unit
          , reset :: Effect Unit
          , validated :: Maybe result
          , form :: JSX
          }
      -> JSX
  } ->
  JSX
formState = unsafePerformEffect do
  Hooks.component "FormState" \props -> Hooks.do
    state <- useForm props.form
      { initialState: props.initialState
      , inlineTable: props.inlineTable
      , forceTopLabels: props.forceTopLabels
      , formProps: props.formProps
      }
    pure (props.render state)


-- | Create an always-valid `FormBuilder` that renders the supplied `JSX`.
static :: forall props value. JSX -> FormBuilder props value Unit
static edit = formBuilder \_ _ -> { edit: const edit, validate: pure unit }

-- | A formatted section header used to visually separate the parts of a form
section :: forall props value. String -> FormBuilder props value Unit
section title =
  static $
    text subsectionHeader
      { children = [ R.text title ]
      , style = R.css
          { marginBottom: "16px"
          , display: "block"
          }
      }

-- | A configurable input box makes a `FormBuilder` for strings
inputBox
  :: forall props
   . Input.InputProps
  -> FormBuilder
       { readonly :: Boolean | props }
       String
       String
inputBox inputProps = formBuilder_ \{ readonly } s onChange ->
  if readonly
    then Input.alignToInput $ body_ s
    else Input.input inputProps
           { value = s
           , onChange = capture targetValue (traverse_ onChange)
           , style = R.css { width: "100%" } <> inputProps.style
           }

-- | A simple text box makes a `FormBuilder` for strings
textbox
  :: forall props
   . FormBuilder
       { readonly :: Boolean | props }
       String
       String
textbox = inputBox Input.text_

-- | A simple password box makes a `FormBuilder` for strings
passwordBox
  :: forall props
   . FormBuilder
       { readonly :: Boolean | props }
       String
       String
passwordBox = inputBox Input.password

-- | A configurable textarea box makes a `FormBuilder` for strings
textarea_
  :: forall props
   . Textarea.TextareaProps
  -> FormBuilder
       { readonly :: Boolean | props }
       String
       String
textarea_ textareaProps = formBuilder_ \{ readonly } s onChange ->
  if readonly
    then Input.alignToInput $ R.text s
    else Textarea.textarea textareaProps
           { value = s
           , onChange = capture targetValue (traverse_ onChange)
           , style = R.css { width: "100%" }
           }

-- | A simple text box makes a `FormBuilder` for strings
textarea
  :: forall props
   . FormBuilder
       { readonly :: Boolean | props }
       String
       String
textarea = formBuilder_ \{ readonly } s onChange ->
  if readonly
    then Input.alignToInput $ R.text s
    else Textarea.textarea Textarea.defaults
           { value = s
           , onChange = capture targetValue (traverse_ onChange)
           , style = R.css { width: "100%" }
           }

-- | A `switch` is an editor for booleans which displays Yes or No.
switch
  :: forall props
   . FormBuilder
       { readonly :: Boolean | props }
       Boolean
       Boolean
switch = formBuilder_ \{ readonly } b onChange ->
  if readonly
    then Input.alignToInput $ R.text (if b then "Yes" else "No")
    else Input.input Input.switch
           { checked = if b then Input.On else Input.Off
           , onChange = Events.handler (stopPropagation >>> targetChecked) (traverse_ onChange)
           }

-- | A `switch` is an editor for booleans which displays Yes or No.
switch'
  :: forall props
   . Input.InputProps ->
     FormBuilder
       { readonly :: Boolean | props }
       Boolean
       Boolean
switch' inputProps = formBuilder_ \{ readonly } b onChange ->
  if readonly
    then Input.alignToInput $ R.text (if b then "Yes" else "No")
    else Input.input (inputProps `merge` Input.switch)
           { checked = if b then Input.On else Input.Off
           , onChange = Events.handler (stopPropagation >>> targetChecked) (traverse_ onChange)
           }

-- | A `checkbox` is an editor for booleans which displays checked or not checked.
checkbox :: forall props. FormBuilder { readonly :: Boolean | props } Boolean Boolean
checkbox = labeledCheckbox mempty

-- | A `labeledCheckbox` is an editor that behaves exactly like `checkbox` but
-- | also accepts a JSX displayed as a label to its right.
labeledCheckbox
  :: forall props
  . JSX
  -> FormBuilder { readonly :: Boolean | props } Boolean Boolean
labeledCheckbox = \label ->
  formBuilder_ \{ readonly } value onChange ->
    Input.label
      { style: R.css
          { flexDirection: "row"
          , alignSelf: "stretch"
          , alignItems: "baseline"
          }
      , for: null
      , children:
          [ Input.input Input.checkbox
              { style = R.css { marginBottom: "0", alignSelf: "baseline" }
              , checked = if value then Input.On else Input.Off
              , disabled = if readonly then true else false
              , onChange =
                  if readonly
                    then Events.handler Events.syntheticEvent \_ -> pure unit
                    else Events.handler (stopPropagation >>> targetChecked) (traverse_ onChange)
              }
          , lumiAlignToInput
              { style: { flex: "1", marginLeft: "8px" }
              , children: label
              }
          ]
      }
  where
    lumiAlignToInput = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-align-to-input")

-- | A form that edits an optional structure represented by group of radio
-- | buttons, visually oriented in either horizontal or vertical fashion.
-- |
-- | This is similar to `select`, but displays radio buttons instead.
radioGroup
  :: forall props a
   . Eq a
  => Orientation
  -> Array { label :: JSX, value :: a }
  -> FormBuilder { readonly :: Boolean | props } (Maybe a) (Maybe a)
radioGroup = radioGroup'
  where
    lumiAlignToInput = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-align-to-input")

    radioGroup' orientation options = formBuilder_  \props selected onChange ->
      wrapper $ options <#> \{ label, value } ->
        Input.label
          { style: R.css
              { flexDirection: "row"
              , alignSelf: "stretch"
              }
          , for: null
          , children:
              [ Input.input Input.radio
                  { style = R.css { marginRight: 8 }
                  , checked = if Just value == selected then Input.On else Input.Off
                  , onChange =
                      Events.handler targetChecked \s ->
                        when (fromMaybe false s) $
                          onChange (Just value)
                  }
              , lumiAlignToInput
                  { style: { flex: "1" }
                  , children: label
                  }
              ]
          }
      where
      wrapper :: Array JSX -> JSX
      wrapper children =
        case orientation of
          Horizontal ->
            row
              { children
              , style: R.css
                  { alignItems: "center"
                  , justifyContent: "flex-start"
                  , flexWrap: "wrap"
                  }
              }
          Vertical ->
            column
              { children
              , style: R.css { alignItems: "flex-start" }
              }

-- | A editor consisting of a file picker.
file
  :: forall props
   . { variant :: Upload.UploadVariant
     , backend :: Upload.UploadBackend
     }
  -> FormBuilder
     { readonly :: Boolean | props }
     (Array Upload.FileId)
     (Array Upload.FileId)
file opts = formBuilder_ \{ readonly } value onChange ->
  Upload.upload Upload.defaults
    { value = value
    , variant = opts.variant
    , onChange = onChange
    , readonly = readonly
    , backend = opts.backend
    }

-- | An editor consisting of a single-select dropdown. For more customization,
-- | see 'selectWith'.
select
  :: forall props a
   . (a -> String)
  -> (String -> Maybe a)
  -> Array { label :: String, value :: a }
  -> FormBuilder { readonly :: Boolean | props } (Maybe a) (Maybe a)
select toString fromString opts =
  selectWith
    { toString
    , fromString
    , opts
    , placeholderLabel: "Select an option ..."
    , initialValue: Nothing
    }

type SelectConfig a =
  { "toString" :: a -> String
  , "fromString" :: String -> Maybe a
  , "opts" :: Array { label :: String, value :: a }
  , "placeholderLabel" :: String
  , "initialValue" :: Maybe a
  }

-- | An editor consisting of a single-select dropdown. For a simpler variant,
-- | see 'select'.
selectWith
  :: forall props a
   . SelectConfig a
  -> FormBuilder { readonly :: Boolean | props } (Maybe a) (Maybe a)
selectWith config =
  formBuilder_ \{ readonly } selected onChange ->
    if readonly then
      let toLabel a =
            Array.findMap (\{ label, value } ->
              if toString value == toString a
                then Just label
                else Nothing) opts
       in Input.alignToInput $ R.text (fromMaybe "" (selected >>= toLabel))
    else
        NativeSelect.nativeSelect NativeSelect.defaults
          { options =
              { label: placeholderLabel, value: "" }
                : map (\{ label, value } -> { label, value: toString value }) opts
          , onChange =
              capture targetValue \newValue ->
                onChange (fromString =<< newValue)
          , value =
              case selected of
                Nothing -> maybe "" toString initialValue
                Just x -> toString x
          }
  where
  { toString, fromString, opts, placeholderLabel, initialValue } = config

class GenericSelect rep where
  fromString :: String -> Maybe rep
  toString :: rep -> String

instance sumGenericSelect :: (GenericSelect a, GenericSelect b) => GenericSelect (Sum a b) where
  toString (Inl a) = toString a
  toString (Inr a) = toString a
  fromString s = case fromString s of
    Just s'-> Just (Inl s')
    Nothing -> map Inr (fromString s)

instance sumGenericSelectConstructor :: (IsSymbol name) => GenericSelect (Constructor name NoArguments) where
  toString _ = reflectSymbol (Proxy :: Proxy name)
  fromString s | s == reflectSymbol (Proxy :: Proxy name) = Just (Constructor NoArguments)
               | otherwise = Nothing

-- | given values which are composed from a sum-type, generate a select dropdown from its elements
-- |
-- | data Yup = One | Two
-- | genericSelect [{label: "1st", value: One}, {label: "2nd", value: Two}]
-- | This would serialize to- and from select options automatically
genericSelect
  :: forall props a rep
   . Generic a rep
  => GenericSelect rep
  => Array { label :: String, value :: a }
  -> FormBuilder { readonly :: Boolean | props } (Maybe a) (Maybe a)
genericSelect opts =
  select (toString <<< from) (map to <<< fromString) opts

-- | An editor consisting of a multi-select dropdown.
multiSelect
  :: forall props a
   . (a -> String)
  -> Array { label :: String, value :: a }
  -> FormBuilder { readonly :: Boolean | props } (Array a) (Array a)
multiSelect encode opts = formBuilder_ \{ readonly } selected onChange ->
  if readonly
    then
      alignToInput $ R.text (String.joinWith ", " $ map encode selected)
    else
      Select.multiSelect
        { value:
            selected # Array.mapMaybe \sel ->
              opts # Array.find \{ value } -> encode value == encode sel
        , options: opts
        , optionSort: Nothing
        , onChange: onChange <<< map _.value
        , className: ""
        , style: R.css {}
        , searchable: true
        , id: ""
        , name: ""
        , noResultsText: "No results"
        , placeholder: "Select an option..."
        , disabled: false
        , loading: false
        , optionRenderer: R.text <<< _.label
        , toSelectOption: \r -> { value: encode r.value, label: r.label }
        }

-- | An editor which uses an API call to populate a single-select
-- | drop-down.
asyncSelect
  :: forall props a
   . (String -> Aff (Array a))
  -> (a -> Select.SelectOption)
  -> (a -> JSX)
  -> FormBuilder { readonly :: Boolean | props } (Maybe a) (Maybe a)
asyncSelect loadOptions toSelectOption optionRenderer =
  formBuilder_ \props@{ readonly } value onChange ->
    if readonly
      then case value of
        Nothing     -> empty
        Just value_ -> alignToInput $
          text body
            { children = [ optionRenderer value_ ]
            }

      else Select.asyncSingleSelect
        { value
        , loadOptions
        , onChange: onChange
        , className: ""
        , style: R.css {}
        , searchable: true
        , id: ""
        , name: ""
        , disabled: false
        , loading: false
        , noResultsText: "No results"
        , placeholder: "Search..."
        , optionRenderer
        , optionSort: Nothing
        , toSelectOption
        }

-- | Similar to `asyncSelect` but allows the current value to be specified using only its key.
asyncSelectByKey
  :: forall props id a
   . (id -> Aff a)
  -> (String -> Aff (Array a))
  -> (id -> String)
  -> (String -> id)
  -> (a -> Select.SelectOption)
  -> (a -> JSX)
  -> FormBuilder { readonly :: Boolean | props } (Maybe id) (Maybe id)
asyncSelectByKey getData loadOptions fromId toId toSelectOption optionRenderer =
  formBuilder_ \props@{ readonly } value onChange ->
    FetchCache.single
      { getData: getData <<< toId
      , id: map fromId value
      , render: \data_ ->
          if readonly
            then case value of
              Nothing -> empty
              Just _  -> alignToInput
                case data_ of
                  Nothing     -> loader
                    { style: R.css { width: "20px", height: "20px", borderWidth: "2px" }
                    , testId: toNullable Nothing
                    }
                  Just data_' -> text body
                        { children = [ optionRenderer data_' ]
                        }
            else
              Select.asyncSingleSelect
                { value: data_
                , loadOptions
                , onChange: onChange <<< map (toId <<< _.value <<< toSelectOption)
                , className: ""
                , style: R.css {}
                , searchable: true
                , id: ""
                , name: ""
                , disabled: false
                , loading: isJust value && isNothing data_
                , noResultsText: "No results"
                , placeholder: "Search..."
                , optionRenderer
                , optionSort: Nothing
                , toSelectOption
                }
      }

-- | A form which edits a number. The form produces a string as a result in
-- | order to allow more control over validation (e.g. to allow special
-- | handling of the empty string, or to distinguish 1, 1.0, and 1.00000 from
-- | each other).
-- |
-- | See also `validNumber` from the Validation module.
number
  :: forall props
   . { min :: Maybe Number
     , max :: Maybe Number
     , step :: Input.InputStep
     }
  -> FormBuilder { readonly :: Boolean | props } String String
number { min, max, step } = formBuilder \{ readonly } value ->
  { edit: \onChange ->
      if readonly
        then Input.alignToInput (R.text value)
        else
          Input.input Input.number
            { value = value
            , onChange = Events.handler targetValue (traverse_ (onChange <<< const))
            , style = R.css { width: "100%" }
            , min = toNullable min
            , max = toNullable max
            , step = notNull step
            }
  , validate: Just value
  }

-- | Edit an `Array` of values.
-- |
-- | This `FormBuilder` displays a removable section for each array element,
-- | along with an "Add..." button in the final row.
array
  :: forall props u a
   . { label :: String
     , addLabel :: String
     , defaultValue :: u
     , editor :: FormBuilder { readonly :: Boolean | props } u a
     }
  -> FormBuilder { readonly :: Boolean | props } (Array u) (Array a)
array { label, addLabel, defaultValue, editor } = FormBuilder \props@{ readonly } xs ->
  let editAt i f xs' = fromMaybe xs' (Array.modifyAt i f xs')
      wrapper children = Array.singleton $ Wrapper { key: Nothing, wrap: R.div_, children }
   in { edit: \onChange ->
          wrapper $ xs # Array.mapWithIndex (\i x ->
                Node
                  { label:
                      fragment
                        [ if readonly
                            then empty
                            else Link.link Link.defaults
                              { navigate = pure $ onChange (\xs' -> fromMaybe xs' (Array.deleteAt i xs'))
                              , text = R.text "×"
                              }
                        , R.text " "
                        , R.text label
                        , R.text (" #" <> show (i + 1))
                        ]
                  , key: Nothing
                  , required: Neither
                  , validationError: Nothing
                  , children: (un FormBuilder editor props x).edit (onChange <<< editAt i)
                  })
             # if readonly then identity else flip Array.snoc
                (Child
                  { key: Nothing
                  , child:
                      Link.link Link.defaults
                        { navigate = pure $ onChange (flip append [defaultValue])
                        , className = pure "lumi"
                        , text = Input.alignToInput $ body_ ("+ " <> addLabel)
                        }
                  })
      , validate: traverse (un FormBuilder editor props >>> _.validate) xs
      }

-- | Edit an `Array` of values without letting the user add or remove entries.
fixedSizeArray
  :: forall props u a
   . { label :: String
     , editor :: FormBuilder { readonly :: Boolean | props } u a
     }
  -> FormBuilder { readonly :: Boolean | props } (Array u) (Array a)
fixedSizeArray { label, editor } = FormBuilder \props xs ->
  let editAt i f xs' = fromMaybe xs' (Array.modifyAt i f xs')
   in { edit: \onChange ->
          xs # Array.mapWithIndex (\i x ->
                Node
                  { key: Just (show i)
                  , label:
                      fragment
                        [ R.text " "
                        , R.text label
                        , R.text (" #" <> show (i + 1))
                        ]
                  , required: Neither
                  , validationError: Nothing
                  , children: (un FormBuilder editor props x).edit (onChange <<< editAt i)
                  })
      , validate: traverse (un FormBuilder editor props >>> _.validate) xs
      }

-- | Edit an `Array` of values.
-- |
-- | Unlike `array`, this `FormBuilder` uses a modal popup for adding and
-- | editing array elements.
-- |
-- | `Note`: `arrayModal` does not support validation, in the sense that the
-- | component _inside_ the modal popup cannot reject its form state when the
-- | use clicks the save button.
arrayModal
  :: forall props componentProps a
   . Union
       componentProps
       ( value :: a
       , onChange :: a -> Effect Unit
       )
       ( value :: a
       , onChange :: a -> Effect Unit
       | componentProps
       )
  => Nub
       ( value :: a
       , onChange :: a -> Effect Unit
       | componentProps
       )
       ( value :: a
       , onChange :: a -> Effect Unit
       | componentProps
       )
  => { label :: String
     , addLabel :: String
     , defaultValue :: a
     , summary :: { readonly :: Boolean | props } -> a -> JSX
     , component :: { value :: a
                    , onChange :: a -> Effect Unit
                    | componentProps
                    }
                 -> JSX
     , componentProps :: Record componentProps
     }
  -> FormBuilder { readonly :: Boolean | props } (Array a) (Array a)
arrayModal { label, addLabel, defaultValue, summary, component, componentProps } = FormBuilder \props@{ readonly } xs ->
  let editAt i f xs' = fromMaybe xs' (Array.modifyAt i f xs')
      wrapper children = Array.singleton $ Wrapper { key: Nothing, wrap: R.div_, children }
   in { edit : \onChange ->
          wrapper $ xs # Array.mapWithIndex (\i x ->
                Node
                  { label:
                      fragment
                        [ if readonly
                            then empty
                            else Link.link Link.defaults
                              { navigate = pure $ onChange (\xs' -> fromMaybe xs' (Array.deleteAt i xs'))
                              , text = R.text "×"
                              }
                        , R.text " "
                        , R.text label
                        , R.text (" #" <> show (i + 1))
                        ]
                  , key: Nothing
                  , required: Neither
                  , validationError: Nothing
                  , children: pure $ Child $
                      { key: Nothing
                      , child:
                          Input.alignToInput
                            if readonly
                              then summary props x
                              else modalLink
                                    { label: summary props x
                                    , title: modalTitle addLabel
                                    , value: x
                                    , onChange: onChange <<< editAt i <<< const
                                    , actionButtonTitle: addLabel
                                    , component
                                    , componentProps
                                    , style: R.css {}
                                    }
                      }
                  })
             # if readonly then identity else flip Array.snoc
                 (Child
                    { key: Nothing
                    , child:
                        Input.alignToInput $
                          modalLink
                            { label: body_ ("+ " <> addLabel)
                            , title: modalTitle addLabel
                            , value: defaultValue
                            , onChange: \x -> onChange (flip append [x])
                            , actionButtonTitle: addLabel
                            , component
                            , componentProps
                            , style: R.css {}
                            }
                    })
      , validate: pure xs
      }

-- | Form that performs an asynchronous effect whenever `id` changes.
-- | The result is `Nothing` while the effect is not completed, and a `Just`
-- | after the value is available.
fetch :: forall props a. JSX -> String -> (String -> Aff a) -> FormBuilder props (Maybe a) (Maybe a)
fetch = fetchWithDebounce mempty

-- | Form that performs an asynchronous effect whenever `id` changes.
-- | The result is `Nothing` while the effect is not completed, and a `Just`
-- | after the value is available.
fetchWithDebounce :: forall props a. Milliseconds -> JSX -> String -> (String -> Aff a) -> FormBuilder props (Maybe a) (Maybe a)
fetchWithDebounce = \debounce loading id getData ->
  formBuilder_ \props value onChange ->
    FetchCache.singleWithDebounce
      { id: Just id
      , getData: \id' -> do
          liftEffect $ onChange Nothing
          getData id'
      , render: \v ->
          if isJust v
            then keyed id $ async (mempty <$ liftEffect (onChange v))
            else loading
      , debounce: debounce
      }

-- | Performs an asynchronous effect once and returns its result encapsulated in
-- | `Just`. The result is `Nothing` while the effect is not completed.
fetch_ :: forall props a. JSX -> Aff a -> FormBuilder props (Maybe a) (Maybe a)
fetch_ loading getData = fetch loading "" (const getData)

-- | A dummy form that, whenever the specified key changes, performs an
-- | asynchronous effect. It displays the specified JSX while the effect is not
-- | complete, sets the form data to the result of the effect and returns it.
asyncEffect :: forall props a. String -> JSX -> Aff (a -> a) -> FormBuilder props a a
asyncEffect key loader aff =
  withKey key $ formBuilder \_ value ->
    { edit: \onChange ->
        keyed key $ asyncWithLoader loader do
          f <- aff
          liftEffect $ onChange f
          mempty
    , validate: Just value
    }

-- | A dummy form that, whenever the specified key changes, performs an
-- | effect. It sets the form data to the result of the effect and returns it.
effect :: forall props a. String -> Effect (a -> a) -> FormBuilder props a a
effect key = asyncEffect key mempty <<< liftEffect

-- | Sequential `SeqFormBuilder` used for asynchronously initializing some
-- | piece of form data, invalidating the form and preventing the rendering of
-- | subsequent fields while the supplied `Aff` is not completed.
-- |
-- | For example:
-- |
-- | ```purescript
-- | myForm = parallel do
-- |   initializer mempty \props value -> do
-- |     foo <- fetchDefaultFoo
-- |     pure $ value
-- |       { foo = foo
-- |       , bar = props.bar
-- |       }
-- |
-- |   sequential ado
-- |     foo <- focus (Proxy :: Proxy "foo") textbox
-- |     bar <- focus (Proxy :: Proxy "bar") switch
-- |     in { foo, bar }
-- | ```
initializer
  :: forall props value
   . Nub (initialized :: Boolean | value) (initialized :: Boolean | value)
  => JSX
  -> (props -> {| value } -> Aff ({ initialized :: Boolean | value } -> { initialized :: Boolean | value }))
  -> SeqFormBuilder props { initialized :: Boolean | value } Unit
initializer loader aff =
  sequential "initializer" $ withValue \value@{ initialized } -> withProps \props ->
    if initialized then
      pure unit
    else
      invalidate
        $ void
        $ asyncEffect "" loader
        $ map (_{ initialized = true } <<< _)
        $ aff props (contractValue value)
  where
    contractValue :: { initialized :: Boolean | value } -> {| value }
    contractValue = unsafeCoerce

-- | Modify a `FormBuilder` using a (partial) `Iso`.
-- |
-- | Technically, we don't require `to (from s) = s`, but we do require
-- | `from (to a) = a` for the form to act sensibly. Since there's no
-- | `PartialIso` in `profunctor-lenses`, we use `Iso` here.
-- |
-- | Caveat emptor, you get what you pay for if you pass in a dodgy
-- | `Iso` here.
via
  :: forall ui props s a result
   . Iso' s a
  -> FormBuilder' ui props a result
  -> FormBuilder' ui props s result
via i e = FormBuilder \props s ->
  let { edit, validate } = un FormBuilder e props (view i s)
   -- TODO: make this point-free
   in { edit: \k -> edit (k <<< i)
      , validate
      }

-- | Focus a `FormBuilder` on a smaller piece of state, using a `Lens`.
focus
  :: forall ui props s a result
   . Lens' s a
  -> FormBuilder' ui props a result
  -> FormBuilder' ui props s result
focus l e = FormBuilder \props s ->
  let { edit, validate } = un FormBuilder e props (view l s)
   in { edit: \k -> edit (k <<< l)
      , validate
      }

-- | Focus a `FormBuilder` on a possible type of state, using a `Prism`,
-- | ignoring validation.
match_
  :: forall ui props s a
   . Monoid ui
  => Prism' s a
  -> FormBuilder' ui props a a
  -> FormBuilder' ui props s s
match_ p = match p p

-- | Focus a `FormBuilder` on a possible type of state, using a `Prism`.
-- |
-- | We need two `Prism`s in order to change the result type for
-- | validation purposes.
match
  :: forall ui props result s t a
   . Monoid ui
  => Prism s s a a
  -> Prism s t a result
  -> FormBuilder' ui props a result
  -> FormBuilder' ui props s t
match p1 p2 e = FormBuilder \props s ->
  case matching p2 s of
    Left t -> { edit: mempty, validate: pure t }
    Right a ->
      let { edit, validate } = un FormBuilder e props a
      in { edit: \k -> edit (k <<< p1)
          , validate: map (review p2) validate
          }

-- | Change the props type.
mapProps
  :: forall ui p q u a
  . (q -> p)
  -> FormBuilder' ui p u a
  -> FormBuilder' ui q u a
mapProps f form = FormBuilder (un FormBuilder form <<< f)

-- | Change the UI type of a form.
mapUI_
  :: forall ui ui' props value result
   . (ui -> ui')
  -> FormBuilder' ui props value result
  -> FormBuilder' ui' props value result
mapUI_ f = mapUI \_ _ _ -> f

-- | Change the UI type of a form based on the props, the current value and the
-- | validated result.
mapUI
  :: forall ui ui' props value result
   . (props -> value -> Maybe result -> ui -> ui')
  -> FormBuilder' ui props value result
  -> FormBuilder' ui' props value result
mapUI f form =
  FormBuilder \props value ->
    let
      { edit, validate } = un FormBuilder form props value
    in
      { edit: f props value validate <<< edit
      , validate
      }

-- | Make the props available. This allows for changing the structure of a form
-- | builder based on the current props.
withProps
  :: forall ui props unvalidated result
   . (props -> FormBuilder' ui props unvalidated result)
  -> FormBuilder' ui props unvalidated result
withProps f = FormBuilder \props value -> un FormBuilder (f props) props value

-- | Make the value available. This allows for changing the structure of a form
-- | builder based on the current value.
withValue
  :: forall ui props unvalidated result
   . (unvalidated -> FormBuilder' ui props unvalidated result)
  -> FormBuilder' ui props unvalidated result
withValue f = FormBuilder \props value -> un FormBuilder (f value) props value

-- | As 'indent' but takes any 'JSX' as the label
jsxIndent
  :: forall props u a
   . JSX
  -> RequiredField
  -> FormBuilder props u a
  -> FormBuilder props u a
jsxIndent label required editor = FormBuilder \props val ->
  let { edit, validate } = un FormBuilder editor props val
   in { edit: \k ->
          pure $ Node
            { label: label
            , key: Nothing
            , required: required
            , validationError: Nothing
            , children: edit k
            }
      , validate
      }

-- | Indent a `Forest` of editors by one level, providing a label.
indent
  :: forall props u a
   . String
  -> RequiredField
  -> FormBuilder props u a
  -> FormBuilder props u a
indent label required editor = jsxIndent (R.text label) required editor

wrap
  :: forall props u a
   . (Array JSX -> JSX)
  -> FormBuilder props u a
  -> FormBuilder props u a
wrap f form =
  FormBuilder \props value ->
    let
      { edit, validate } = un FormBuilder form props value
    in
      { edit: \k ->
          pure $ Wrapper
            { key: Nothing
            , wrap: f
            , children: edit k
            }
      , validate
      }

-- | Filter parts of the form based on the current value (and the props).
filterWithProps
  :: forall ui props u a
   . Monoid ui
  => (props -> u -> Boolean)
  -> FormBuilder' ui props u a
  -> FormBuilder' ui props u a
filterWithProps p editor = FormBuilder \props value ->
  let { edit, validate } = un FormBuilder editor props value
   in { edit: \onChange ->
          if p props value
            then edit onChange
            else mempty
      , validate
      }

-- | TODO: document
withKey
  :: forall props u a
   . String
  -> FormBuilder props u a
  -> FormBuilder props u a
withKey key editor = FormBuilder \props value ->
  let { edit, validate } = un FormBuilder editor props value
   in { edit: \onChange ->
          edit onChange # Array.mapWithIndex case _, _ of
            i, Child a -> Child a { key = Just (key <> "--" <> show i) }
            i, Wrapper a -> Wrapper a { key = Just (key <> "--" <> show i) }
            i, Node  n -> Node  n { key = Just (key <> "--" <> show i) }
      , validate
      }

styles :: JSS
styles = jss
  { "@global":
      { ".lumi-form":
          { "& hr.lumi.field-divider":
              { margin: "8px 0"
              , height: "0"
              , display: "none"

              , "@media (max-width: 860px)":
                  { margin: "4px 0"
                  }
              }

            -- hide outer borders in nested Forms
          , "& .labeled-field--right > .lumi-form > hr.lumi.field-divider":
              { "&:first-child, &:last-child": { display: "none" }
              }

          , "&.readonly label.lumi":
              { cursor: "auto"
              , userSelect: "auto"
              }

            -- TODO: this will probably just be handled by lumi Link styles
          , "& a.lumi":
              { color: cssStringHSLA colors.primary
              , cursor: "pointer"
              , "&:hover": { textDecoration: "underline" }
              }

            -- not InlineTable Form rules
          , "& .labeled-field":
              { paddingBottom: "16px"

              , "&[data-force-top-label=\"true\"] lumi-align-to-input":
                  { padding: "0 0 4px 0"
                  }
              }

          , "&.inline-table":
                -- If necessary, override the not(.inline-table)
                -- rule above (for nested forms)
              { "& .labeled-field":
                  { paddingBottom: "0"

                  , "&[data-force-top-label=\"true\"] lumi-align-to-input":
                      { padding: "0"
                      }
                  }

              , "& hr.lumi.field-divider":
                  { height: "1px"
                  , display: "block"
                  }

              , "& label.lumi:not(:hover)":
                  { "& input.lumi, & select.lumi":
                      { borderColor: "transparent"
                      }
                  }

              , "& lumi-body.field-label":
                  { color: cssStringHSLA colors.black1
                  }
              }

          , "& .labeled-field--validation-error":
              { extend: labeledFieldValidationErrorStyles
              , marginBottom: "calc(4 * 4px)"
              }

          , "& .labeled-field--validation-warning":
              { extend: labeledFieldValidationWarningStyles
              , marginBottom: "calc(4 * 4px)"
              }

          , "& lumi-editable-table":
              { "& table tr td:not(.lumi)":
                  { verticalAlign: "top"
                  , "& lumi-column":
                      { flex: "1 1 auto"
                      }
                  }
              , "& .labeled-field--validation-error, & .labeled-field--validation-warning":
                  { marginBottom: "0"
                  }
              }
          }
      }
  }
