module Lumi.Components.Examples.Form where

import Prelude

import Color (cssStringHSLA)
import Control.Coroutine.Aff (close, emit, produceAff)
import Control.MonadZero (guard)
import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Int as Int
import Data.Lens (iso)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid as Monoid
import Data.Newtype (class Newtype, un)
import Data.Nullable as Nullable
import Data.String as String
import Data.String.NonEmpty (NonEmptyString, appendString, length, toString)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, error, throwError)
import Effect.Class (liftEffect)
import Effect.Random (randomRange)
import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components.Button as Button
import Lumi.Components.Color (colors)
import Lumi.Components.Column (column, column_)
import Lumi.Components.Example (example)
import Lumi.Components.Form (FormBuilder, Validated)
import Lumi.Components.Form as F
import Lumi.Components.Form.Defaults (formDefaults)
import Lumi.Components.Form.Table as FT
import Lumi.Components.Input as Input
import Lumi.Components.LabeledField (RequiredField(..))
import Lumi.Components.Modal (dialog)
import Lumi.Components.Row (row)
import Lumi.Components.Size (Size(..))
import Lumi.Components.Text as T
import Lumi.Components.Textarea as Textarea
import Lumi.Components.Upload (FileId(..))
import Lumi.Components.Upload as Upload
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, (/\))
import React.Basic.Hooks as React
import Web.File.File as File

docs :: JSX
docs = flip element {} $ unsafePerformEffect do

  userFormExample <- mkUserFormExample

  component "FormExample" \_ -> React.do
    { formData, form } <- F.useForm metaForm
        { initialState: formDefaults
        , inlineTable: false
        , forceTopLabels: false
        , formProps: { readonly: false }
        }

    pure $ column_
      [ column
          { style: css { width: "100%", maxWidth: 300, padding: "20px 0" }
          , children: [ form ]
          }

      , example $ element userFormExample formData
      ]

-- | This form renders the toggles at the top of the example
metaForm
  :: forall props
   . FormBuilder
      { readonly :: Boolean
      | props
      }
      { inlineTable :: Boolean
      , forceTopLabels :: Boolean
      , readonly :: Boolean
      , simulatePauses :: Boolean
      }
      Unit
metaForm = ado
  inlineTable <-
    F.indent "Inline table" Neither
    $ F.focus (prop (SProxy :: SProxy "inlineTable"))
    $ F.switch
  forceTopLabels <-
    F.indent "Force top labels" Neither
    $ F.focus (prop (SProxy :: SProxy "forceTopLabels"))
    $ F.switch
  readonly <-
    F.indent "Readonly" Neither
    $ F.focus (prop (SProxy :: SProxy "readonly"))
    $ F.switch
  simulatePauses <-
    F.indent "Simulate pauses (pet color picker)" Neither
    $ F.focus (prop (SProxy :: SProxy "simulatePauses"))
    $ F.switch
  in unit

mkUserFormExample
  :: Effect
       ( ReactComponent
           { inlineTable :: Boolean
           , forceTopLabels :: Boolean
           , readonly :: Boolean
           , simulatePauses :: Boolean
           }
       )
mkUserFormExample = do
  component "UserFormExample" \props -> React.do
    userDialog /\ setUserDialog <- useState Nothing

    { setModified, reset, validated, form } <- F.useForm userForm
        { initialState: formDefaults
        , inlineTable: props.inlineTable
        , forceTopLabels: props.forceTopLabels && not props.inlineTable
        , formProps:
            { readonly: props.readonly
            , simulatePauses: props.simulatePauses
            }
        }

    pure $ R.form -- Forms should be enclosed in a single "<form>" element to enable
                  -- default browser behavior, such as the enter key. Use "type=submit"
                  -- on the form's submit button and `preventDefault` to keep the browser
                  -- from reloading the page on submission.
      { onSubmit: handler preventDefault \_ ->
          case validated of
            Nothing ->
              setModified
            Just { firstName, lastName } ->
              setUserDialog \_ -> Just { firstName, lastName }
      , style: R.css { alignSelf: "stretch" }
      , children:
          [ form
          , row
              { style: R.css { justifyContent: "flex-end" }
              , children:
                  [ Button.button Button.secondary
                      { title = "Reset"
                      , onPress = handler_ reset
                      }
                  , Button.button Button.primary
                      { title = "Submit"
                      , type = "submit"
                      , style = R.css { marginLeft: "12px" }
                      }
                  ]
              }
          , case userDialog of
              Nothing ->
                mempty
              Just { firstName, lastName } ->
                dialog
                  { modalOpen: true
                  , onRequestClose: do
                      reset
                      setUserDialog \_ -> Nothing
                  , onActionButtonClick: Nullable.null
                  , actionButtonTitle: ""
                  , size: Medium
                  , children: R.text $
                      "Created user " <> toString firstName <> " " <> toString lastName <> "!"
                  }
          ]
      }

data Country
  = BR
  | US

countryToString :: Country -> String
countryToString BR = "Brazil"
countryToString US = "United States"

countryFromString :: String -> Maybe Country
countryFromString "Brazil" = Just BR
countryFromString "United States" = Just US
countryFromString _    = Nothing

newtype State = State String
derive instance newtypeState :: Newtype State _

statesForCountry :: Country -> Array State
statesForCountry BR = map State ["Ceará", "Minas Gerais", "São Paulo"]
statesForCountry US = map State ["Arizona", "California", "New York"]

type User =
  { firstName :: Validated String
  , lastName :: Validated String
  , password ::
      { password1 :: Validated String
      , password2 :: Validated String
      }
  , admin :: Boolean
  , checkbox :: Boolean
  , descriptiveCheckbox :: Boolean
  , height :: Validated String
  , addresses :: Validated (Array Address)
  , pets :: Validated (Array Pet)
  , leastFavoriteColors :: Array String
  , notes :: String
  , avatar :: Maybe Upload.FileId
  }

type ValidatedUser =
  { firstName :: NonEmptyString
  , lastName :: NonEmptyString
  , password :: NonEmptyString
  , admin :: Boolean
  , checkbox :: Boolean
  , descriptiveCheckbox :: Boolean
  , height :: Maybe Number
  , addresses :: Array ValidatedAddress
  , pets :: Array ValidatedPet
  , leastFavoriteColors :: Array String
  , notes :: String
  , avatar :: Maybe Upload.FileId
  }

type Pet =
  { firstName :: Validated String
  , lastName :: Validated String
  , animal :: Validated (Maybe String)
  , age :: Validated String
  , color :: Maybe String
  }

type ValidatedPet =
  { name :: NonEmptyString
  , animal :: String
  , age :: Int
  , color :: Maybe String
  }

userForm
  :: forall props
   . FormBuilder
      { readonly :: Boolean
      , simulatePauses :: Boolean
      | props
      }
      User
      ValidatedUser
userForm = ado
  firstName <-
    F.indent "First Name" Required
    $ F.focus (prop (SProxy :: SProxy "firstName"))
    $ F.warn (\x ->
        Monoid.guard
          (length x <= 2)
          (pure "First name should be longer than two characters (but it doesn't have to be).")
      )
    $ F.validated (F.nonEmpty "First name")
    $ F.textbox
  lastName <-
    F.indent "Last Name" Required
    $ F.focus (prop (SProxy :: SProxy "lastName"))
    $ F.validated (F.nonEmpty "Last name")
    $ F.textbox
  password <-
    F.focus (prop (SProxy :: SProxy "password"))
    $ F.parallel "password" do
        password1 <- F.sequential "password1"
          $ F.indent "Password" Required
          $ F.focus (prop (SProxy :: SProxy "password1"))
          $ F.validated (F.nonEmpty "Password")
          $ F.passwordBox
        password2 <- F.sequential "password2"
          $ F.indent "Confirm password" Required
          $ F.focus (prop (SProxy :: SProxy "password2"))
          $ F.validated (F.mustEqual (toString password1) "Passwords do not match.")
          $ F.passwordBox
        pure password1
  admin <-
    F.indent "Admin?" Neither
    $ F.focus (prop (SProxy :: SProxy "admin"))
    $ F.switch
  checkbox <-
    F.indent "Checked?" Neither
    $ F.focus (prop (SProxy :: SProxy "checkbox"))
    $ F.checkbox
  descriptiveCheckbox <-
    F.focus (prop (SProxy :: SProxy "descriptiveCheckbox"))
    $ F.labeledCheckbox
    $ column
        { style: R.css { maxWidth: "300px" }
        , children:
            [ T.body_ "This is a right aligned description"
            , T.text T.subtext
                { style = R.css { color: cssStringHSLA colors.black1 }
                , children = [ R.text "with a sublabel that reads \"Lorem ipsum dolor sit amet, consectetur adipiscing elit.\"" ]
                }
            ]
        }

  F.section "Personal data"
  height <-
    F.indent "Height (in)" Optional
    $ F.focus (prop (SProxy :: SProxy "height"))
    $ F.validated (F.optional (F.validNumber "Height"))
    $ F.number { min: Nothing, max: Nothing, step: Input.Any }
  addresses <-
    F.focus (prop (SProxy :: SProxy "addresses"))
    $ F.warn (\as ->
        Monoid.guard (Array.null as) (pure "No address added.")
      )
    $ F.array
        { label: "Address"
        , addLabel: "Add address"
        , defaultValue: formDefaults
        , editor: addressForm
        }
  leastFavoriteColors <-
    F.indent "Least Favorite Colors" Neither
    $ F.focus (prop (SProxy :: SProxy "leastFavoriteColors"))
    $ F.multiSelect show
    $ map (\x -> { label: x, value: x })
    $ [ "Beige"
      , "Fuchsia"
      , "Goldenrod"
      , "Magenta"
      , "Puce"
      , "Slate"
      ]
  notes <-
    F.indent "Notes" Optional
    $ F.focus (prop (SProxy :: SProxy "notes"))
    $ F.textarea
  notes <-
    F.indent "Notes (with placeholder)" Optional
    $ F.focus (prop (SProxy :: SProxy "notes"))
    $ F.textarea_ $ Textarea.textareaPlaceholderText_ "Placeholder text..."

  F.section "Pets"
  pets <-
    F.focus (prop (SProxy :: SProxy "pets"))
    $ F.warn (\pets ->
        Monoid.guard (Array.null pets) (pure "You should adopt a pet.")
      )
    $ FT.editableTable
        { addLabel: "Add pet"
        , defaultValue: Just
            { firstName: F.Fresh ""
            , lastName: F.Fresh ""
            , animal: F.Fresh Nothing
            , age: F.Fresh "1"
            , color: Nothing
            }
        , maxRows: top
        , summary: mempty
        , formBuilder: ado
            name <- FT.column_ "Name" ado
              firstName <-
                F.focus (prop (SProxy :: SProxy "firstName"))
                $ F.validated (F.nonEmpty "First name")
                $ F.textbox
              lastName <-
                F.focus (prop (SProxy :: SProxy "lastName"))
                $ F.warn (\lastName -> do
                    guard (not String.null lastName)
                    pure "Did you really give your pet a surname?"
                  )
                $ F.textbox
              in
                appendString firstName
                  $ foldMap (" " <> _)
                  $ Monoid.guard (not String.null lastName)
                  $ Just lastName
            animal <-
              FT.column_ "Animal"
              $ F.focus (prop (SProxy :: SProxy "animal"))
              $ F.validated (F.nonNull "Animal")
              $ F.select identity pure
              $ map (\value -> { label: value, value })
                  [ "Bird"
                  , "Cat"
                  , "Cow"
                  , "Dog"
                  , "Duck"
                  , "Fish"
                  , "Horse"
                  , "Rabbit"
                  , "Rat"
                  , "Turle"
                  ]
            age <-
              FT.column_ "Age"
              $ F.focus (prop (SProxy :: SProxy "age"))
              $ F.validated (F.validInt "Age")
              $ F.number
                  { step: Input.Step 1.0
                  , min: Just 0.0
                  , max: Nothing
                  }
            color <-
              FT.column_ "Color"
              $ F.withProps \props ->
                  F.focus (prop (SProxy :: SProxy "color"))
                  $ F.asyncSelectByKey
                      (loadColor props.simulatePauses)
                      (loadColors props.simulatePauses)
                      identity
                      identity
                      identity
                      (R.text <<< _.label)
            in
              { name
              , animal
              , age
              , color
              }
        }

  F.section "Images"
  avatar <-
    F.indent "Avatar" Optional
    $ F.focus (prop (SProxy :: SProxy "avatar"))
    $ F.match_ (iso (maybe [] pure) Array.head)
    $ F.file
        { variant: Upload.Avatar
        , backend:
            { fetch: \id ->
                pure { id, name: Upload.FileName "avatar", previewUri: Nothing }
            , upload: \file -> produceAff \emitter -> do
                let
                  totalBytes = Int.round $ File.size file
                  progress = { totalBytes, uploadedBytes: 0 }
                randomPause
                emit emitter progress
                randomPause
                emit emitter progress { uploadedBytes = totalBytes / 8 }
                randomPause
                emit emitter progress { uploadedBytes = totalBytes / 2 }
                randomPause
                emit emitter progress { uploadedBytes = totalBytes }

                close emitter $ pure $ FileId $ File.name file
            }
        }
  in
    { firstName
    , lastName
    , password
    , admin
    , checkbox
    , descriptiveCheckbox
    , height
    , pets
    , leastFavoriteColors
    , addresses
    , notes
    , avatar
    }
  where
    randomPause = do
      interval <- liftEffect $ randomRange 100.0 700.0
      delay $ Milliseconds interval

    loadColor simulatePauses c = do
      when simulatePauses do
        randomPause
      case String.toLower c of
        "red" -> pure { label: "Red", value: "red" }
        "green" -> pure { label: "Green", value: "green" }
        "blue" -> pure { label: "Blue", value: "blue" }
        _ -> throwError (error "No color")

    loadColors simulatePauses search = do
      when simulatePauses do
        randomPause
        randomPause
      pure
        [ { label: "Red", value: "red" }
        , { label: "Green", value: "green" }
        , { label: "Blue", value: "blue" }
        ]

type Address =
  { name :: Validated String
  , street :: Validated String
  , city :: Validated String
  , country :: Validated (Maybe Country)
  , state :: Validated (Maybe State)
  }

type ValidatedAddress =
  { name :: NonEmptyString
  , street :: NonEmptyString
  , city :: NonEmptyString
  , country :: Country
  , state :: State
  }

addressForm
  :: forall props
   . FormBuilder
       { readonly :: Boolean | props }
       Address
       ValidatedAddress
addressForm = ado
  name <-
    F.indent "Name" Required
    $ F.focus (prop (SProxy :: SProxy "name"))
    $ F.validated (F.nonEmpty "Name")
    $ F.textbox
  street <-
    F.indent "Street" Required
    $ F.focus (prop (SProxy :: SProxy "street"))
    $ F.validated (F.nonEmpty "Street")
    $ F.textbox
  city <-
    F.indent "City" Required
    $ F.focus (prop (SProxy :: SProxy "city"))
    $ F.validated (F.nonEmpty "City")
    $ F.textbox
  { country, state } <- F.parallel "countryState" do
      country <- F.sequential "country"
        $ F.indent "Country" Neither
        $ F.focus (prop (SProxy :: SProxy "country"))
        $ F.validated (F.nonNull "Country")
        $ countryFormBuilder
      state <- F.sequential "state"
        $ F.indent "State" Neither
        $ F.focus (prop (SProxy :: SProxy "state"))
        $ F.validated (F.nonNull "State")
        $ stateFormBuilder country
      pure { country, state }
  in
    { name
    , street
    , city
    , country
    , state
    }
  where
    countryFormBuilder =
      F.select countryToString countryFromString
        [ { label: countryToString BR
          , value: BR
          }
        , { label: countryToString US
          , value: US
          }
        ]

    stateFormBuilder country =
      F.select (un State) (pure <<< State) $
        statesForCountry country <#> \state ->
          { label: un State state
          , value: state
          }
