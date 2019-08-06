module Lumi.Components.Examples.Wizard where

import Prelude

import Data.Either (Either(..), isRight)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Data.String.NonEmpty (toString) as NES
import Data.Symbol (SProxy(..))
import Effect.Uncurried (mkEffectFn1)
import Lumi.Components.Button as Button
import Lumi.Components.Column (column, column_)
import Lumi.Components.Form (FormBuilder, Validated)
import Lumi.Components.Form as F
import Lumi.Components.Form.Defaults (formDefaults)
import Lumi.Components.Input as Input
import Lumi.Components.LabeledField (RequiredField(..))
import Lumi.Components.Row (row)
import Lumi.Components.Text as T
import Lumi.Components.Wizard (Wizard, WizardStep)
import Lumi.Components.Wizard as W
import Lumi.Components.Example (example)
import React.Basic (JSX, createComponent, make)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture, capture_)

data Action a
  = OnChange (FormData -> FormData)
  | SetModified
  | SetWizardStep (WizardStep Step { readonly :: Boolean } FormData Result)
  | Finalize Result

docs :: JSX
docs = unit # make (createComponent "WizardExample") { initialState, render }
  where
    initialState =
      { formData: formDefaults :: FormData
      , result: Nothing
      , wizardStep: W.liftStep exampleWizard
      }

    send self =
      case _ of
        OnChange modify ->
          self.setState _ { formData = modify self.state.formData }

        SetModified ->
          case W.stepIdentifier self.state.wizardStep of
            Just FirstStep -> self.setState \state -> state { formData { firstStep = F.setModified state.formData.firstStep } }
            Just SecondStep -> self.setState \state -> state { formData { secondStep = F.setModified state.formData.secondStep } }
            Just ThirdStep -> self.setState \state -> state { formData { thirdStep = F.setModified state.formData.thirdStep } }
            Nothing -> pure unit

        SetWizardStep wizardStep ->
          self.setState _ { wizardStep = wizardStep }

        Finalize result ->
          self.setState _ { result = Just result }

    render self@{ state: { formData, result, wizardStep } } =
      column_
        [ example $ column
            { style: R.css { alignSelf: "stretch" }
            , children:
                case result of
                  Nothing ->
                    [ W.wizard
                        { step: wizardStep
                        , value: formData
                        , onChange: send self <<< OnChange
                        , forceTopLabels: false
                        , inlineTable: false
                        , formProps: { readonly: false }
                        }
                    , row
                        { style: R.css { justifyContent: "flex-end" }
                        , children:
                            let
                              -- Applying `W.previousStep` to a given `WizardStep` returns
                              -- the previous `WizardStep`, in the case there is one.
                              previousStepM = W.previousStep wizardStep
                              -- Applying `W.resumeStep` to a given `WizardStep` returns
                              -- either the next step, or the final result, if the current
                              -- step is valid.
                              nextStepM = W.resumeStep wizardStep { readonly: false } formData
                              hasFinalResult = maybe false isRight nextStepM
                            in
                              [ Button.button Button.secondary
                                  { title = "Back"
                                  , buttonState = if isNothing previousStepM then Button.Disabled else Button.Enabled
                                  , onPress = maybe (mkEffectFn1 mempty) (capture identity <<< const <<< send self <<< SetWizardStep) previousStepM
                                  }
                              , Button.button Button.primary
                                  { title = if hasFinalResult then "Submit" else "Next"
                                  , style = R.css { marginLeft: "12px" }
                                  , onPress = capture_
                                      case nextStepM of
                                        Nothing -> send self SetModified
                                        Just (Left nextStep) -> send self (SetWizardStep nextStep)
                                        Just (Right r) -> send self (Finalize r)
                                  }
                              ]
                        }
                    ]
                  Just { text, occupation } ->
                    [ T.p_ "Dilly dily data, my magic's a done! I'm a Wizard, after all."
                    , T.p_ text
                    , T.p_ "From all this information, I can only conclude that yourself are a:"
                    , T.text T.sectionHeader
                        { style = R.css { textAlign: "center" }
                        , children = [ R.text "PureScript programmer!" ]
                        }
                    ]
            }
        ]

-- Example Wizard
data Step = FirstStep | SecondStep | ThirdStep
derive instance eqStep :: Eq Step

type FormData =
  { firstStep :: FirstStepFormData
  , secondStep :: SecondStepFormData
  , thirdStep :: ThirdStepFormData
  }

type Result =
  { text :: String
  , occupation :: String
  }

exampleWizard :: Wizard Step { readonly :: Boolean } FormData Result
exampleWizard = do
  firstStep <-
    W.step FirstStep
    $ F.focus (prop (SProxy :: SProxy "firstStep"))
    $ firstStepForm

  secondStep <-
    W.step SecondStep
    $ F.focus (prop (SProxy :: SProxy "secondStep"))
    $ secondStepForm firstStep.age

  thirdStep <-
    W.step ThirdStep
    $ F.focus (prop (SProxy :: SProxy "thirdStep"))
    $ thirdStepForm firstStep secondStep

  let { firstName, lastName, age } = firstStep
  let { country, workFromHome, height, favoriteColor } = secondStep

  pure
    { text:
        "I know now that your name is " <> firstName <> " " <> lastName
        <> maybe "" (\a -> ", you are " <> show a <> " years old") age
        <> ", live in " <> country
        <> " and " <> (if workFromHome then "do" else "do not") <> " work from home."
        <> " Also, "
        <> maybe "" (\h -> " you are " <> show h <> " inches tall, and ") height
        <> " your favorite color is " <> favoriteColor
        <> "."
    , occupation: "PureScript programmer"
    }

-- First step
type FirstStepFormData =
  { firstName :: Validated String
  , lastName :: Validated String
  , age :: Validated String
  }

type FirstStepResult =
  { firstName :: String
  , lastName :: String
  , age :: Maybe Int
  }

firstStepForm :: forall props. FormBuilder { readonly :: Boolean | props } FirstStepFormData FirstStepResult
firstStepForm = ado
  firstName <-
    F.indent "First name" Required
    $ F.focus (prop (SProxy :: SProxy "firstName"))
    $ F.validated (F.nonEmpty "First name")
    $ F.textbox

  lastName <-
    F.indent "Last name" Required
    $ F.focus (prop (SProxy :: SProxy "lastName"))
    $ F.validated (F.nonEmpty "Last name")
    $ F.textbox

  age <-
    F.indent "Age" Neither
    $ F.focus (prop (SProxy :: SProxy "age"))
    $ F.validated (F.optional (F.validInt "Age"))
    $ F.number
        { min: Just 0.0
        , max: Nothing
        , step: Input.Step 1.0
        }

  in { firstName: NES.toString firstName, lastName: NES.toString lastName, age }

-- Second step
type SecondStepFormData =
  { country :: Validated String
  , workFromHome :: Boolean
  , height :: Validated String
  , favoriteColor :: Validated (Maybe String)
  }

type SecondStepResult =
  { country :: String
  , workFromHome :: Boolean
  , height :: Maybe Number
  , favoriteColor :: String
  }

secondStepForm :: forall props. Maybe Int -> FormBuilder { readonly :: Boolean | props } SecondStepFormData SecondStepResult
secondStepForm age = ado
  country <-
    F.indent "Country" Required
    $ F.focus (prop (SProxy :: SProxy "country"))
    $ F.validated (F.nonEmpty "Country")
    $ F.textbox

  workFromHome <-
    if isJust age && age < Just 15
      then pure false
      else
        F.indent "Work from home?" Neither
        $ F.focus (prop (SProxy :: SProxy "workFromHome"))
        $ F.switch

  height <-
    F.indent "Height (in)" Optional
    $ F.focus (prop (SProxy :: SProxy "height"))
    $ F.validated (F.optional (F.validNumber "Height"))
    $ F.number { min: Just 0.0, max: Nothing, step: Input.Any }

  favoriteColor <-
    F.indent "Favorite color" Required
    $ F.focus (prop (SProxy :: SProxy "favoriteColor"))
    $ F.validated (F.nonNull "Favorite color")
    $ F.select identity pure
        [ { label: "Red", value: "red" }
        , { label: "Green", value: "green" }
        , { label: "Blue", value: "blue" }
        ]

  in { country: NES.toString country, workFromHome, height, favoriteColor }

-- Third step
type ThirdStepFormData =
  { hasAdditionalInfo :: Boolean
  , additionalInfo :: Validated String
  }

type ThirdStepResult =
  { hasAdditionalInfo :: Boolean
  , additionalInfo :: Maybe String
  }

thirdStepForm
  :: forall props
   . FirstStepResult
  -> SecondStepResult
  -> FormBuilder { readonly :: Boolean | props } ThirdStepFormData ThirdStepResult
thirdStepForm { firstName, lastName, age } { country, workFromHome, height, favoriteColor } = F.parallel "thirdStepForm" do
  F.sequential "text"
    $ F.static
    $ T.p_
      $ "Alright! What I know so far is that your name is " <> firstName <> " " <> lastName
        <> maybe "" (\a -> ", you are " <> show a <> " years old") age
        <> ", live in " <> country
        <> " and " <> (if workFromHome then "do" else "do not") <> " work from home."
        <> " Also, "
        <> maybe "" (\h -> " you are " <> show h <> " inches tall, and ") height
        <> " your favorite color is " <> favoriteColor
        <> "."

  hasAdditionalInfo <- F.sequential "hasAdditionalInfo"
    $ F.indent "Do you want to give me additional information?" Neither
    $ F.focus (prop (SProxy :: SProxy "hasAdditionalInfo"))
    $ F.switch

  additionalInfo <- F.sequential "additionalInfo"
    $ if not hasAdditionalInfo
        then pure Nothing
        else
          F.indent "Additional information" Required
          $ F.focus (prop (SProxy :: SProxy "additionalInfo"))
          $ map (Just <<< NES.toString)
          $ F.validated (F.nonEmpty "Additional info")
          $ F.textarea

  pure { hasAdditionalInfo, additionalInfo }
