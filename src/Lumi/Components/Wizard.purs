module Lumi.Components.Wizard
  ( Wizard
  , step
  , revalidate
  , WizardStep
  , stepIdentifier
  , liftStep
  , resumeStep
  , previousStep
  , gotoStep
  , wizard
  ) where

import Prelude

import Control.Monad.Free (Free, foldFree, liftF, resume)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Effect (Effect)
import Lumi.Components.Form (FormBuilder)
import Lumi.Components.Form as F
import React.Basic.Classic (JSX)

-- | `Form` is the base functor for the `Wizard` language.  It represents a
-- | form as a step of the wizard, taking a parametrized identifier for the
-- | current step, and a function to effectively run the form step.
newtype Form step props value a =
  Form
    { run ::
        props
        -> value
        -> { form :: { forceTopLabels :: Boolean, inlineTable :: Boolean } -> ((value -> value) -> Effect Unit) -> JSX
           , result :: Maybe a
           }
    , step :: step
    }
derive instance functorForm :: Functor (Form step props value)

-- | A `Wizard` is a sequential computation of `FormBuilder`s, representing,
-- | each one, a wizard step that can depend on the result of previous steps to
-- | produce a final result.
-- |
-- | `Wizard` is a monadic DSL that contains only a single instruction: `step`.
newtype Wizard step (props :: Type) value a = Wizard (Free (Form step props value) a)

derive instance newtypeWizard :: Newtype (Wizard step props value a) _
derive newtype instance functorWizard :: Functor (Wizard step props value)
derive newtype instance applyWizard :: Apply (Wizard step props value)
derive newtype instance applicativeWizard :: Applicative (Wizard step props value)
derive newtype instance bindWizard :: Bind (Wizard step props value)
derive newtype instance monadWizard :: Monad (Wizard step props value)

-- | Lift a `FormBuilder` and an associated identifier of type `step` into a
-- | `Wizard step`.
step
  :: forall step props value
   . step
  -> FormBuilder { readonly :: Boolean | props } value
  ~> Wizard step { readonly :: Boolean | props } value
step s f =
  let
    form = F.build f
  in
    Wizard $ liftF $ Form
      { step: s
      , run:
          \formProps value ->
            { form: \{ forceTopLabels, inlineTable } onChange ->
                form
                  { value
                  , onChange
                  , forceTopLabels
                  , inlineTable
                  , formProps
                  }
            , result: F.revalidate f formProps value
            }
      }

-- | Revalidate a `Wizard`, returning the final result if, with the given
-- | `value`, all steps produce valid results.
revalidate :: forall step props value a. Wizard step props value a -> props -> value -> Maybe a
revalidate w props value = foldFree go (un Wizard w)
  where
    go :: Form step props value ~> Maybe
    go (Form form) = (form.run props value).result

-- | `WizardStep` is a suspended `Wizard` computation, that is, a `Wizard`
-- | whose execution has been stopped at some step. It can be thought of as a
-- | `Wizard` zipper.
newtype WizardStep step (props :: Type) value a = WizardStep
  { previous :: Maybe (WizardStep step props value a)
  , current :: Either (Form step props value (Wizard step props value a)) a
  }

derive instance newtypeWizardStep :: Newtype (WizardStep step props value a) _

-- | Retrieve the identifier for the focused step of a `WizardStep`.
-- | Returns `Nothing` if the `WizardStep` represents a finalized `Wizard`.
stepIdentifier :: forall step props value a. WizardStep step props value a -> Maybe step
stepIdentifier (WizardStep { current }) =
  case current of
    Left (Form form) -> Just form.step
    Right _ -> Nothing

-- | Lifts a `Wizard` into a `WizardStep` that focuses on its first step.
liftStep :: forall step props value. Wizard step props value ~> WizardStep step props value
liftStep = WizardStep <<< { previous: Nothing, current: _ } <<< lmap (map Wizard) <<< resume <<< un Wizard

-- | Resume a suspended `WizardStep`, if the current step produces a valid
-- | result, returning either the next `WizardStep` or the final result.
resumeStep
  :: forall step props value a
   . WizardStep step props value a
  -> props
  -> value
  -> Maybe (Either (WizardStep step props value a) a)
resumeStep s@(WizardStep { current }) props value =
  case current of
    Right a -> Just (Right a)
    Left (Form form) ->
      let
        { result } = form.run props value
      in
        case result of
          Nothing -> Nothing
          Just next ->
            case resume (un Wizard next) of
              Right a -> Just (Right a)
              Left _ -> Just $ Left $
                WizardStep
                  { previous: Just s
                  , current: lmap (map Wizard) (resume (un Wizard next))
                  }

-- | Return the previous step, if there is one.
previousStep
  :: forall step props value a
   . WizardStep step props value a
  -> Maybe (WizardStep step props value a)
previousStep = _.previous <<< un WizardStep

-- | Given that the `step` identifiers for the supplied `Wizard` are ordered,
-- | go to the first step in a `Wizard` that is assigned to the given
-- | identifier, if found.
gotoStep
  :: forall step props value a
   . Ord step
  => step
  -> Wizard step props value a
  -> props
  -> value
  -> Maybe (WizardStep step props value a)
gotoStep s w props value = go (liftStep w)
  where
    go :: WizardStep step props value a -> Maybe (WizardStep step props value a)
    go wizardStep@(WizardStep { previous, current }) =
      case current of
        Right a -> Nothing
        Left (Form form) ->
          if form.step >= s
            then Just wizardStep
            else
              let
                { result } = form.run props value
              in
                case result of
                  Nothing -> Nothing
                  Just next ->
                    case resume (un Wizard next) of
                      Right a -> Nothing
                      Left _ ->
                        go $ WizardStep
                          { previous: Just wizardStep
                          , current: lmap (map Wizard) (resume (un Wizard next))
                          }

-- | A component that renders a `WizardStep` (a suspended `Wizard`).
wizard
  :: forall step props value a
   . { step :: WizardStep step props value a
     , value :: value
     , onChange :: (value -> value) -> Effect Unit
     , forceTopLabels :: Boolean
     , inlineTable :: Boolean
     , formProps :: props
     }
  -> JSX
wizard props@{ value, onChange, forceTopLabels, inlineTable, formProps } =
  case (un WizardStep props.step).current of
    Right a ->
      mempty
    Left (Form form') ->
      let
        { form } = form'.run formProps value
      in
        form { forceTopLabels, inlineTable } onChange
