module Lumi.Components.Form.Internal where

import Prelude

import Control.Alternative (class Alt, class Alternative, class Plus)
import Control.Parallel.Class (class Parallel)
import Data.Array as Array
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Effect (Effect)
import Effect.Aff (Aff, runAff_)
import Effect.Exception (throwException)
import Lumi.Components.LabeledField (RequiredField, ValidationMessage)
import Prim.TypeError (class Warn, Above, Text)
import React.Basic.Classic (JSX, fragment, keyed)

data Tree
  = Child
      { key :: Maybe String
      , child :: JSX
      }
  | Wrapper
      { key :: Maybe String
      , wrap :: Array JSX -> JSX
      , children :: Forest
      }
  | Node
      { label :: JSX
      , key :: Maybe String
      , required :: RequiredField
      , validationError :: Maybe ValidationMessage
      , children :: Forest
      }

type Forest = Array Tree

-- | Traverse a tree bottom-up, removing all "internal" nodes (i.e. `Wrapper`
-- | or `Node` constructors) which have empty `children` arrays. In the case
-- | where there's nothing left in the tree after pruning, we return `Nothing`.
-- |
-- | We need to perform the traversal bottom-up because, for example, a subtree
-- | such as
-- |
-- | ```
-- | let
-- |   w children = Wrapper { key: Nothing, children }
-- | in
-- |   w [w []]
-- | ```
-- |
-- | should be pruned, but a top-down operation would not be able to identify
-- | such a subtree as prunable.
pruneTree :: Tree -> Maybe Tree
pruneTree =
  case _ of
    t@(Child _) ->
      Just t
    Wrapper r@{ children } ->
      case Array.mapMaybe pruneTree children of
        [] ->
          Nothing
        children' ->
          Just (Wrapper r { children = children' })
    Node r@{ children } ->
      case Array.mapMaybe pruneTree children of
        [] ->
          Nothing
        children' ->
          Just (Node r { children = children' })

-- | An applicative functor which can be used to build forms.
-- | Forms can be turned into components using the `build` function.
newtype FormBuilder' ui props unvalidated result = FormBuilder
  (props
  -- ^ additional props
  -> unvalidated
  -- ^ the current value
  -> { edit :: ((unvalidated -> unvalidated) -> Effect Unit) -> ui
     , validate :: Maybe result
     })

type FormBuilder props unvalidated result = FormBuilder' Forest props unvalidated result

derive instance newtypeFormBuilder :: Newtype (FormBuilder' ui props unvalidated result) _

derive instance functorFormBuilder :: Functor (FormBuilder' ui props unvalidated)

instance applyFormBuilder :: Semigroup ui => Apply (FormBuilder' ui props unvalidated) where
  apply (FormBuilder f) (FormBuilder x) = FormBuilder \props unvalidated ->
    let { edit: editF, validate: validateF } = f props unvalidated
        { edit: editX, validate: validateX } = x props unvalidated
     in { edit: \k -> editF k <> editX k
        , validate: validateF <*> validateX
        }

instance applicativeFormBuilder :: Monoid ui => Applicative (FormBuilder' ui props unvalidated) where
  pure a = FormBuilder \_ _ ->
    { edit: mempty
    , validate: pure a
    }

instance parallelFormBuilder
  :: Warn
      ( Above
          (Text "The `Parallel` instance to `FormBuilder` is deprecated.")
          (Text "Prefer using `Form.parallel` and `Form.sequential` instead.")
      )
  => Parallel (FormBuilder' (Array Tree) props unvalidated) (SeqFormBuilder' (Array Tree) props unvalidated) where
  parallel (SeqFormBuilder (FormBuilder f)) = FormBuilder \props value ->
    let { edit, validate } = f props value
     in { edit: \onChange ->
            [ Wrapper
                { key: Just "seq"
                , wrap: keyed "seq" <<< fragment
                , children: edit onChange
                }
            ]
        , validate: validate
        }
  sequential = SeqFormBuilder

parallel :: forall props value. String -> SeqFormBuilder props value ~> FormBuilder props value
parallel key (SeqFormBuilder (FormBuilder f)) = FormBuilder \props value ->
  let { edit, validate } = f props value
   in { edit: \onChange ->
          [ Wrapper
              { key: Just key
              , wrap: keyed key <<< fragment
              , children: edit onChange
              }
          ]
      , validate: validate
      }

sequential :: forall props value. String -> FormBuilder props value ~> SeqFormBuilder props value
sequential key (FormBuilder f) = SeqFormBuilder $ FormBuilder \props value ->
  let { edit, validate } = f props value
   in { edit: \onChange ->
          [ Wrapper
              { key: Just key
              , wrap: keyed key <<< fragment
              , children: edit onChange
              }
          ]
      , validate: validate
      }

-- | A form builder where each field depends on the validity of the previous ones.
-- | That is, every field is only displayed if all the previous ones are valid.
-- | Forms can be turned into components using the `build` function.
newtype SeqFormBuilder' ui props unvalidated result =
  SeqFormBuilder (FormBuilder' ui props unvalidated result)

type SeqFormBuilder props unvalidated result = SeqFormBuilder' Forest props unvalidated result

derive instance newtypeSeqFormBuilder :: Newtype (SeqFormBuilder' ui props unvalidated result) _
derive newtype instance functorSeqFormBuilder :: Functor (SeqFormBuilder' ui props unvalidated)

instance applySeqFormBuilder :: Monoid ui => Apply (SeqFormBuilder' ui props unvalidated) where
  apply = ap

derive newtype instance applicativeSeqFormBuilder :: Monoid ui => Applicative (SeqFormBuilder' ui props unvalidated)

instance bindSeqFormBuilder :: Monoid ui => Bind (SeqFormBuilder' ui props unvalidated) where
  bind (SeqFormBuilder f) g =
    SeqFormBuilder $ FormBuilder \props unvalidated ->
      let { edit: editF, validate: validateF } = (un FormBuilder f) props unvalidated
      in
        case g <$> validateF of
          Nothing ->
            { edit: editF, validate: Nothing }
          Just (SeqFormBuilder x) ->
            let { edit: editX, validate: validateX } = (un FormBuilder x) props unvalidated
             in { edit: \k -> editF k <> editX k
                 , validate: validateX
                 }

instance monadSeqFormBuilder :: Monoid ui => Monad (SeqFormBuilder' ui props unvalidated)

instance altSeqFormBuilder :: Monoid ui => Alt (SeqFormBuilder' ui props unvalidated) where
  alt (SeqFormBuilder f) (SeqFormBuilder g) =
    SeqFormBuilder $ FormBuilder \props unvalidated ->
      let rf@{ validate: validateF } = un FormBuilder f props unvalidated
          rg@{ validate: validateG } = un FormBuilder g props unvalidated
       in case validateF, validateG of
            Just _, _ -> rf
            _, _ -> rg

instance plusSeqFormBuilder :: Monoid ui => Plus (SeqFormBuilder' ui props unvalidated) where
  empty = SeqFormBuilder $ FormBuilder \_ _ -> { edit: mempty, validate:  Nothing }

instance alternativeSeqFormBuilder :: Monoid ui => Alternative (SeqFormBuilder' ui props unvalidated)

-- | Create a `FormBuilder` from a function which produces a form
-- | element as `JSX` and a validated result.
formBuilder
  :: forall props unvalidated a
   . (props
      -> unvalidated
      -> { edit :: ((unvalidated -> unvalidated) -> Effect Unit) -> JSX
         , validate :: Maybe a
         })
  -> FormBuilder props unvalidated a
formBuilder f =
  FormBuilder \props value ->
    let { edit, validate } = f props value
     in { edit: \onChange -> [ Child { key: Nothing, child: edit onChange } ]
        , validate: validate
        }

-- | The simplest way to create a `FormBuilder`. Create a `FormBuilder`
-- | provided a function that, given the current value and a change callback,
-- | renders a form element as `JSX`.
formBuilder_
  :: forall props a
   . (props -> a -> (a -> Effect Unit) -> JSX)
  -> FormBuilder props a a
formBuilder_ f = formBuilder \props value ->
  { edit: f props value <<< (_ <<< const)
  , validate: pure value
  }

-- | Invalidate a form, keeping its user interface but discarding the result
-- | and possibly changing its type.
invalidate :: forall ui props unvalidated a b. FormBuilder' ui props unvalidated a -> FormBuilder' ui props unvalidated b
invalidate (FormBuilder f) = FormBuilder \props value ->
  { edit: (f props value).edit
  , validate: Nothing
  }

-- | Revalidate the form, in order to display error messages or create
-- | a validated result.
revalidate
  :: forall ui props unvalidated result
   . FormBuilder' ui props unvalidated result
  -> props
  -> unvalidated
  -> Maybe result
revalidate editor props value = (un FormBuilder editor props value).validate

-- | Listens for changes in a form's state and allows for performing
-- | asynchronous effects and additional state changes.
-- |
-- | The asynchronous effect is fired on every state change. Note that from a UI
-- | perspective, the effect is not observably asynchronous: the asynchronous
-- | action is executed in the background, but the wrapped form's UI is not
-- | responsive to the user until the action completes. To use this function
-- | effectively, the supplied callback must return promptly. Setting some React
-- | state in the callback would be appropriate but making network calls should
-- | be done judiciously. Network calls can be justified if they are in response
-- | to infrequent updates (e.g. selecting an item in a dropdown that causes
-- | loading of data specific to that selection) but if the calls are in
-- | response to every digit typed into a number input field for example, a user
-- | may experience delays with their inputs and the backend will be hit
-- | excessively.
listen
  :: forall ui props unvalidated result
   . (unvalidated -> Aff (unvalidated -> unvalidated))
  -> FormBuilder' ui props unvalidated result
  -> FormBuilder' ui props unvalidated result
listen cb (FormBuilder f) = FormBuilder \props unvalidated ->
  let { edit, validate } = f props unvalidated
   in { edit: \onChange ->
          edit \update ->
            runAff_ (either throwException onChange) (map (_ <<< update) (cb (update unvalidated)))
      , validate
      }
