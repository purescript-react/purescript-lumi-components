module Lumi.Components.Form.Validation
  ( Validator
  , nonEmpty, nonEmptyArray, nonNull
  , mustEqual, mustBe
  , validNumber, validInt
  , optional
  , Validated(..)
  , _Validated, _Fresh, _Modified
  , setFresh, setModified
  , ModifyValidated
  , class CanValidate, fresh, modified, fromValidated
  , validated
  , warn
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty (fromArray) as NEA
import Data.Either (Either(..), either, hush, note)
import Data.Eq (class Eq1)
import Data.Foldable (foldMap)
import Data.Int as Int
import Data.Lens (Lens, Prism', lens, over, prism', review, view)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Newtype (un)
import Data.Nullable (notNull)
import Data.Number as Number
import Data.Ord (class Ord1)
import Data.String.NonEmpty (NonEmptyString)
import Data.String.NonEmpty (fromString) as NES
import Heterogeneous.Mapping (class HMap, class MapRecordWithIndex, class Mapping, ConstMapping, hmap, mapping)
import Lumi.Components.Column (column_)
import Lumi.Components.Form.Internal (Forest, FormBuilder(..), Tree(..))
import Lumi.Components.LabeledField (ValidationMessage(..))
import Lumi.Components.Text (subtext, text)
import Prim.RowList as RL
import React.Basic (JSX)
import React.Basic.DOM as R

-- | A `Validator` takes a possibly invalid form `result` and produces
-- | a `valid` result, or an error message.
type Validator result valid =
  result -> Either String valid

-- | A `WarningValidator` can be used to issue a message to the user on
-- | certain form data, but cannot cause the form to fail. Accordingly,
-- | it cannot modify the form data value or type.
type WarningValidator result =
  result -> Maybe String

-- | A `Validator` which verifies that an input string is non-empty.
nonEmpty :: String -> Validator String NonEmptyString
nonEmpty name = note (name <> " is required.") <<< NES.fromString

-- | A `Validator` which verifies that an input array is non-empty.
nonEmptyArray :: forall a. String -> Validator (Array a) (NonEmptyArray a)
nonEmptyArray name = note (name <> " cannot be empty.") <<< NEA.fromArray

-- | A `Validator` which verifies that an optional field is specified.
nonNull :: forall a. String -> Validator (Maybe a) a
nonNull name = note (name <> " is required.")

-- | A `Validator` which verifies that its input equals some value.
mustEqual :: forall a. Eq a => a -> String -> Validator a a
mustEqual value1 = mustBe (_ == value1)

-- | A `Validator` which verifies that its input fulfills a specified condition.
mustBe :: forall a. (a -> Boolean) -> String -> Validator a a
mustBe cond error value
  | cond value = pure value
  | otherwise  = Left error

-- | A `Validator` which verifies that its input can be parsed as a number.
validNumber :: String -> Validator String Number
validNumber name = note (name <> " must be a number.") <<< Number.fromString

-- | A `Validator` which verifies that its input can be parsed as an integer.
validInt :: String -> Validator String Int
validInt name = note (name <> " must be a whole number.") <<< Int.fromString

-- | Modify a `Validator` to accept empty strings in addition to anything it
-- | already accepts. The empty string is mapped to `Nothing`, and any other
-- | valid input is mapped to `Just` the result of the original validator.
optional :: forall a. Validator String a -> Validator String (Maybe a)
optional _ "" = pure Nothing
optional v s  = map Just (v s)

-- | The `Validated` type describes the state of a validated form field. This
-- | state may be used to modify the way this form field or its validation
-- | messages are displayed.
-- |
-- | TODO: maybe convert this type to a record? Possible extensions to this
-- | type (as a record) could be a field `valid :: Boolean` to display an
-- | indicator that the field is valid, or a field
-- | `validating :: Maybe (Canceler a)` to control form fields with asynchronous
-- | validation.
data Validated a
  = Fresh a
  | Modified a

derive instance eqValidated :: Eq a => Eq (Validated a)
derive instance eq1Validated :: Eq1 Validated
derive instance ordValidated :: Ord a => Ord (Validated a)
derive instance ord1Validated :: Ord1 Validated

derive instance functorValidated :: Functor Validated

instance applyValidated :: Apply Validated where
  apply (Fresh f) r = f <$> r
  apply (Modified f) (Fresh a) = Modified (f a)
  apply (Modified f) (Modified a) = Modified (f a)

instance applicativeValidated :: Applicative Validated where
  pure = Fresh

-- | Lens for viewing and modifying `Validated` values.
_Validated :: forall a b. Lens (Validated a) (Validated b) a b
_Validated = flip lens ($>) $
  case _ of
    Fresh a -> a
    Modified a -> a

-- | Prism for the `Fresh` constructor of `Validated`.
_Fresh :: forall a. Prism' (Validated a) a
_Fresh = prism' Fresh $
  case _ of
    Fresh a -> Just a
    _ -> Nothing

-- | Prism for the `Modified` constructor of `Validated`.
_Modified :: forall a. Prism' (Validated a) a
_Modified = prism' Modified $
  case _ of
    Modified a -> Just a
    _ -> Nothing

-- | Sets all `Validated` fields in a record to `Fresh`, hiding all validation
-- | messages.
setFresh
  :: forall row xs row'
   . RL.RowToList row xs
  => HMap ModifyValidated {| row} {| row'}
  => {| row}
  -> {| row'}
setFresh = hmap (ModifyValidated (Fresh <<< view _Validated))

-- | Sets all `Validated` fields in a record to `Modified`, showing all
-- | validation messages.
setModified
  :: forall row xs row'
   . RL.RowToList row xs
  => HMap ModifyValidated {| row} {| row'}
  => {| row}
  -> {| row'}
setModified = hmap (ModifyValidated (Modified <<< view _Validated))

-- | Internal utility type for modifying the validated state of fields in
-- | records containing `Validated` values.
newtype ModifyValidated = ModifyValidated (Validated ~> Validated)

instance modifyValidated :: Mapping ModifyValidated a a => Mapping ModifyValidated (Validated a) (Validated a) where
  mapping m@(ModifyValidated f) = over _Validated (mapping m) <<< f
else instance modifyValidatedRecord :: (RL.RowToList r xs, MapRecordWithIndex xs (ConstMapping ModifyValidated) r r) => Mapping ModifyValidated {| r} {| r} where
  mapping d = hmap d
else instance modifyValidatedArray :: Mapping ModifyValidated a a => Mapping ModifyValidated (Array a) (Array a) where
  mapping d = map (mapping d)
else instance modifyValidatedIdentity :: Mapping ModifyValidated a a where
  mapping _ = identity

-- | Internal utility type class used to flatten repeated applications of
-- | `Validated` to a type.
class CanValidate u v | u -> v where
  fresh :: Prism' (Validated v) u
  modified :: Prism' (Validated v) u
  fromValidated :: Validated v -> u

instance canValidateValidated :: CanValidate (Validated a) a where
  fresh = identity
  modified = identity
  fromValidated = identity
else instance canValidateAny :: CanValidate a a where
  fresh = _Fresh
  modified = _Modified
  fromValidated = view _Validated

-- | Attach a validation function to a `FormBuilder p u a`, producing a new
-- | `FormBuilder` that takes a `Validated u` as form state and displays an
-- | error message if its form data is invalid.
-- |
-- | This `Validated` data type describes a form field as either `Fresh` or
-- | `Modified`, so that validation messages are only displayed if the field
-- | is `Modified`.
validated
  :: forall props unvalidated validated result result_
   . CanValidate unvalidated validated
  => Validator result_ result
  -> FormBuilder { readonly :: Boolean | props } unvalidated result_
  -> FormBuilder { readonly :: Boolean | props } (Validated validated) result
validated runValidator editor = FormBuilder \props@{ readonly } v ->
  let value = fromValidated v

      { edit, validate } = un FormBuilder editor props value

      modify :: Maybe String -> Forest JSX -> Forest JSX
      modify message forest =
          case Array.unsnoc forest of
            Nothing -> [Child { key: Nothing, child: errLine }]
            Just { init, last: Child c } ->
              Array.snoc init (Child c { child = column_ [c.child, errLine] })
            Just { init, last: Wrapper c } ->
              Array.snoc init (Wrapper c { children = modify message c.children })
            Just { init, last: Node n } ->
              Array.snoc init (Node n { validationError = Error <$> message })
        where
          errLine =
            guard (not readonly) message # foldMap \s ->
              case Error s of
                Error e ->
                  text subtext
                    { className = notNull "labeled-field--validation-error"
                    , children = [ R.text e ]
                    }
                Warning w ->
                  text subtext
                    { className = notNull "labeled-field--validation-warning"
                    , children = [ R.text w ]
                    }

      -- The validation can produce either a valid result, an error message, or
      -- none in the case where the form is Fresh.
      res :: Maybe (Either String result)
      res = do
        valid <- validate
        case v of
          Fresh _ ->
            pure <$> hush (runValidator valid)
          _ ->
            pure $ runValidator valid

      err = either pure (const Nothing) =<< res

   in { edit: \onChange -> (modify err <<< edit) (onChange <<< \f ->
          case _ of
            v'@(Fresh u) -> review modified (f (fromValidated v'))
            v'@(Modified u) -> review modified (f (fromValidated v'))
        )
      , validate: hush =<< res
      }

-- | Attach a validation function to a `FormBuilder p u a`, producing a new
-- | `FormBuilder` that takes a `Validated u` as form state and displays a
-- | warning message if its form data triggers a warning, while still allowing
-- | the form to proceed.
warn
  :: forall props unvalidated validated result
   . CanValidate unvalidated validated
  => WarningValidator result
  -> FormBuilder { readonly :: Boolean | props } unvalidated result
  -> FormBuilder { readonly :: Boolean | props } (Validated validated) result
warn warningValidator editor = FormBuilder \props@{ readonly } v ->
  let { edit, validate } = un FormBuilder editor props (fromValidated v)

      modify :: Forest JSX -> Forest JSX
      modify forest =
          case Array.unsnoc forest of
            Nothing -> [Child { key: Nothing, child: errLine }]
            Just { init, last: Child c } ->
              Array.snoc init (Child c { child = column_ [c.child, errLine] })
            Just { init, last: Wrapper c } ->
              Array.snoc init (Wrapper c { children = modify c.children })
            Just { init, last: Node n } ->
              Array.snoc init (Node n { validationError = Warning <$> message })

      errLine :: JSX
      errLine =
        guard (not readonly) message # foldMap \s ->
          text subtext
            { className = notNull "labeled-field--validation-warning"
            , children = [ R.text s ]
            }

      message :: Maybe String
      message =
        case v of
          Fresh _ ->
            Nothing
          _ ->
            warningValidator =<< validate

   in { edit: \onChange -> (modify <<< edit) (onChange <<< \f ->
          case _ of
            v'@(Fresh u) -> review modified (f (fromValidated v'))
            v'@(Modified u) -> review modified (f (fromValidated v'))
        )
      , validate
      }
