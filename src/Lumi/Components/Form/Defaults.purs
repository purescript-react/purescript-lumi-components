module Lumi.Components.Form.Defaults
  ( class FormDefaults
  , formDefaults
  , class FormDefaultsRecord
  , formDefaultsRecordBuilder
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, SProxy(..))
import Lumi.Components.Form.Validation (Validated(..))
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList, Cons, Nil)
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Data.RowList (RLProxy(..))

-- | Provides default values for primitive data types to be used as initial
-- | values in form builders.
-- |
-- | For example:
-- |
-- | ```purescript
-- | build_ formDefaults ado
-- |   firstName <- focus_ (prop (SProxy :: SProxy "firstName")) textbox
-- |   lastName <- focus_ (prop (SProxy :: SProxy "lastName")) textbox
-- |   in { firstName, lastName }
-- | ```
class FormDefaults a where
  formDefaults :: a

instance formDefaultsUnit :: FormDefaults Unit where formDefaults = unit
instance formDefaultsBoolean :: FormDefaults Boolean where formDefaults = false
instance formDefaultsNumber :: FormDefaults Number where formDefaults = 0.0
instance formDefaultsInt :: FormDefaults Int where formDefaults = 0
instance formDefaultsString :: FormDefaults String where formDefaults = ""
instance formDefaultsArray :: FormDefaults (Array a) where formDefaults = []

instance formDefaultsMaybe :: FormDefaults (Maybe a) where
  formDefaults = Nothing

instance formDefaultsEither :: FormDefaults a => FormDefaults (Either a b) where
  formDefaults = Left formDefaults

instance formDefaultsValidated :: FormDefaults a => FormDefaults (Validated a) where
  formDefaults = Fresh formDefaults

instance formDefaultsRecord ::
  ( RowToList r rl
  , FormDefaultsRecord rl () r
  ) => FormDefaults (Record r) where
  formDefaults = Builder.build (formDefaultsRecordBuilder (RLProxy :: RLProxy rl)) {}

--
class FormDefaultsRecord rl r_ r | rl -> r_ r where
  formDefaultsRecordBuilder :: RLProxy rl -> Builder { | r_ } { | r }

instance formDefaultsRecordNil :: FormDefaultsRecord Nil () () where
  formDefaultsRecordBuilder _ = identity

instance formDefaultsRecordCons ::
  ( IsSymbol l
  , FormDefaults a
  , FormDefaultsRecord tail r_ tailR
  , Lacks l tailR
  , Cons l a tailR r
  ) => FormDefaultsRecord (Cons l a tail) r_ r where
  formDefaultsRecordBuilder _ = head <<< tail
    where
      head = Builder.insert (SProxy :: SProxy l) (formDefaults :: a)
      tail = formDefaultsRecordBuilder (RLProxy :: RLProxy tail)
