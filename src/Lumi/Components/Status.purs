module Lumi.Components.Status where

import Prelude

data Status
  = Active
  | Warning
  | Error
  | Unknown

derive instance eqStatus :: Eq Status

instance showStatus :: Show Status where
  show Active = "active"
  show Warning = "warning"
  show Error = "error"
  show Unknown = "unknown"

active :: Status
active = Active

warning :: Status
warning = Warning

error :: Status
error = Error

unknown :: Status
unknown = Unknown
