module Lumi.Components.Size where

import Prelude

import Data.Maybe (Maybe(..))

data Size
  = Small
  | Medium
  | Large
  | ExtraLarge (Maybe { extraExtraLarge :: Boolean })

derive instance eqSize :: Eq Size
derive instance ordSize :: Ord Size

instance showSize :: Show Size where
  show Small = "small"
  show Medium = "medium"
  show Large = "large"
  show (ExtraLarge s) =
    case s of
      Just { extraExtraLarge } ->
        if extraExtraLarge then "extra-extra-large" else "extra-large"
      _ -> "extra-large"

small :: Size
small = Small

medium :: Size
medium = Medium

large :: Size
large = Large

extraLarge :: Size
extraLarge = ExtraLarge Nothing
