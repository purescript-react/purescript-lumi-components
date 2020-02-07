module Lumi.Components.Size where

import Prelude


data Size
  = Small
  | Medium
  | Large
  | ExtraLarge
  | ExtraExtraLarge

derive instance eqSize :: Eq Size
derive instance ordSize :: Ord Size

instance showSize :: Show Size where
  show Small = "small"
  show Medium = "medium"
  show Large = "large"
  show ExtraLarge = "extra-large"
  show ExtraExtraLarge = "extra-extra-large"

small :: Size
small = Small

medium :: Size
medium = Medium

large :: Size
large = Large

extraLarge :: Size
extraLarge = ExtraLarge

extraExtraLarge :: Size
extraExtraLarge = ExtraExtraLarge
