module Lumi.Components.Orientation where

import Prelude

data Orientation
  = Horizontal
  | Vertical

derive instance eqOrientation :: Eq Orientation

instance showOrientation :: Show Orientation where
  show Horizontal = "horizontal"
  show Vertical = "vertical"
