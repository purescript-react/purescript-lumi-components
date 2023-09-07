module Lumi.Components2.PasswordStrength where


import React.Basic.Hooks as Hooks

type PasswordStrengthProps =
  { password :: String
  , minLength :: Int
  }

foreign import passwordStrengthBar :: Hooks.ReactComponent PasswordStrengthProps