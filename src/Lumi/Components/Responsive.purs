module Lumi.Components.Responsive where

import Prelude
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks (Hook, JSX, ReactComponent, component, element, unsafeHook)
import React.Basic.Hooks as React

foreign import data MediaQuery :: Type

foreign import data UseMediaQuery :: Type -> Type

useMedia :: String -> Hook UseMediaQuery Boolean
useMedia mq = unsafeHook (runEffectFn1 useMedia_ mq)

foreign import useMedia_ :: EffectFn1 String Boolean

useIsPhone :: Hook UseMediaQuery Boolean
useIsPhone = map not (useMedia ("(min-width: " <> show minWidthNonPhone <> "px)"))

useIsMobile :: Hook UseMediaQuery Boolean
useIsMobile = map not useIsDesktop

useIsDesktop :: Hook UseMediaQuery Boolean
useIsDesktop = useMedia ("(min-width: " <> show minWidthDesktop <> "px)")

-- | Prefer `useIsPhone` or `phone` to using this value
-- | directly. Named in the negative because -- | this
-- | width is the first size that is _not_ a phone and
-- | trying to use this width as a "max-width" leaves
-- | out fractional pixels (447.5px) which sometimes
-- | occur on high-dpi displays.
minWidthNonPhone :: Int
minWidthNonPhone = 448

minWidthDesktop :: Int
minWidthDesktop = 860

mobile :: (Unit -> JSX) -> JSX
mobile = element c <<< { render: _ }
  -- WARNING: Don't add any arguments the `mobile` function!
  --   `c` must be fully applied at module creation!
  where
  c =
    unsafePerformEffect do
      mkMediaComponent "Mobile" useIsMobile

phone :: (Unit -> JSX) -> JSX
phone = element c <<< { render: _ }
  -- WARNING: Don't add any arguments the `phone` function!
  --   `c` must be fully applied at module creation!
  where
  c =
    unsafePerformEffect do
      mkMediaComponent "Phone" useIsPhone

desktop :: (Unit -> JSX) -> JSX
desktop = element c <<< { render: _ }
  -- WARNING: Don't add any arguments the `desktop` function!
  --   `c` must be fully applied at module creation!
  where
  c =
    unsafePerformEffect do
      mkMediaComponent "Desktop" useIsDesktop

mkMediaComponent ::
  String ->
  Hook UseMediaQuery Boolean ->
  Effect (ReactComponent { render :: Unit -> JSX })
mkMediaComponent name umq = do
  component ("MediaQuery(" <> name <> ")") \{ render } -> React.do
    isMatch <- umq
    if isMatch then
      pure (render unit)
    else
      mempty
