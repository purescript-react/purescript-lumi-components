module Lumi.Components.Responsive where

import Prelude

import Effect.Uncurried (EffectFn1, runEffectFn1)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks (Component, Hook, JSX, component, unsafeHook)
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
mobile = \render ->
  c \isMobile -> if isMobile then render unit else mempty
  -- WARNING: Don't add any arguments to the definition of `mobile`!
  --   `c` must be fully applied at module creation!
  where
  c = unsafePerformEffect (mkMediaComponent "Mobile" useIsMobile)

withMobile :: (Boolean -> JSX) -> JSX
withMobile = c
  -- WARNING: Don't add any arguments to the `withMobile` function!
  --   `c` must be fully applied at module creation!
  where
  c = unsafePerformEffect (mkMediaComponent "Mobile" useIsMobile)

phone :: (Unit -> JSX) -> JSX
phone = \render ->
  c \isPhone -> if isPhone then render unit else mempty
  -- WARNING: Don't add any arguments to the definition of `phone`!
  --   `c` must be fully applied at module creation!
  where
  c = unsafePerformEffect (mkMediaComponent "Phone" useIsPhone)

withPhone :: (Boolean -> JSX) -> JSX
withPhone = c
  -- WARNING: Don't add any arguments to the `withPhone` function!
  --   `c` must be fully applied at module creation!
  where
  c = unsafePerformEffect (mkMediaComponent "Phone" useIsPhone)

desktop :: (Unit -> JSX) -> JSX
desktop = \render ->
  c \isDesktop -> if isDesktop then render unit else mempty
  -- WARNING: Don't add any arguments the `desktop` function!
  --   `c` must be fully applied at module creation!
  where
  c = unsafePerformEffect (mkMediaComponent "Desktop" useIsDesktop)

withDesktop :: (Boolean -> JSX) -> JSX
withDesktop = c
  -- WARNING: Don't add any arguments to the `withDesktop` function!
  --   `c` must be fully applied at module creation!
  where
  c = unsafePerformEffect (mkMediaComponent "Desktop" useIsDesktop)

mkMediaComponent ::
  String ->
  Hook UseMediaQuery Boolean ->
  Component (Boolean -> JSX)
mkMediaComponent name umq = do
  component ("MediaQuery(" <> name <> ")") \render -> React.do
    isMatch <- umq
    pure $ render isMatch
