module Lumi.Components.Utility.AffToPromise where

import Prelude

import Data.Either (Either(..), either)
import Effect.Aff (Aff, makeAff, nonCanceler, runAff_)
import Effect.Promise (class Deferred, Promise, promise, runPromise)

affToPromise :: forall a. Aff a -> (Deferred => Promise a)
affToPromise aff = promise \resolve reject -> do
  runAff_ (either reject resolve) aff

promiseToAff :: forall a. (Deferred => Promise a) -> Aff a
promiseToAff prom = makeAff \resolve -> do
  runPromise (resolve <<< Right) (resolve <<< Left) prom
  pure nonCanceler
