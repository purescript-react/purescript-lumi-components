module Lumi.Components.FetchCache where

import Prelude

import Data.Array (foldr)
import Data.Array as Array
import Data.Foldable (foldMap, for_)
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing)
import Data.Set as Set
import Data.Traversable (traverse)
import Effect.Aff (Aff, Milliseconds, delay, forkAff, killFiber, launchAff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import React.Basic.Classic (Component, JSX, createComponent, make, readState)

type FetchCacheProps id value =
  { getData :: id -> Aff value
  , id :: Maybe id
  , render :: Maybe value -> JSX
  }

type FetchCacheWithDebounceProps id value =
  { getData :: id -> Aff value
  , id :: Maybe id
  , render :: Maybe value -> JSX
  , debounce :: Milliseconds
  }

-- | Fetch data on mount and rerender when it's ready.
single :: forall id value. Ord id => FetchCacheProps id value -> JSX
single props = singleWithDebounce
  { getData: props.getData
  , id: props.id
  , render: props.render
  , debounce: mempty
  }

-- | Fetch data on mount and rerender when it's ready, with an added debounce
singleWithDebounce :: forall id value. Ord id => FetchCacheWithDebounceProps id value -> JSX
singleWithDebounce props =
  multiWithDebounce
    { getData: \ids -> do
        values <- traverse props.getData ids
        pure $ Array.zipWith (\id value -> { id, value }) ids values
    , ids: map Array.singleton props.id
    , render: \values -> props.render (join $ map _.value (Array.head =<< values))
    , debounce: props.debounce
    }

type FetchCacheMultiProps id value
  = { getData :: Array id -> Aff (Array { id :: id, value :: value })
    , ids :: Maybe (Array id)
    , render :: Maybe (Array { id :: id, value :: Maybe value }) -> JSX
    }

type FetchCacheMultiWithDebounceProps id value
  = { getData :: Array id -> Aff (Array { id :: id, value :: value })
    , ids :: Maybe (Array id)
    , render :: Maybe (Array { id :: id, value :: Maybe value }) -> JSX
    , debounce :: Milliseconds
    }

-- | Fetch data on mount and rerender when it's ready.
multi
  :: forall id value. Ord id
  => FetchCacheMultiProps id value
  -> JSX
multi props = multiWithDebounce
  { getData: props.getData
  , ids: props.ids
  , render: props.render
  , debounce: mempty
  }

-- | Fetch data on mount and rerender when it's ready, with an added debounce
multiWithDebounce
  :: forall id value. Ord id
  => FetchCacheMultiWithDebounceProps id value -> JSX
multiWithDebounce = make component
  { initialState
  , didMount: checkIds
  , didUpdate: \self _ -> checkIds self
  , render
  }
  where
    initialState =
      { cache: (Map.empty :: Map.Map id (Maybe value))
      , fiber: Nothing
      }

    checkIds self = do
      for_ self.props.ids \currentIds -> do
        let
          missingIds = Array.filter (not (flip Map.member) self.state.cache) currentIds
          -- ids we haven't fetched yet that aren't of current interest either
          orphanIds = Map.keys $ Map.filterWithKey (\k v -> isNothing v && not (k `Array.elem` currentIds)) self.state.cache
        unless (Array.null missingIds && Set.isEmpty orphanIds) do
          self.setState \state ->
            state { cache
                      -- insert new keys of interest
                      = flip (foldr (flip Map.insert Nothing)) missingIds
                      -- remove orphans
                      <<< Map.filterKeys (not <<< flip Set.member orphanIds)
                      $ state.cache
                  }
          -- to debounce, we stick our effect in a fiber which waits before doing the effect.
          -- each time we have a change in the ids we currently care about, we kill the old fiber,
          -- and so avoid doing the old effect if it hasn't finished yet.
          fiber <- launchAff do
            void $ forkAff $ do foldMap (killFiber (error "Canceled")) self.state.fiber
            delay self.props.debounce
            -- we fetch the latest state. this picks up the Nothings we just inserted, and also accounts
            -- for the possiblity that the existing fiber finished and populated part of our cache while
            -- in this function, avoiding potentially recaching it
            unfetchedIds <- Set.toUnfoldable <<< Map.keys <<< Map.filter isNothing <<< _.cache <$> liftEffect (readState self)
            values <- self.props.getData unfetchedIds
            liftEffect $
              self.setState \state ->
                state { cache = foldr (\{ id, value } -> Map.insert id (Just value)) state.cache values }
          self.setState _{ fiber = Just fiber }

    render { props, state } =
      props.render (map (map (\id -> { id, value: join $ Map.lookup id state.cache })) props.ids)

data FetchCacheAction id value
  = Fetch (Array id)
  | Receive (Array { id :: id, value :: value })

component :: forall id value. Component (FetchCacheMultiWithDebounceProps id value)
component = createComponent "FetchCache"
