module Lumi.Components.FetchCache where

import Prelude

import Data.Array (foldr)
import Data.Array as Array
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import React.Basic (Component, JSX, createComponent, make, readProps)

type FetchCacheProps id value =
  { getData :: id -> Aff value
  , id :: Maybe id
  , render :: Maybe value -> JSX
  }

-- | Fetch data on mount and rerender when it's ready.
single :: forall id value. Ord id => FetchCacheProps id value -> JSX
single props =
  multi
    { getData: \ids -> do
        values <- traverse props.getData ids
        pure $ Array.zipWith (\id value -> { id, value }) ids values
    , ids: map Array.singleton props.id
    , render: \values -> props.render (join $ map _.value (Array.head =<< values))
    }

type FetchCacheMultiProps id value =
  { getData :: Array id -> Aff (Array { id :: id, value :: value })
  , ids :: Maybe (Array id)
  , render :: Maybe (Array { id :: id, value :: Maybe value }) -> JSX
  }

-- | Fetch data on mount and rerender when it's ready.
multi :: forall id value. Ord id => FetchCacheMultiProps id value -> JSX
multi = make component
  { initialState
  , didMount: checkIds
  , didUpdate: \self _ -> checkIds self
  , render
  }
  where
    initialState =
      Map.empty :: Map.Map id (Maybe value)

    checkIds self = do
      for_ self.props.ids \currentIds -> do
        let missingIds = Array.filter (not (flip Map.member) self.state) currentIds
        unless (Array.null missingIds) do
          self.setState \state -> foldr (flip Map.insert Nothing) state missingIds
          props <- readProps self
          launchAff_ do
            values <- props.getData missingIds
            liftEffect do
              self.setState \state -> foldr (\{ id, value } -> Map.insert id (Just value)) state values

    render { props, state } =
      props.render (map (map (\id -> { id, value: join $ Map.lookup id state })) props.ids)

data FetchCacheAction id value
  = Fetch (Array id)
  | Receive (Array { id :: id, value :: value })

component :: forall id value. Component (FetchCacheMultiProps id value)
component = createComponent "FetchCache"
