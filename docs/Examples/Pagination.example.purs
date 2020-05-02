module Lumi.Components.Examples.Pagination where

import Prelude

import Data.Maybe (Maybe(..))
import Lumi.Components.Column (column_)
import Lumi.Components.Pagination (defaults, pagination)
import Lumi.Components.Example (example)
import React.Basic (JSX, createComponent, make)
import React.Basic.DOM as R

docs :: JSX
docs = unit # make (createComponent "PaginationExample")
  { initialState
  , render
  }
  where
    initialState =
      { currentPage: 0
      }

    render self =
      column_ $
        [ example
          $ pagination defaults
              { currentPage = self.state.currentPage
              , pages = 53
              , onChange = self.setState <<< flip _{ currentPage = _ }
              , details = Just $ R.text "531 results"
              }

        , example
          $ pagination defaults
              { currentPage = self.state.currentPage
              , pages = 4
              , onChange = self.setState <<< flip _{ currentPage = _ }
              , details = Just $ R.text "40 results"
              }

        , example
          $ pagination defaults
              { currentPage = self.state.currentPage
              , pages = 0
              , onChange = self.setState <<< flip _{ currentPage = _ }
              , details = Just $ R.text "0 results"
              }

        , example
          $ pagination defaults
              { currentPage = self.state.currentPage
              , pages = 40
              , onChange = self.setState <<< flip _{ currentPage = _ }
              , focusWindow = 4
              , marginPages = 3
              , details = Just $ R.text "408 results"
              }
        ]
