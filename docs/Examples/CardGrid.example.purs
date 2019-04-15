module Lumi.Components.Examples.CardGrid where

import Prelude

import Data.Array (mapWithIndex, take)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Effect.Console (log)
import Lumi.Components.CardGrid (cardGrid)
import Lumi.Components.Column (column_)
import Lumi.Components.Text (h2_, p_)
import Lumi.Components.Example (example)
import React.Basic (JSX, createComponent, make)
import React.Basic.DOM as R
import Web.HTML.History (URL(..))

docs :: JSX
docs = unit # make (createComponent "CardGridExample")
  { initialState
  , render
  }
  where
    initialState =
      { selected: []
      }

    render self =
      column_
        [ h2_ "Responsive item grid"
        , p_ "* Resize the window to see how the component responds."
        , example $
            cardGrid
              { items: items # mapWithIndex \index { title, subtitle, href, image } ->
                  { key: show index
                  , title
                  , subtitle
                  , href
                  , children: image # foldMap \src -> [ R.img { src } ]
                  }
              , onNavigate: log <<< un URL
              , selection: Nothing
              }

        , h2_ "Item grid with few items"
        , example $
            cardGrid
              { items: take 2 items # mapWithIndex \index { title, subtitle, href, image } ->
                  { key: show index
                  , title
                  , subtitle
                  , href
                  , children: image # foldMap \src -> [ R.img { src } ]
                  }
              , onNavigate: log <<< un URL
              , selection: Nothing
              }

        , h2_ "Responsive, selectable item grid"
        , p_ "* Resize the window to see how the component responds. Selection boxes are always visible on small screens."
        , example $
            cardGrid
              { items: items # mapWithIndex \index { title, subtitle, href, image } ->
                  { key: show index
                  , title
                  , subtitle
                  , href
                  , children: image # foldMap \src -> [ R.img { src } ]
                  }
              , onNavigate: log <<< un URL
              , selection: Just
                  { selectedKeys: self.state.selected
                  , onSelect: self.setState <<< flip _{ selected = _ }
                  }
              }
        ]

    items =
      [ { title: "Poly Mailers"
        , subtitle: "14.50\" × 19.00\""
        , href: Just $ URL "lumi.com"
        , image: Just "https://s3.amazonaws.com/lumi-flapjack-production/mockups/4985252ac5ba4c79e2ecbc6d3438e4ca.jpg"
        }
      , { title: "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec rhoncus neque in consequat fermentum"
        , subtitle: "9.00\" × 5.00\""
        , href: Just $ URL "lumi.com/items"
        , image: Just "https://s3.amazonaws.com/lumi-flapjack-production/mockups/4985252ac5ba4c79e2ecbc6d3438e4ca.jpg"
        }
      , { title: "Poly Mailers (no image)"
        , subtitle: "14.50\" × 19.00\""
        , href: Nothing
        , image: Nothing
        }
      , { title: "Random product"
        , subtitle: "4.50\" × 12.30\""
        , href: Just $ URL "lumi.com"
        , image: Nothing
        }
      , { title: "This text is so large it doesn't fit in a single line."
        , subtitle: "11.00\" × 7.00\""
        , href: Nothing
        , image: Just "https://s3.amazonaws.com/lumi-flapjack-production/mockups/4985252ac5ba4c79e2ecbc6d3438e4ca.jpg"
        }
      , { title: "Random product 2"
        , subtitle: "This is a very long subtitle. I wonder what will happen to it?"
        , href: Nothing
        , image: Nothing
        }
      ]
