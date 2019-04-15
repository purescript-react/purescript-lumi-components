module Lumi.Components.Examples.Navigation where

import Prelude

import Data.Maybe (Maybe(..))
import Data.NonEmpty ((:|))
import Effect.Console (log)
import Lumi.Components.Column (column_)
import Lumi.Components.Example (example)
import Lumi.Components.Navigation (navigation)
import Lumi.Components.Text (h2_)
import React.Basic (JSX)
import Web.HTML.History (URL(..))

docs :: JSX
docs =
  column_
    [ h2_ "Compact navigation"
    , example $
        navigation
          { navLinks:
              { text: "Home"
              , href: URL "/"
              , subNavLinks: []
              } :|
              [ { text: "Customers"
                , href: URL "/customers"
                , subNavLinks: []
                }
              , { text: "Products"
                , href: URL "/products"
                , subNavLinks:
                    [ { text: "Products"
                      , href: URL "/products"
                      }
                    , { text: "Categories"
                      , href: URL "/categories"
                      }
                    ]
                }
              , { text: "Orders"
                , href: URL "/orders"
                , subNavLinks: []
                }
              , { text: "Help"
                , href: URL "/help"
                , subNavLinks: []
                }
              ]
          , user:
            { src: "https://s3.amazonaws.com/lumi-flapjack-staging/avatars/vfhpnqairr/thumbnail_1517878176349.png"
            , name: Just "Flexo"
            , email: Just "flexo@lumi.com"
            , id: "flexo-1234"
            }
          , client:
              { name: Nothing
              }
          , compact: true
          , onCartClick: Nothing
          , cartAmount: Nothing
          , onLogoutClick: Just $ log "logout click"
          , logo: mempty
          }

    , h2_ "Responsive navigation"
    , example $
        navigation
          { navLinks:
              { text: "Items"
              , href: URL "/items"
              , subNavLinks: []
              } :|
              [ { text: "Orders"
                , href: URL "/orders"
                , subNavLinks: []
                }
              , { text: "Shipments"
                , href: URL "/shipments"
                , subNavLinks: []
                }
              , { text: "Billing"
                , href: URL "/billing"
                , subNavLinks: []
                }
              , { text: "Shop"
                , href: URL "/shop"
                , subNavLinks: []
                }
              ]
          , user:
            { src: "https://s3.amazonaws.com/lumi-flapjack-staging/avatars/vfhpnqairr/thumbnail_1517878176349.png"
            , name: Just $ "Flexo"
            , email: Just $ "flexo@lumi.com"
            , id: "flexo-1234"
            }
          , client:
              { name: Just "Lumi"
              }
          , compact: false
          , onCartClick: Just $ log "cart click"
          , cartAmount: Just 2
          , onLogoutClick: Just $ log "logout click"
          , logo: mempty
          }
    ]
