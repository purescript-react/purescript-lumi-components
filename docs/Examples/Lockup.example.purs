module Lumi.Components.Examples.Lockup where

import Prelude

import Data.Maybe (Maybe(..))
import Lumi.Components.Column (column_)
import Lumi.Components.Images (productThumb_)
import Lumi.Components.Lockup (pageLockup, productLockup, userLockup)
import Lumi.Components.Size (Size(..))
import Lumi.Components.Text (h4_)
import Lumi.Components.Example (example)
import React.Basic (JSX)
import React.Basic.DOM as R

docs :: JSX
docs =
  column_
    [ h4_ "Product Lockup"
    , example $
        productLockup
          { name: "Item title"
          , description: Just "Foo Bar"
          , image: R.img { src: productImgSrc }
          }

    , h4_ "User Lockup"
    , example $
        userLockup
          { name: "Flexo Rodriguez"
          , description: Just "Lumi"
          , image: R.img { src: avatarImgSrc }
          }

    , h4_ "User Lockup (no image)"
    , example $
        userLockup
          { name: "Flexo Rodriguez"
          , description: Just "Lumi"
          , image: mempty
          }

    , h4_ "Page title Lockup"
    , example $
        pageLockup
          { title: "Small Pouch - Rebrand"
          , subtitle: Just "7\" × 11\""
          , image: Just $ productThumb_ { size: Medium, image: R.img { src: productImgSrc } }
          }
    ]
  where
    avatarImgSrc = "https://s3.amazonaws.com/lumi-flapjack-staging/avatars/vfhpnqairr/thumbnail_1517878176349.png"
    productImgSrc = "https://s3.amazonaws.com/lumi-flapjack-staging/mockups/9e7f08b801e6bb3a428ef72e93c49fe5.jpg"
