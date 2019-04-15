module Lumi.Components.Examples.Images where

import Prelude

import Lumi.Components.Column (column_)
import Lumi.Components.Images (avatar_, productThumb_)
import Lumi.Components.Size (extraLarge, large, medium, small)
import Lumi.Components.Text (h2_)
import Lumi.Components.Example (example)
import React.Basic (JSX)
import React.Basic.DOM as R

docs :: JSX
docs =
  column_
    [ h2_ "Avatar"

    , example $
        avatar_ { image: R.img { src: avatarImgSrc }, size: small }
    , example $
        avatar_ { image: R.img { src: avatarImgSrc }, size: medium }
    , example $
        avatar_ { image: R.img { src: avatarImgSrc }, size: large }
    , example $
        avatar_ { image: R.img { src: avatarImgSrc }, size: extraLarge }

    , h2_ "Product Thumb"

    , example $
        productThumb_ { image: R.img { src: productImgSrc }, size: small }
    , example $
        productThumb_ { image: R.img { src: productImgSrc }, size: small }
    , example $
        productThumb_ { image: R.img { src: productImgSrc }, size: medium }
    , example $
        productThumb_ { image: R.img { src: productImgSrc }, size: large }
    , example $
        productThumb_ { image: R.img { src: productImgSrc }, size: extraLarge }
    ]
  where
    avatarImgSrc = "https://s3.amazonaws.com/lumi-blog/avatars/_x600/flexo.jpg"
    productImgSrc = "https://s3.amazonaws.com/lumi-flapjack-staging/mockups/9e7f08b801e6bb3a428ef72e93c49fe5.jpg"
