module Lumi.Components2.Examples.Image where

import Prelude

import Data.Array as Array
import Lumi.Components (($$$))
import Lumi.Components.Example (example)
import Lumi.Components.Spacing (Space(..), vspace)
import Lumi.Components.Text (h4_)
import Lumi.Components2.Image as Image
import React.Basic.Classic (JSX)

docs :: JSX
docs =
  let flexo = "https://i.picsum.photos/id/985/1000/300.jpg?hmac=t_lmj43iuzwGqZaRMY1ee9udE_pCzfYLgCD49YrCPjw"
        -- "https://s3.amazonaws.com/lumi-blog/avatars/_x600/flexo.jpg"
  in Array.intercalate (vspace S16)
    [ h4_ "Image default (will respect image's original aspect ratio & dimensions)"
    , example
        $ Image.image
        $$$ flexo
    , h4_ "Image + resize { width: 40px, height: 120px }, the image will fill the height and width of its parent (object-fit: cover), maintaining its aspect ratio but cropping the image"
    , example
        $ Image.image
        $ Image.resize { width: 40, height: 120 }
        $$$ flexo
    , h4_ "Thumbnail default (will always have a square aspect ratio); and defaults to object-fit: cover"
    , example
        $ Image.thumbnail
        $$$ flexo
    , h4_ "Thumbnail default (will always have a square aspect ratio); but can be override with object-fit: contain"
    , example
        $ Image.thumbnail
        $ Image.contain
        $ _ { content = flexo
            }
    , h4_ "Thumbnail + resize 400px"
    , example
        $ Image.thumbnail
        $ Image.resizeSquare 400
        $$$ flexo
    , h4_ "Thumbnail + small"
    , example
        $ Image.thumbnail
        $ Image.small
        $$$ flexo
    , h4_ "Thumbnail + medium"
    , example
        $ Image.thumbnail
        $ Image.medium
        $$$ flexo
    , h4_ "Thumbnail + large"
    , example
        $ Image.thumbnail
        $ Image.large
        $$$ flexo
    , h4_ "Thumbnail + extra large"
    , example
        $ Image.thumbnail
        $ Image.extraLarge
        $$$ flexo
    , h4_ "Thumbnail + avatar (small)"
    , example
        $ Image.thumbnail
        $ Image.smallAvatar
        $$$ flexo
    , h4_ "Thumbnail + avatar (medium)"
    , example
        $ Image.thumbnail
        $ Image.mediumAvatar
        $$$ flexo
    , h4_ "Thumbnail + avatar (large)"
    , example
        $ Image.thumbnail
        $ Image.largeAvatar
        $$$ flexo
    , h4_ "Default placeholder (can be overriden)"
    , example
        $ Image.image
        $ Image.resize { width: 320, height: 240 }
        $$$ ""
    , example
        $ Image.thumbnail
        $ Image.medium
        $$$ ""
    ]
