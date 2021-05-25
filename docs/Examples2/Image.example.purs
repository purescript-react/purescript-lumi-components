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
  let flexo = "https://s3.amazonaws.com/lumi-blog/avatars/_x600/flexo.jpg"
  in Array.intercalate (vspace S16)
    [ h4_ "Image default (will respect image's aspect ratio)"
    , example
        $ Image.image
        $$$ "http://via.placeholder.com/640x360"
    , h4_ "Image + resize { width: 120px, height: 40px }, contains image to parent container's aspect ratio (will clip edges)"
    , example
        $ Image.image
        $ Image.resize { width: 120, height: 40 }
        $$$ "http://via.placeholder.com/360x640"
    , h4_ "Thumbnail default (will always have a square aspect ratio)"
    , example
        $ Image.thumbnail
        $$$ "http://via.placeholder.com/360x640"
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
    , h4_ "Placeholders (can be overriden)"
    , example
        $ Image.image
        $ Image.resize { width: 900, height: 50 }
        $$$ ""
    , example
        $ Image.thumbnail
        $ Image.medium
        $$$ ""
    ]
