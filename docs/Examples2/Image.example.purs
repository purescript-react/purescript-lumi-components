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
    , h4_ "Image + resize { width: 90px, height: 50px }, constrained to image's aspect ratio"
    , example
        $ Image.image
        $ Image.resize { width: 90, height: 50 }
        $$$ "http://via.placeholder.com/640x360"
    , h4_ "Thumbnail default (will always have a square aspect ratio)"
    , example
        $ Image.thumbnail
        $$$ "http://via.placeholder.com/640x360"
    , h4_ "Thumbnail + resize 48px"
    , example
        $ Image.thumbnail
        $ Image.resizeSquare 80
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
    , h4_ "Thumbnail + round"
    , example
        $ Image.thumbnail
        $ Image.round
        $ Image.extraLarge
        $$$ flexo
    ]
