module Lumi.Components2.Examples.Image where

import Prelude

import Data.Array as Array
import Lumi.Components.Example (example)
import Lumi.Components.Spacing (Space(..), vspace)
import Lumi.Components.Text (h4_)
import Lumi.Components2.Image as Image
import Lumi.Styles.Image as Image.Styles
import React.Basic.Classic (JSX)

docs :: JSX
docs =
  let imgSrc = "https://s3.amazonaws.com/lumi-flapjack-staging/mockups/9e7f08b801e6bb3a428ef72e93c49fe5.jpg"
      flexo = "https://s3.amazonaws.com/lumi-blog/avatars/_x600/flexo.jpg"
  in Array.intercalate (vspace S16)
    [ example $ Image.imageThumb
        $ Image.Styles._extraLarge
        $ _ { src = ""
            }
    , h4_ "Small"
    , example $ Image.small
        $ _ { src = imgSrc }
    , h4_ "Medium"
    , example $ Image.medium
        $ _ { src = imgSrc }
    , h4_ "Large"
    , example $ Image.large
        $ _ { src = imgSrc }
    , h4_ "ExtraLarge"
    , example $ Image.extraLarge
        $ _ { src = imgSrc }

    , h4_ "Round"
    , example $ Image.round
        $ _ { src = flexo }
    , h4_ "Round + small"
    , example $ Image.small
        $ Image.Styles._round
        $ _ { src = flexo }
    , h4_ "Round + medium"
    , example $ Image.medium
        $ Image.Styles._round
        $ _ { src = flexo }
    , h4_ "Round + large"
    , example $ Image.large
        $ Image.Styles._round
        $ _ { src = flexo }
    , h4_ "Round + extra large"
    , example $ Image.extraLarge
        $ Image.Styles._round
        $ _ { src = flexo }
    ]