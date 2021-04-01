module Lumi.Components2.Examples.Image where

import Prelude

import Effect.Console (log)
import Effect.Uncurried (mkEffectFn1)
import Lumi.Components (($$$))
import Lumi.Components.Column (column_)
import Lumi.Components.Example (example)
import Lumi.Components.NativeSelect (nativeSelect, defaults)
import Lumi.Components.Spacing (Space(..))
import Lumi.Components.Text (h2_)
import Lumi.Components2.Image as Image
import Lumi.Styles.Image as Image.Styles
import React.Basic.Classic (JSX)
import React.Basic.DOM as R

docs :: JSX
docs =
  let imgSrc = "https://s3.amazonaws.com/lumi-flapjack-staging/mockups/9e7f08b801e6bb3a428ef72e93c49fe5.jpg"
  in column_
    [ Image.image
        $ _ { src = " " }

    , Image.small
        $ _ { src = imgSrc }
    , Image.medium
        $ _ { src = imgSrc }
    , Image.large
        $ _ { src = imgSrc }
    , Image.extraLarge
        $ _ { src = imgSrc }

    -- @TODO need default size
    -- , Image.round
    --     $ _ { src = imgSrc }
    , Image.small
        $ Image.Styles._round
        $ _ { src = imgSrc }
    , Image.medium
        $ Image.Styles._round
        $ _ { src = imgSrc }
    , Image.large
        $ Image.Styles._round
        $ _ { src = imgSrc }
    , Image.extraLarge
        $ Image.Styles._round
        $ _ { src = imgSrc }

    , Image.image
        $ Image._customSize S112
        $ _ { src = imgSrc }
    , Image.round
        $ Image._customSize S112
        $ _ { src = imgSrc }
    ]