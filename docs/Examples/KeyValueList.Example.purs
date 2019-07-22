module Lumi.Components.Examples.KeyValueList where

import Prelude

import Data.Array (intercalate)
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Lumi.Components.Size (Size(..))
import Lumi.Components.Button (ButtonState(..), button, defaults, primary, secondary, linkStyle, iconButton, iconButtonDefaults)
import Lumi.Components.Color (colorNames)
import Lumi.Components.Column (column_)
import Lumi.Components.Icon (IconType(..))
import Lumi.Components.Images (avatar_, productThumb_)
import Lumi.Components.KeyValueList (keyValueList)
import Lumi.Components.Lockup (lockup)
import Lumi.Components.Text as T
import Lumi.Components.Example (example)
import React.Basic (JSX)
import React.Basic.DOM (css)
import React.Basic.DOM as R

docs :: JSX
docs =
  column_ $
    [ example
        $ keyValueList
            { rightAligned: true
            , rows: r
            , borders: true
            }
    , example
        $ keyValueList
            { rightAligned: false
            , rows: r
            , borders: true
            }
    , example
        $ keyValueList
            { rightAligned: true
            , rows: r
            , borders: false
            }
    , example
        $ keyValueList
            { rightAligned: false
            , rows: r
            , borders: false
            }
    ]

  where
    r =
      [ { label: "Name"
        , value: T.body_ "Flexo"
        }
      , { label: "Email"
        , value: T.body_ "flexo@lumi.com"
        }
      , { label: ""
        , value: lockup
            { title: R.text "Flexo R."
            , subtitle: Just $ R.text "Lumi"
            , image: Just $ avatar_
                { size: Large
                , image:
                    R.img
                      { src: "https://s3.amazonaws.com/lumi-flapjack-staging/mockups/9e7f08b801e6bb3a428ef72e93c49fe5.jpg"
                      }
                }
            }
        }
      ]
