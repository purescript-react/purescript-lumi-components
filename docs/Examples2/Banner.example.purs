module Lumi.Components2.Examples.Banner where

import Prelude

import Data.Array (intercalate)
import Lumi.Components (LumiProps, ($$$))
import Lumi.Components.Button as Button
import Lumi.Components.Example (example)
import Lumi.Components.Spacing (Space(..), vspace)
import Lumi.Components.Text (h2_, p_, subsectionHeader_)
import Lumi.Components2.Banner as Banner
import Lumi.Components2.Box (box)
import Lumi.Styles.Box (FlexAlign(..), _alignSelf)
import React.Basic (JSX, fragment)
import React.Basic.DOM as R

docs :: JSX
docs =
  intercalate (vspace S8)
    [ p_ "Banners are small pieces of informative content that draw the attention of the user through color and that may require user action."

    , h2_ "round (default), spaced list, non-dismissable"
    , example
        $ fragment
        $ allColors
        $$$ loremIpsum

    , h2_ "round (default), spaced list, dismissable"
    , example
        $ fragment
        $ allColors
        $ _ { dismissable = true
            , content = importantMessage
            }

    , h2_ "action banner, round (default), spaced list, non-dismissable"
    , example
        $ fragment
        $ allColors
        $ Banner.actionBanner actions
        $$$ loremIpsum

    , h2_ "action banner, round (default), spaced list, dismissable"
    , example
        $ fragment
        $ allColors
        $ Banner.actionBanner actions
        $ _ { dismissable = true
            , content = importantMessage
            }

    ]
  where
    allColors :: (LumiProps Banner.BannerProps -> LumiProps Banner.BannerProps) -> Array JSX
    allColors props =
      [ Banner.banner
          $ _alignSelf Stretch
          $ Banner._listSpaced
          $ props
      , Banner.banner
          $ _alignSelf Stretch
          $ Banner._listSpaced
          $ Banner.primary
          $ props
      , Banner.banner
          $ _alignSelf Stretch
          $ Banner._listSpaced
          $ Banner.active
          $ props
      , Banner.banner
          $ _alignSelf Stretch
          $ Banner._listSpaced
          $ Banner.warning
          $ props
      , Banner.banner
          $ _alignSelf Stretch
          $ Banner._listSpaced
          $ Banner.error
          $ props
      ]

    loremIpsum :: Array JSX
    loremIpsum = [ R.text "Lorem ipsum dolor sit amet, consectetur adipiscing elit." ]

    importantMessage :: Array JSX
    importantMessage =
      [ box
        $$$ [ subsectionHeader_ "An important title"
            , R.text "Here's some important message about your account."
            ]
      ]

    actions :: Array JSX
    actions =
      [ Button.button Button.primary
          { title = "Try again"
          }
      , Button.button Button.linkStyle
          { title = "View error"
          }
      ]
