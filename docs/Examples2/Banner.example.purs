module Lumi.Components2.Examples.Banner where

import Prelude

import Data.Array (intercalate)
import Lumi.Components (LumiProps, lumiElement)
import Lumi.Components.Button as Button
import Lumi.Components.Example (example)
import Lumi.Components.Spacing (Space(..), vspace)
import Lumi.Components.Text (h2_, p_, subsectionHeader_)
import Lumi.Components2.Box (box)
import Lumi.Components2.Banner as Banner
import Lumi.Styles.Box (FlexAlign(..), _alignSelf)
import React.Basic (JSX, fragment)
import React.Basic.DOM as R

docs :: JSX
docs =
  intercalate (vspace S8)
    [ p_ "TODO"

    , h2_ "TODO"
    , example
        $ fragment
        $ allColors
        $ _alignSelf Stretch
        $ _ { content = loremIpsum }

    , h2_ "TODO"
    , example
        $ fragment
        $ allColors
        $ _alignSelf Stretch
        $ _ { dismissable = true
            , content = importantMessage
            }

    , h2_ "TODO"
    , example
        $ fragment
        $ allColors
        $ _alignSelf Stretch
        $ Banner.actionBanner actions
        $ _ { content = loremIpsum }

    , h2_ "TODO"
    , example
        $ fragment
        $ allColors
        $ _alignSelf Stretch
        $ Banner.actionBanner actions
        $ _ { dismissable = true
            , content = importantMessage
            }

    ]
  where
    allColors :: (LumiProps Banner.BannerProps -> LumiProps Banner.BannerProps) -> Array JSX
    allColors props =
      [ lumiElement Banner.banner
          $ Banner._listSpaced
          $ props
      , lumiElement Banner.banner
          $ Banner._listSpaced
          $ Banner.primary
          $ props
      , lumiElement Banner.banner
          $ Banner._listSpaced
          $ Banner.active
          $ props
      , lumiElement Banner.banner
          $ Banner._listSpaced
          $ Banner.warning
          $ props
      , lumiElement Banner.banner
          $ Banner._listSpaced
          $ Banner.error
          $ props
      ]

    loremIpsum :: Array JSX
    loremIpsum = [ R.text "Lorem ipsum dolor sit amet, consectetur adipiscing elit." ]

    importantMessage :: Array JSX
    importantMessage =
      [ lumiElement box
        $ _ { content =
                [ subsectionHeader_ "An important title"
                , R.text "Here's some important message about your account."
                ]
            }
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
