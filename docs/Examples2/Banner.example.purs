module Lumi.Components2.Examples.Banner where

import Prelude

import Data.Array (intercalate)
import Lumi.Components (LumiProps)
import Lumi.Components.Button as Button
import Lumi.Components.Example (example)
import Lumi.Components.Images (avatar)
import Lumi.Components.Size (large)
import Lumi.Components.Spacing (Space(..), vspace)
import Lumi.Components.Text (h2_, p_, subsectionHeader_)
import Lumi.Components2.Banner as Banner
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
        $ _ { content = loremIpsum_ }

    , h2_ "round (default), spaced list, dismissable"
    , example
        $ fragment
        $ allColors
        $ _ { content = loremIpsum_
            , dismissable = true
            }

    , h2_ "round (default), spaced list, non-dismissable with icon"
    , example
        $ fragment
        $ allColors
        $ _ { content = loremIpsum_
            , icon = bannerIcon
            }

    , h2_ "round (default), spaced list, dismissable with icon"
    , example
        $ fragment
        $ allColors
        $ _ { dismissable = true
            , content = loremIpsum_
            , icon = bannerIcon
            }

    , h2_ "round (default), spaced list, dismissable"
    , example
        $ fragment
        $ allColors
        $ _ { dismissable = true
            , title = bannerTitle
            , content = loremIpsum
            }

    , h2_ "action banner, round (default), spaced list, non-dismissable with icon"
    , example
        $ fragment
        $ allColors
        $ Banner.actionBanner simpleActions
        $ _ { content = loremIpsum_
            , icon = bannerIcon
            }

    , h2_ "action banner, round (default), spaced list, non-dismissable"
    , example
        $ fragment
        $ allColors
        $ Banner.actionBanner simpleActions
        $ _ { content = loremIpsum_ }

    , h2_ "action banner, round (default), spaced list, dismissable"
    , example
        $ fragment
        $ allColors
        $ Banner.actionBanner simpleActions
        $ _ { dismissable = true
            , title = bannerTitle
            , content = loremIpsum
            }

    , h2_ "action banner, round (default), spaced list, dismissable with icon"
    , example
        $ fragment
        $ allColors
        $ Banner.actionBanner simpleActions
        $ _ { dismissable = true
            , title = bannerTitle
            , content = loremIpsum
            , icon = bannerIcon
            }

    , h2_ "action banner, round (default), spaced list, non-dismissable"
    , example
        $ fragment
        $ allColors
        $ Banner.actionBanner twoButtonActions
        $ _ { content = loremIpsum_ }

    , h2_ "action banner, round (default), spaced list, dismissable"
    , example
        $ fragment
        $ allColors
        $ Banner.actionBanner twoButtonActions
        $ _ { dismissable = true
            , title = bannerTitle
            , content = loremIpsum
            }

    , h2_ "action banner, round (default), spaced list, non-dismissable"
    , example
        $ fragment
        $ allColors
        $ Banner.actionBanner actions
        $ _ { content = loremIpsum_ }

    , h2_ "action banner, round (default), spaced list, dismissable"
    , example
        $ fragment
        $ allColors
        $ Banner.actionBanner actions
        $ _ { dismissable = true
            , title = bannerTitle
            , content = loremIpsum
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

    loremIpsum_ :: Array JSX
    loremIpsum_ = [ R.text "Lorem ipsum dolor sit amet, consectetur adipiscing elit." ]

    loremIpsum :: Array JSX
    loremIpsum =
      [ R.text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Etiam varius erat a purus fermentum luctus. Sed porta nisi eget mauris facilisis, a accumsan erat feugiat. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas. Pellentesque maximus mauris nulla."
      ]

    bannerTitle :: Array JSX
    bannerTitle =
      [ subsectionHeader_ "Lorem ipsum dolor sit amet"
      ]

    bannerIcon :: Array JSX
    bannerIcon =
      [ avatar
          { image: R.img { src: "https://s3.amazonaws.com/lumi-blog/avatars/_x600/flexo.jpg" }
          , size: large
          , style: R.css { height: "40px", width: "40px" }
          }
      ]

    simpleActions :: Array JSX
    simpleActions =
      [ Button.button Button.primary
          { title = "Submit"
          }
      ]

    twoButtonActions :: Array JSX
    twoButtonActions =
      [ Button.button Button.secondary
          { title = "Clear"
          , style = R.css { backgroundColor: "transparent" }
          }
      , Button.button Button.primary
          { title = "Submit"
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
