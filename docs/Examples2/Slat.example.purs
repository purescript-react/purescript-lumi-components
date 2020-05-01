module Lumi.Components2.Examples.Slat where

import Prelude

import Data.Array (intercalate, replicate)
import Data.Maybe (Maybe(..))
import Data.Nullable as Nullable
import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components (LumiComponent, lumiComponent)
import Lumi.Components.Example (example)
import Lumi.Components.Lockup (userLockup)
import Lumi.Components.Spacing (Space(..), vspace)
import Lumi.Components.Svg (userSvg)
import Lumi.Components.Text (h2_, p_)
import Lumi.Components.Text as Text
import Lumi.Components2.Box (box)
import Lumi.Components2.Slat as Slat
import Lumi.Styles (StyleModifier, style_)
import Lumi.Styles.Box (FlexAlign(..))
import Lumi.Styles.Theme (LumiTheme(..), useTheme)
import React.Basic (JSX, fragment)
import React.Basic.DOM as R
import React.Basic.Emotion as E
import React.Basic.Hooks as React
import Web.HTML (window)
import Web.HTML.Window (alert)

docs :: JSX
docs =
  let
    exampleSlatContent =
      [ box
          $ slatColumn 4
          $ _ { content = [ userLockup { name: "Xiamen, China", description: Nothing, image: userSvg } ] }
      , labeledInfo
          $ slatColumn 1
          $ _
              { title = R.text "Lead time"
              , value = R.text "11 weeks"
              }
      , labeledInfo
          $ slatColumn 1
          $ _
              { title = R.text "Quantities"
              , value = R.text "500-2.5k"
              }
      ]
  in
    intercalate (vspace S8)
      [ p_ "Slats are stackable, bordered containers with optional interactive behavior. They do not prescribe any formatting on their content besides defaulting to a center-aligned row layout."
      , h2_ "square (default), spaced list, interactive, min-width content"
      , example
          $ fragment
          $ replicate 3
          $ Slat.slat
          $ Slat._listSpaced
          $ Slat._interactive
              { onClick: window >>= alert "click!"
              , tabIndex: 1
              , href: Nothing
              }
          $ _ { content = exampleSlatContent }
      , h2_ "square (default), spaced list, interactive (background color), min-width content"
      , example
          $ fragment
          $ replicate 3
          $ Slat.slat
          $ Slat._listSpaced
          $ Slat._interactiveBackground
              { onClick: window >>= alert "click!"
              , tabIndex: 1
              , href: Nothing
              }
          $ _ { content = exampleSlatContent }
      , h2_ "round, spaced list, non-interactive, min-width 500px"
      , example
          $ fragment
          $ replicate 3
          $ Slat.slat
          $ slatExWidth
          $ Slat._round
          $ Slat._listSpaced
          $ _ { content = exampleSlatContent }
      , h2_ "top/bottom, compact list, interactive, min-width 500px"
      , example
          $ fragment
          $ replicate 9
          $ Slat.slat
          $ Slat._interactive
              { onClick: window >>= alert "click!"
              , tabIndex: 1
              , href: Nothing
              }
          $ Slat._topBottom
          $ Slat._listCompact
          $ slatExWidth
          $ _ { content = exampleSlatContent }
      , h2_ "top/bottom, compact list, interactive (background color), min-width 500px"
      , example
          $ fragment
          $ replicate 9
          $ Slat.slat
          $ Slat._interactiveBackground
              { onClick: window >>= alert "click!"
              , tabIndex: 1
              , href: Nothing
              }
          $ Slat._topBottom
          $ Slat._listCompact
          $ slatExWidth
          $ _ { content = exampleSlatContent }
      ]

slatExWidth :: StyleModifier
slatExWidth = style_ $ E.css { maxWidth: E.int 500, width: E.str "100%" }

slatColumn :: Int -> StyleModifier
slatColumn flexGrow =
  style_
    $ E.css
    $ { flexGrow: E.int flexGrow
      , "&:not(:first-child)":
        E.nested
          $ E.css
              { marginLeft: E.prop S16
              , alignItems: E.prop End
              }
      }

labeledInfo :: LumiComponent ( title :: JSX, value :: JSX )
labeledInfo =
  unsafePerformEffect do
    lumiComponent "LabeledInfo" defaults \{ className, css, title, value } -> React.do
      theme@(LumiTheme { colorNames }) <- useTheme
      pure
        $ box
        $ style_ (E.css { label: E.str "labeledInfo" })
        $ _
            { css = css
            , className = className
            , content =
              [ Text.text
                  Text.body
                    { children = [ value ]
                    , color = Nullable.notNull colorNames.black
                    , style = R.css { whiteSpace: "nowrap" }
                    }
              , Text.text
                  Text.subtext
                    { children = [ title ]
                    , color = Nullable.notNull colorNames.black1
                    , style = R.css { whiteSpace: "nowrap" }
                    }
              ]
            }
  where
  defaults =
    { title: mempty
    , value: mempty
    }
