module Lumi.Components.Examples.Slat where

import Prelude

import Data.Array (intercalate, replicate)
import Data.Maybe (Maybe(..))
import Data.Nullable as Nullable
import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components (LumiComponent, lumiComponent, lumiElement, withContent)
import Lumi.Components.Column (column_)
import Lumi.Components.Example (example)
import Lumi.Components.Lockup (userLockup)
import Lumi.Components.Spacing (Space(..), vspace)
import Lumi.Components.Svg (userSvg)
import Lumi.Components.Text as Text
import Lumi.Components2.Box (box)
import Lumi.Components2.Slat as Slat
import Lumi.Styles (StyleModifier, styleModifier_, withStyle)
import Lumi.Styles.Border as Border
import Lumi.Styles.Box (FlexAlign(..))
import Lumi.Styles.Theme (LumiTheme(..), lumiThemeContext, useTheme)
import React.Basic (JSX, fragment)
import React.Basic.DOM as R
import React.Basic.Emotion as E
import React.Basic.Hooks as React
import Web.HTML (window)
import Web.HTML.Window (alert)

docs :: JSX
docs = (flip lumiElement identity) do
  unsafePerformEffect do
    lumiComponent "SlatExample" {className: "" }\_ -> React.do
      theme <- useTheme
      let
        exampleSlatContent =
          [ lumiElement box
              $ withStyle theme do slatColumn 4
              >>> withContent
                [ userLockup { name: "Xiamen, China", description: Nothing, image: userSvg }
                ]
          , lumiElement labeledInfo
              $ withStyle theme do slatColumn 1
              >>> _
                  { title = R.text "Lead time"
                  , value = R.text "11 weeks"
                  }
          , lumiElement labeledInfo
              $ withStyle theme do slatColumn 1
              >>> _
                  { title = R.text "Quantities"
                  , value = R.text "500-2.5k"
                  }
          ]
      pure $ column_
        $ intercalate [ vspace S16 ]
            [ [ example
                  $ fragment
                  $ replicate 3
                  $ lumiElement Slat.slat
                  $ withStyle theme do
                    slatWidth
                      >>> Border.border
                      >>> Border.round
                      >>> Border.listSpaced
                  >>> withContent exampleSlatContent
              , example
                  $ fragment
                  $ replicate 3
                  $ lumiElement Slat.slat
                  $ withStyle theme do
                    Border.border
                      >>> Border.listSpaced
                      >>> Slat.interactive
                          { onClick: window >>= alert "click!"
                          , tabIndex: 1
                          , href: Nothing
                          }
                  >>> withContent exampleSlatContent
              , example
                  $ fragment
                  $ replicate 9
                  $ lumiElement Slat.slat
                  $ withStyle theme do
                    slatWidth
                      >>> Border.border
                      >>> Border.topBottom
                      >>> Border.listCompact
                      >>> Slat.interactive
                          { onClick: window >>= alert "click!"
                          , tabIndex: 1
                          , href: Nothing
                          }
                  >>> withContent exampleSlatContent
              ]
            ]

slatWidth :: forall props. StyleModifier props
slatWidth = styleModifier_ $ E.css { maxWidth: E.int 500, width: E.str "100%" }

slatColumn :: forall props. Int -> StyleModifier props
slatColumn flexGrow =
  styleModifier_
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
labeledInfo = unsafePerformEffect do
  lumiComponent "LabeledInfo" defaults \{ className, title, value } -> React.do
    LumiTheme theme <- React.useContext lumiThemeContext
    pure
      $ lumiElement box
      $ _
          { className = className
          , content =
            [ Text.text
                Text.body
                  { children = [ value ]
                  , color = Nullable.notNull theme.colorNames.black
                  , style = R.css { whiteSpace: "nowrap" }
                  }
            , Text.text
                Text.subtext
                  { children = [ title ]
                  , color = Nullable.notNull theme.colorNames.black1
                  , style = R.css { whiteSpace: "nowrap" }
                  }
            ]
          }
  where
  defaults =
    { title: mempty
    , value: mempty
    }
