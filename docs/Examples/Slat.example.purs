module Lumi.Components.Examples.Slat where

import Prelude

import Data.Array (intercalate, replicate)
import Data.Maybe (Maybe(..))
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components (LumiComponent, StyleModifier, lumiComponent, styleModifier, (%))
import Lumi.Components.Color (colorNames, colors)
import Lumi.Components.Column (column_)
import Lumi.Components.Example (example)
import Lumi.Components.Lockup (userLockup)
import Lumi.Components.Spacing (Space(..), vspace)
import Lumi.Components.Svg (userSvg)
import Lumi.Components.Text as Text
import Lumi.Components2.Box (mkBox)
import Lumi.Components2.Slat (mkSlat)
import Lumi.Styles.Border as Border
import Lumi.Styles.Box (FlexAlign(..))
import Lumi.Styles.Theme (LumiTheme)
import React.Basic (JSX, ReactContext, createContext, fragment)
import React.Basic.DOM as R
import React.Basic.Emotion as E
import React.Basic.Hooks as React
import Web.HTML (window)
import Web.HTML.History (URL(..))
import Web.HTML.Window (alert)

exampleTheme :: LumiTheme
exampleTheme = { colors, colorNames }

docs :: JSX
docs =
  unsafePerformEffect do
    t <- createContext exampleTheme
    box <- mkBox t
    slat <- mkSlat t
    labeledInfo <- mkLabeledInfo t
    let
      exampleSlatContent =
        [ box
          % slatColumn 4 _
              { content =
                [ userLockup { name: "Xiamen, China", description: Nothing, image: userSvg }
                ]
              }
        , labeledInfo
          % slatColumn 1 _
              { title = R.text "Lead time"
              , value = R.text "11 weeks"
              }
        , labeledInfo
          % slatColumn 1 _
              { title = R.text "Quantities"
              , value = R.text "500-2.5k"
              }
        ]

    pure $ column_
      $ intercalate [ vspace S16 ]
          [ [ example
                $ fragment
                $ replicate 3
                $ slat
                % slatWidth _
                    { content = exampleSlatContent
                    }

            , example
                $ fragment
                $ replicate 3
                $ slat
                % Border.interactive exampleTheme
                $ Border.topBottom exampleTheme
                $ _ { content = exampleSlatContent
                    , interaction = Just
                      { onClick: window >>= alert "click!"
                      , tabIndex: 1
                      , href: Nothing
                      }
                    }

            , example
                $ fragment
                $ replicate 9
                $ slat
                % slatWidth
                $ Border.topBottom exampleTheme
                $ _ { content = exampleSlatContent
                    , interaction = Just
                      { onClick: window >>= alert "click!"
                      , tabIndex: 2
                      , href: Just $ URL "#"
                      }
                    }

            ]
          ]

slatWidth :: StyleModifier
slatWidth = styleModifier $ E.css { maxWidth: E.int 500, width: E.str "100%" }

slatColumn :: Int -> StyleModifier
slatColumn flexGrow =
  styleModifier
    $ E.css
    $ { flexGrow: E.int flexGrow
      , "&:not(:first-child)":
        E.nested
          $ E.css
              { marginLeft: E.prop S16
              , alignItems: E.prop End
              }
      }

mkLabeledInfo :: ReactContext LumiTheme -> Effect (LumiComponent ( title :: JSX, value :: JSX ))
mkLabeledInfo t = do
  box <- mkBox t
  lumiComponent "LabeledInfo" defaults \{ className, title, value } -> React.do
    theme <- React.useContext t
    pure
      $ box
      % _
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
