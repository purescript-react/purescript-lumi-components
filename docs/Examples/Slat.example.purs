module Lumi.Components.Examples.Slat where

import Prelude

import Data.Array (intercalate, replicate)
import Data.Maybe (Maybe(..))
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components (LumiComponent, lumiComponent, lumiElement', (%))
import Lumi.Components.Color (colorNames, colors)
import Lumi.Components.Column (column_)
import Lumi.Components.Example (example)
import Lumi.Components.Lockup (userLockup)
import Lumi.Components.Spacing (Space(..), vspace)
import Lumi.Components.Svg (userSvg)
import Lumi.Components.Text as Text
import Lumi.Components2.Box (mkBox)
import Lumi.Components2.Slat (mkSlat)
import Lumi.Styles.Border (Border(..))
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
        [ lumiElement' (slatColumn 4) box _
          { content =
            [ userLockup { name: "Xiamen, China", description: Nothing, image: userSvg }
            ]
          }
        , lumiElement' (slatColumn 1) labeledInfo _
          { title = R.text "Lead time"
          , value = R.text "11 weeks"
          }
        , lumiElement' (slatColumn 1) labeledInfo _
          { title = R.text "Quantities"
          , value = R.text "500-2.5k"
          }
        ]

    pure $ column_
      $ intercalate [ vspace S16 ]
          [ [ example
                $ fragment
                $ replicate 3
                $ lumiElement' slatWidth slat _
                    { content = exampleSlatContent
                    }
            , example
                $ fragment
                $ replicate 3
                $ slat
                % _
                    { border = BorderSquare
                    , content = exampleSlatContent
                    , onInteraction = Just
                      { onClick: window >>= alert "click!"
                      , tabIndex: 1
                      , href: Nothing
                      }
                    }
            , example
                $ fragment
                $ replicate 9
                $ lumiElement' slatWidth slat _
                    { border = BorderTopBottom
                    , content = exampleSlatContent
                    , onInteraction = Just
                      { onClick: window >>= alert "click!"
                      , tabIndex: 2
                      , href: Just $ URL "#"
                      }
                    }
            ]
          ]

slatWidth :: E.Style
slatWidth = E.css { maxWidth: E.int 500, width: E.str "100%" }

slatColumn :: Int -> E.Style
slatColumn flexGrow =
  E.css
    { flexGrow: E.int flexGrow
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
  lumiComponent
    "LabeledInfo"
    { className: ""
    , title: mempty :: JSX
    , value: mempty :: JSX
    }
    ( \{ className, title, value } -> React.do
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
    )
