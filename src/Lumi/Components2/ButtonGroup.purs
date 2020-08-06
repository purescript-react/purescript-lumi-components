module Lumi.Components2.ButtonGroup where

import Prelude

import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components (PropsModifier)
import Lumi.Components as L
import Lumi.Components.ZIndex (ziButtonGroup)
import Lumi.Styles (css, nested, px, str, style_, toCSS)
import Lumi.Styles.Box (_row, box)
import Lumi.Styles.Theme (useTheme)
import React.Basic.DOM as R
import React.Basic.Emotion as E
import React.Basic.Hooks (JSX)
import React.Basic.Hooks as React

type ButtonGroupProps
  = ( component :: ButtonGroup
    , content :: Array JSX
    )

buttonGroup :: L.LumiComponent ButtonGroupProps
buttonGroup =
  unsafePerformEffect do
    L.lumiComponent "ButtonGroup" { component: ButtonGroup, content: [] } \props -> React.do
      theme <- useTheme
      pure
        $ E.element R.div'
            { className: props.className
            , children: props.content
            , css: theme # toCSS styles <> props.css
            }

  where
  styles =
    box
      <<< _row
      <<< style_
            ( css
                { label: str "buttonGroup"
                , "& > *:not(:last-child)":
                    nested
                      $ css
                          { marginRight: px 8
                          }
                }
            )


data ButtonGroup = ButtonGroup

type ButtonGroupModifier = forall r. PropsModifier ( component :: ButtonGroup | r )

joined :: ButtonGroupModifier
joined =
  style_
    $ css
        { label: str "joined"
        , "& > *:not(:last-child)":
            nested
              $ css
                  { marginRight: px (-1)
                  , borderTopRightRadius: px 0
                  , borderBottomRightRadius: px 0
                  }
        , "& > *:not(:first-child)":
            nested
              $ css
                  { borderTopLeftRadius: px 0
                  , borderBottomLeftRadius: px 0
                  }
        , "& > *:focus, & > *:hover":
            nested
              $ css
                  { zIndex: px ziButtonGroup
                  }
        }
