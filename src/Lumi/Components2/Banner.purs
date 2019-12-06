module Lumi.Components2.Banner
  ( Banner
  , BannerProps
  , banner
  , actionBanner
  , primary
  , active
  , warning
  , error
  , module Styles.Banner
  ) where

import Prelude

import Color (lighten)
import Data.Foldable (fold)
import Data.Monoid as Monoid
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Data.Tuple.Nested ((/\))
import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components (LumiComponent, PropsModifier, lumiComponent, ($$$))
import Lumi.Components.Icon as Icon
import Lumi.Components.Spacing (Space(..))
import Lumi.Components2.Box (box)
import Lumi.Styles (StyleModifier, color, css, nested, prop, str, style, style_, toCSS)
import Lumi.Styles.Banner (banner) as S
import Lumi.Styles.Border (_listSpaced, _listCompact) as Styles.Banner
import Lumi.Styles.Box (FlexAlign(..), _align, _column, _flex, _interactive, _justify, _row, box) as S
import Lumi.Styles.Responsive (desktopQuery)
import Lumi.Styles.Responsive (onDesktop) as S
import Lumi.Styles.Theme (LumiTheme(..), useTheme)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)
import React.Basic.Emotion as E
import React.Basic.Hooks (useState)
import React.Basic.Hooks as React

data Banner = Banner

type BannerProps =
  ( component :: Banner
  , dismissable :: Boolean
  , content :: Array JSX
  )

banner :: LumiComponent BannerProps
banner =
  unsafePerformEffect do
    lumiComponent "Banner" defaults \props -> React.do
      theme <- useTheme
      visible /\ setVisible <- useState true
      pure
        $ Monoid.guard visible
        $ box
        $ S.banner
        $ style props.css
        $ _ { className = props.className
            , content =
                [ box
                  $ S._align S.Stretch
                  $ S.onDesktop (S._row <<< S._align S.Center)
                  $ S._flex
                  $$$ props.content
                , Monoid.guard props.dismissable
                    $ E.element lumiButtonElement
                    $ { "aria-label": Nullable.notNull "dismiss"
                      , className: ""
                      , css: toCSS dismissButtonStyle theme
                      , onClick: capture_ $ setVisible \_ -> false
                      , type: "button"
                      , children:
                          [ Icon.icon
                              { style: R.css { fontSize: "12px" }
                              , type_: Icon.Remove
                              }
                          ]
                      }
                ]
            }
  where
    defaults :: Record BannerProps
    defaults =
      { component: Banner
      , content: []
      , dismissable: false
      }

    lumiButtonElement ::
      forall attrs attrs_.
      Union attrs attrs_ ( | R.Props_button ) =>
      ReactComponent
        { "aria-label" :: Nullable String
        | attrs
        }
    lumiButtonElement = R.unsafeCreateDOMComponent "button"

    dismissButtonStyle :: StyleModifier
    dismissButtonStyle =
      S.box
      <<< S._interactive
      <<< style \(LumiTheme { colors }) ->
            css
              { outline: str "none"
              , border: str "none"
              , background: str "none"
              , padding: str "0"
              , margin: str "0 0 0 16px"
              , "&:hover": nested $ css
                  { color: color colors.black1
                  }
              }

actionBanner :: Array JSX -> PropsModifier BannerProps
actionBanner actions f =
  f >>> \props -> props
    { content =
        [ box
          $ S._column
          $ S._flex
          $ S.onDesktop (S._row <<< S._align S.Center)
          $$$ [ box
                $ S._flex
                $$$ props.content
              , box
                $ S._row
                $ S._align S.Center
                $ S._justify S.Start
                $ S.onDesktop (S._justify S.End)
                $ style_
                    ( fold
                        [ css
                            { margin: str "8px 0 0"
                            , "& :not(:first-child)": nested $ css
                                { marginLeft: prop S8
                                }
                            }
                        , desktopQuery $ css { margin: str "0 0 0 8px" }
                        ]
                    )
                $$$ actions
              ]
        ]
    }

primary :: forall props. PropsModifier (component :: Banner | props)
primary =
  style \(LumiTheme { colors }) ->
    css
      { backgroundColor: color colors.primary3
      }

active :: forall props. PropsModifier (component :: Banner | props)
active =
  style \(LumiTheme { colors }) ->
    css
      { backgroundColor: color $ lighten 0.4137 $ colors.accent1
      }

warning :: forall props. PropsModifier (component :: Banner | props)
warning =
  style \(LumiTheme { colors }) ->
    css
      { backgroundColor: color $ lighten 0.4137 $ colors.accent2
      }

error :: forall props. PropsModifier (component :: Banner | props)
error =
  style \(LumiTheme { colors }) ->
    css
      { backgroundColor: color $ lighten 0.4137 $ colors.accent3
      }
