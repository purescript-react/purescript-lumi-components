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
import Data.Array as Array
import Data.Foldable (fold)
import Data.Monoid as Monoid
import Data.Tuple.Nested ((/\))
import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components (LumiComponent, PropsModifier, lumiComponent, lumiElement)
import Lumi.Components.Icon as Icon
import Lumi.Components.Spacing (Space(..))
import Lumi.Components2.Box (box)
import Lumi.Styles (color, css, int, nested, prop, str, styleModifier, styleModifier_)
import Lumi.Styles.Banner (banner) as S
import Lumi.Styles.Border (_listSpaced, _listCompact) as Styles.Banner
import Lumi.Styles.Box (FlexAlign(..), _align, _alignSelf, _column, _flex, _interactive, _justify, _row) as S
import Lumi.Styles.Responsive (desktopQuery)
import Lumi.Styles.Responsive (onDesktop) as S
import Lumi.Styles.Theme (LumiTheme(..), useTheme)
import React.Basic (JSX)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)
import React.Basic.Hooks (useState)
import React.Basic.Hooks as React

data Banner = Banner

type BannerProps =
  ( component :: Banner
  , dismissable :: Boolean
  , icon :: Array JSX
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
        $ lumiElement box
        $ styleModifier props.css
        $ S.banner
        $ _ { className = props.className
            , content =
                [ Monoid.guard (not Array.null props.icon)
                    $ lumiElement box
                    $ styleModifier_
                        ( css
                          { marginRight: int 16
                          }
                        )
                    $ _ { content = props.icon
                        }
                , lumiElement box
                  $ S._align S.Stretch
                  $ S.onDesktop (S._row >>> S._align S.Center)
                  $ S._flex
                  $ _ { content = props.content }
                , Monoid.guard props.dismissable
                    $ lumiElement box
                    $ S._alignSelf S.Start
                    $ S.onDesktop (S._alignSelf S.Center)
                    $ dismissButtonStyle
                    $ _ { content =
                            [ Icon.icon
                                { style: R.css { fontSize: "12px" }
                                , type_: Icon.Remove
                                }
                            ]
                        , onClick = capture_ $ setVisible \_ -> false
                        }
                ]
            }
  where
    defaults :: Record BannerProps
    defaults =
      { component: Banner
      , content: []
      , dismissable: false
      , icon: []
      }

    dismissButtonStyle :: forall props. PropsModifier props
    dismissButtonStyle =
      S._interactive
      >>> styleModifier \(LumiTheme { colors }) ->
            css
              { padding: str "8px"
              , margin: str "0 -8px 0 8px"
              , "&:hover": nested $ css
                  { color: color colors.black1
                  }
              }

actionBanner :: Array JSX -> PropsModifier BannerProps
actionBanner actions f =
  f >>> \props -> props
    { content =
        [ lumiElement box
          $ S._column
          $ S._flex
          $ S.onDesktop (S._row >>> S._align S.Center)
          $ _ { content =
                  [ lumiElement box
                    $ S._flex
                    $ _ { content = props.content }
                  , lumiElement box
                    $ S._row
                    $ S._align S.Center
                    $ S._justify S.End
                    $ styleModifier_
                        ( fold
                            [ css
                                { margin: str "16px 0 0"
                                , "& :not(:first-child)": nested $ css
                                    { marginLeft: prop S8
                                    }
                                }
                            , desktopQuery $ css
                                { margin: str "0 0 0 40px"
                                }
                            ]
                        )
                    $ _ { content = actions }
                  ]
              }
        ]
    }

primary :: forall props. PropsModifier (component :: Banner | props)
primary =
  styleModifier \(LumiTheme { colors }) ->
    css
      { backgroundColor: color colors.primary3
      }

active :: forall props. PropsModifier (component :: Banner | props)
active =
  styleModifier \(LumiTheme { colors }) ->
    css
      { backgroundColor: color $ lighten 0.4137 $ colors.accent1
      }

warning :: forall props. PropsModifier (component :: Banner | props)
warning =
  styleModifier \(LumiTheme { colors }) ->
    css
      { backgroundColor: color $ lighten 0.4137 $ colors.accent2
      }

error :: forall props. PropsModifier (component :: Banner | props)
error =
  styleModifier \(LumiTheme { colors }) ->
    css
      { backgroundColor: color $ lighten 0.4137 $ colors.accent3
      }
