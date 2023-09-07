module Lumi.Components2.Tabs where

import Prelude

import Color (cssStringHSLA)
import Data.Foldable (find)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid as Monoid
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Lumi.Components (LumiComponent, lumiComponent, ($$$))
import Lumi.Components.Responsive (withMobile)
import Lumi.Components.Tab (tab)
import Lumi.Components2.Box as Box
import Lumi.Styles (Style, StyleModifier, style, style_, toCSS)
import Lumi.Styles.Box as Styles.Box
import Lumi.Styles.Responsive (onDesktop)
import Lumi.Styles.Theme (LumiTheme(..), useTheme)
import React.Basic.Classic (JSX)
import React.Basic.Classic as React
import React.Basic.DOM as R
import React.Basic.Emotion as E
import React.Basic.Hooks as Hooks
import Web.HTML.History (URL)

type TabsProps' tab r =
  { selectedTab :: tab
  , navigate :: tab -> r
  , tabs :: Array
      { tab :: tab
      , content :: Unit -> JSX
      , label :: String
      , count :: Maybe Int
      , testId :: Maybe String
      }
  , css :: LumiTheme -> Style
  }

-- some context that includes a navigation effect
type NavigationEnv r =
  { onNavigate :: EffectFn1 URL Unit
  | r
  }

-- Create a tabLayout component. This effect is not safe to
-- force at the module level because of the `Eq` constraint. Run
-- this effect in the consuming components creation effect.
mkTabLayout
  :: forall tab r env
   . Eq tab
  => Hooks.Hook (Hooks.UseContext (NavigationEnv env)) (NavigationEnv env)
  -> (NavigationEnv env  -> r -> URL)
  -> Hooks.Component (TabsProps' tab r)
mkTabLayout useEnv print = do
  Hooks.component "FJTabLayout" \props -> Hooks.do
    env <- useEnv
    pure $ tabLayout _
      { selectedTab = fromMaybe "" do
          selected <- find (_.tab >>> (_ == props.selectedTab)) props.tabs
          pure selected.label
      , navigate = Just $ runEffectFn1 env.onNavigate
      , css = props.css
      , tabs =
          props.tabs <#> \tab ->
            { content: tab.content
            , href: print env $ props.navigate tab.tab
            , label: tab.label
            , count: tab.count
            , testId: tab.testId
            }
      }
type TabsProps =
  ( selectedTab :: String
  , navigate :: Maybe (URL -> Effect Unit)
  , tabs :: Array
      { content :: Unit -> JSX
      , href :: URL
      , label :: String
      , count :: Maybe Int
      , testId :: Maybe String
      }
  )

tabLayout :: LumiComponent TabsProps
tabLayout = unsafePerformEffect do
  lumiComponent "TabLayout"
    { selectedTab: ""
    , navigate: Nothing
    , tabs: []
    } \props -> Hooks.do

      theme <- useTheme
      pure $
        Box.column
        $ Styles.Box._flex
        $$$
          [ E.element R.div'
              { className: props.className
              , css: toCSS tabLayoutStyle theme <> props.css theme
              , children: props.tabs <#> \{ href, label, count, testId } ->
                  let
                    active = props.selectedTab == label
                  in withMobile \isMobile ->
                    tab
                      { active
                      , href
                      , label: R.text label
                      , count
                      , navigate: map (_ $ href) props.navigate
                      , style: R.mergeStyles
                          [ R.css { marginLeft: 0, marginRight: "22px", height: "42px" }
                          , Monoid.guard (isMobile) $ R.css { padding: "2px 0", marginLeft: 0, marginRight: "22px", height: "42px" }
                          ]
                      , testId
                      }
              }
          , case find (eq props.selectedTab <<< _.label) props.tabs of
              Nothing -> React.empty
              Just { content } ->
                E.element R.div'
                  { className: "lumi-tab-content"
                  , css: toCSS tabContentStyle theme
                  , children: [ content unit ]
                  }
          ]
      where
      tabContentStyle :: StyleModifier
      tabContentStyle =
        style_ $ E.css
          { boxSizing: E.borderBox
          , display: E.flex
          , flexFlow: E.column
          , flex: E.str "1"
          }

      tabLayoutStyle :: StyleModifier
      tabLayoutStyle =
        Styles.Box.box
          <<< Styles.Box._row
          <<< onDesktop (style_ (E.css { height: E.px 42 }))
          <<< style \(LumiTheme theme) ->
            ( E.css
                { height: E.str "auto"
                , display: E.flex
                , flex: E.str "0 0 auto"
                , flexWrap: E.wrap
                , alignItems: E.center
                , boxSizing: E.borderBox
                , boxShadow: E.str $ "inset 0 -1px 0 0 " <> cssStringHSLA theme.colors.black4
                , overflowY: E.hidden
                , overflowX: E.auto
                -- hide the horizontal scrollbar, it overlaps the tab content
                , "&::-webkit-scrollbar":
                     E.nested $ E.css
                      { height: E.important $ E.px 0
                      }
                , scrollbarWidth: E.none
                , msOverflowStyle: E.none
                }
            )
