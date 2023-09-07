module Lumi.Components.Tab
  ( TabId(..)
  , TabKey(..)
  , TabProps
  , TabsProps
  , URLParts
  , fromUrlParts
  , getTab
  , hasTab
  , setTab
  , styles
  , tab
  , tabComponent
  , tabs
  , tabsComponent
  , urlParts
  )
  where

import Prelude

import Color (cssStringHSLA)
import Control.Alt ((<|>))
import Data.Array (filter, find, head, index, partition)
import Data.Array as Array
import Data.Maybe (Maybe(..), fromJust, fromMaybe, maybe)
import Data.Newtype (class Newtype, un)
import Data.String (Pattern(..), drop, indexOf, joinWith, null, split)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import JSS (JSS, jss)
import JSURI (encodeURIComponent)
import Lumi.Components.Badge (badge)
import Lumi.Components.Color (colors)
import Lumi.Components.Column (column_)
import Lumi.Components.Link as Link
import Partial.Unsafe (unsafePartial)
import React.Basic.Classic (Component, JSX, createComponent, element, empty, fragment, makeStateless)
import React.Basic.DOM (CSS, mergeStyles)
import React.Basic.DOM as R
import Web.HTML.History (URL(..))

unsafeEncodeURIComponent :: String -> String
unsafeEncodeURIComponent str = unsafePartial $ fromJust $ encodeURIComponent str

type TabProps =
  { active :: Boolean
  , href :: URL
  , label :: JSX
  , count :: Maybe Int
  , navigate :: Maybe (Effect Unit)
  , style :: CSS
  , testId :: Maybe String
  }

tabComponent :: Component TabProps
tabComponent = createComponent "Tab"

tab :: TabProps -> JSX
tab = makeStateless tabComponent render
  where
    lumiTabElement = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-tab")

    render props =
      lumiTabElement
        { children:
            [ Link.link Link.defaults
                { href = props.href
                , navigate = props.navigate
                , testId = props.testId
                , text =
                    fragment
                      [ props.label
                      , props.count # maybe mempty \count ->
                          badge
                            { style: R.css { marginLeft: "8px" }
                            , background: colors.black5
                            , color: colors.secondary
                            , text: show count
                            }
                      ]
                }
            ]
        , "data-active": props.active
        , style: props.style
        }

newtype TabKey = TabKey String
derive instance ntTabKey :: Newtype TabKey _
newtype TabId = TabId String
derive instance ntTabId :: Newtype TabId _
derive instance eqTabId :: Eq TabId

type TabsProps =
  { currentLocation :: URL
  , useHash :: Boolean
  , navigate :: Maybe (URL -> Effect Unit)
  , queryKey :: TabKey
  , style :: CSS
  , tabStyle :: CSS
  , selectedTabStyle :: CSS
  , tabs :: Array
      { content :: Unit -> JSX
      , id :: TabId
      , label :: String
      , count :: Maybe Int
      , testId :: Maybe String
      }
  }

tabsComponent :: Component TabsProps
tabsComponent = createComponent "Tabs"

tabs :: TabsProps -> JSX
tabs = makeStateless tabsComponent render
  where
    lumiTabsElement = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-tabs")
    lumiTabContentElement = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-tab-content")

    render props =
      column_
        [ lumiTabsElement
            { style: props.style
            , children: props.tabs <#> \{ id, label, count, testId } ->
                let
                  active = case selectedTabOrHead of
                    Nothing -> false
                    Just st -> st == id
                  href = hrefFor id
                in
                  tab
                    { active
                    , href
                    , label: R.text label
                    , count
                    , navigate: map (_ $ href) props.navigate
                    , style:
                        if active
                          then mergeStyles [ props.tabStyle, props.selectedTabStyle ]
                          else props.tabStyle
                    , testId
                    }
            }
        , case selectedTabOrHead >>= \st -> find (eq st <<< _.id) props.tabs of
            Nothing -> empty
            Just { content } ->
              lumiTabContentElement { children: content unit }
        ]

      where
        parts = urlParts props.currentLocation

        selectedTab = getTab props.queryKey (if props.useHash then parts.hash.query else parts.query)

        selectedTabOrHead = selectedTab <|> head (_.id <$> props.tabs)

        hrefFor id = fromUrlParts $
          if props.useHash
            then parts { hash = parts.hash { query = setTab props.queryKey id parts.hash.query } }
            else parts { query = setTab props.queryKey id parts.query }

type URLParts =
  { base :: String
  , path :: String
  , query :: String
  , hash ::
      { path :: String
      , query :: String
      }
  }

foreign import urlParts :: URL -> URLParts

fromUrlParts :: URLParts -> URL
fromUrlParts p = URL (p.base <> p.path <> p.query <> p.hash.path <> p.hash.query)

getTab :: TabKey -> String -> Maybe TabId
getTab key query =
  let
    clean = unsafeEncodeURIComponent

    splitQueryParams = filter (not null) $ split (Pattern "&") $ drop 1 query

    startsWith p s = eq (Just 0) $ indexOf p s

    tabsParam = filter (startsWith (Pattern "tabs=")) splitQueryParams

    tabsSplit =
      filter (startsWith $ Pattern $ unsafeEncodeURIComponent (un TabKey key <> "."))
        (filter (not null) $ split (Pattern ",") $ drop 5 $ fromMaybe "" $ head tabsParam)
  in
    map TabId (join (head (map (flip index 1 <<< split (Pattern ".")) tabsSplit)))

hasTab :: TabKey -> TabId -> String -> Boolean
hasTab key id query =
  let
    clean = unsafeEncodeURIComponent

    splitQueryParams = filter (not null) $ split (Pattern "&") $ drop 1 query

    startsWith p s = eq (Just 0) $ indexOf p s

    tabsParam = filter (startsWith (Pattern "tabs=")) splitQueryParams

    tabsSplit =
      filter (eq $ unsafeEncodeURIComponent (un TabKey key <> "." <> un TabId id))
        (filter (not null) $ split (Pattern ",") $ drop 5 $ fromMaybe "" $ head tabsParam)
  in
    not Array.null tabsSplit

setTab :: TabKey -> TabId -> String -> String
setTab key id query = "?" <> joinWith "&" params
  where
    params =
      let
        clean = unsafeEncodeURIComponent

        splitQueryParams = filter (not null) $ split (Pattern "&") $ drop 1 query

        startsWith p s = eq (Just 0) $ indexOf p s

        { yes: oldTabsParam, no: otherParams } = partition (startsWith (Pattern "tabs=")) splitQueryParams

        newTabsParam =
          "tabs=" <> joinWith "," (otherTabs <> [ (clean (un TabKey key) <> "." <> clean (un TabId id)) ])
          where
            otherTabs =
              filter (not startsWith (Pattern (clean (un TabKey key))))
                (filter (not null) $ split (Pattern ",") $ drop 5 $ fromMaybe "" $ head oldTabsParam)
      in
        otherParams <> [ newTabsParam ]

styles :: JSS
styles = jss
  { "@global":
      { "lumi-tab":
          { flex: "0 0 auto"
          , display: "flex"
          , alignItems: "center"
          , boxSizing: "border-box"
          , "& > a.lumi":
              { flex: "0 0 auto"
              , display: "flex"
              , alignItems: "baseline"
              , fontSize: "14px "
              , cursor: "pointer"
              , whiteSpace: "nowrap"
              , touchAction: "manipulation"
              , userSelect: "none"
              , "&, &:hover, &:visited":
                  { color: cssStringHSLA colors.black1
                  , textDecoration: "none"
                  }
              }

          , "&[data-active=\"true\"]":
              { boxShadow: "inset 0 -1px 0 0 " <> cssStringHSLA colors.black
              , "& > a.lumi, & > a.lumi:hover, & > a.lumi:visited":
                  { color: cssStringHSLA colors.black
                  }
            }
          }

      , "lumi-tabs":
          { display: "flex"
          , flexDirection: "row"
          , boxSizing: "border-box"
          , boxShadow: "inset 0 -1px 0 0 " <> cssStringHSLA colors.black4
          , height: "42px"

          , overflowY: "hidden"
          , overflowX: "auto"

          , "& > lumi-tab:not(:first-child)": { marginLeft: "22px" }
          , "&[data-variant=\"full\"] lumi-tab": { marginLeft: "22px" }

          -- hide the horizontal scrollbar, it overlaps the tab content
          , "&::-webkit-scrollbar":
              { height: "0 !important"
              }
          , scrollbarWidth: "none"
          , "MsOverflowStyle": "none"
          }

      , "lumi-tab-content":
          { boxSizing: "border-box"
          , display: "flex"
          , flexFlow: "column"
          }
      }
  }