module Lumi.Components.List where

import Prelude

import Color (cssStringHSLA)
import Data.Array (mapMaybe)
import Data.Maybe (Maybe(..))
import JSS (JSS, jss)
import Lumi.Components.Color (colors)
import Lumi.Components.Size (Size(..))
import React.Basic (Component, JSX, createComponent, element, makeStateless)
import React.Basic.DOM as R

type ListProps r =
  { size :: Maybe Size
  , rightAligned :: Boolean
  , rows :: Array (Array JSX)
  | r
  }

listComponent :: ListProps (borders :: Boolean) -> JSX
listComponent = makeStateless (createComponent "List") $ lumiList <<< mapProps
  where
    mapProps props =
      { className: "lumi"
      , "data-size":
          case props.size of
            Just size -> show size
            Nothing -> show Medium
      , "data-right-aligned": props.rightAligned
      , "data-borders": props.borders
      , children: map renderRow props.rows
      }
      where
        renderRow row =
          lumiListRow { children: map renderRowCell row }

        renderRowCell rowCell =
          lumiListRowCell { children: [ rowCell ] }

    lumiList = element (R.unsafeCreateDOMComponent "lumi-list")
    lumiListRow = element (R.unsafeCreateDOMComponent "lumi-list-row")
    lumiListRowCell = element (R.unsafeCreateDOMComponent "lumi-list-row-cell")

list :: ListProps () -> JSX
list props =
  listComponent
    { size: props.size
    , rightAligned: props.rightAligned
    , rows: props.rows
    , borders: true
    }

borderlessList :: ListProps () -> JSX
borderlessList props =
  listComponent
    { size: props.size
    , rightAligned: props.rightAligned
    , rows: props.rows
    , borders: false
    }

defaultList :: ListProps ()
defaultList =
  { size: Just $ Medium
  , rightAligned: false
  , rows: []
  }

compactList :: ListProps ()
compactList =
  { size: Just $ Small
  , rightAligned: false
  , rows: []
  }

type StructuredColumnListProps row =
  { rightAligned :: Boolean
  , rows :: Array row
  , columns :: Array
      { renderCell :: row -> JSX
      }
  }

structuredColumnListComponent :: forall row. Component (StructuredColumnListProps row)
structuredColumnListComponent = createComponent "StructuredColumnList"

structuredColumnList :: forall a. (StructuredColumnListProps a) -> JSX
structuredColumnList = makeStateless structuredColumnListComponent render
  where
    render props =
      list
        { size: Just $ Large
        , rightAligned: props.rightAligned
        , rows: map renderRow props.rows
        }
      where
        renderRow row =
          props.columns `flip mapMaybe` \col ->
              Just $ renderRowCell row col

        renderRowCell row col =
          col.renderCell row

styles :: JSS
styles = jss
  { "@global":
      { "lumi-list":
          { boxSizing: "border-box"
          , display: "flex"
          , flexFlow: "column"
          , listStyleType: "none"
          , borderBottom: [ "1px", "solid", cssStringHSLA colors.black4 ]

          , "& > lumi-list-row":
              { boxSizing: "border-box"
              , display: "flex"
              , flexFlow: "row wrap"
              , justifyContent: "space-between"
              , minHeight: "calc(48px + 1px)"
              , padding: "6px 0"
              , borderTop: [ "1px", "solid", cssStringHSLA colors.black4 ]

              , "& > lumi-list-row-cell":
                  { boxSizing: "border-box"
                  , display: "flex"
                  , flexFlow: "column"
                  , justifyContent: "center"
                  , flex: "1"
                  , maxWidth: "100%"
                  }
              }

            , "&[data-borders=\"false\"]":
                { border: "0"
                , "& > lumi-list-row":
                  { border: "0"
                  }
                }

            , "&[data-size=\"small\"] > lumi-list-row":
                { minHeight: "calc(40px + 1px)"
                , padding: "2px 0"
                }
            , "&[data-size=\"large\"] > lumi-list-row":
                { minHeight: "calc(64px + 1px)"
                , padding: "14px 0"
                }

            , "&[data-right-aligned=\"true\"] > lumi-list-row > lumi-list-row-cell:last-child":
                { flexFlow: "row"
                , justifyContent: "flex-end"
                , alignItems: "center"
                }

            , "@media (max-width: 448px)":
                { "& > lumi-list-row > lumi-list-row-cell":
                    { alignItems: "flex-end"
                    }
                }
            }

      }
  }
