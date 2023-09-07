module Lumi.Components.List where

import Prelude

import Color (cssStringHSLA)
import Data.Array (mapMaybe)
import Data.Maybe (Maybe(..))
import Data.Nullable (notNull)
import Effect.Unsafe (unsafePerformEffect)
import JSS (JSS, jss)
import Lumi.Components.Color (colors, colorNames)
import Lumi.Components.Row (row)
import Lumi.Components.Size (Size(..))
import Lumi.Components.Text as T
import React.Basic.Classic (Component, JSX, createComponent, element, makeStateless)
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

    lumiList = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-list")
    lumiListRow = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-list-row")
    lumiListRowCell = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-list-row-cell")

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

keyValueList
  :: { rightAligned :: Boolean
     , rows ::
         Array
           { label :: String
           , value :: JSX
           }
     , borders :: Boolean
     }
  -> JSX
keyValueList args = keyValueList' $ args { rows = textRow <$> args.rows }
  where
    textRow { label, value } =
      { value: value
      , label: T.text T.body
                { style = R.css {}
                , color = notNull colorNames.black1
                , children = [ R.text label ]
                }
     }

keyValueList'
  :: { rightAligned :: Boolean
     , rows ::
         Array
           { label :: JSX
           , value :: JSX
           }
     , borders :: Boolean
     }
  -> JSX
keyValueList' { rightAligned, rows, borders } =
  let
    lumiKeyValueListElement = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-key-value-list")
    lumiKeyValueListLabelElement = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-key-value-list-label")
    lumiKeyValueListValueElement = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-key-value-list-value")

    toRows r =
      r <#> \{ label, value } ->
        [ row
            { style: R.css
                { alignItems: "center"
                , justifyContent: "space-between"
                , width: "100%"
                }
            , children:
                [ lumiKeyValueListLabelElement
                    { children:
                        [ label
                        ]
                    , style: R.css {}
                    }
                , lumiKeyValueListValueElement
                    { children: [ value ]
                    , style: R.css
                        { justifyContent: if rightAligned then "flex-end" else "flex-start"
                        }
                    }
                ]
            }
        ]
  in
    lumiKeyValueListElement
      { children:
          [ if borders
              then list compactList
                { rows = toRows rows
                }
              else borderlessList compactList
                { rows = toRows rows
                }
          ]
      , style: R.css { width: "100%" }
      }

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
                { "& > lumi-list-row > lumi-list-row-cell:not(:first-child)":
                    { alignItems: "flex-end"
                    }
                }
            }
      , "lumi-key-value-list":
          { "& lumi-key-value-list-label":
              { "flex": "3 5 0%"
              , "padding": "8px 0"
              }
          , "& lumi-key-value-list-value":
              { "display": "flex"
              , "flexFlow": "row"
              , "alignItems": "center"
              , "flex": "7 7 0%"
              , "flexWrap": "wrap"
              }
          , "@media (max-width: 860px)":
              { "width": "100%"
              , "& lumi-key-value-list-label":
                  { "flex": "initial"
                  }
              , "& lumi-key-value-list-value":
                  { "flex": "initial"
                  }
              }
          }
      }
  }
