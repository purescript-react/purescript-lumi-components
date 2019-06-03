module Lumi.Components.EditableTable where

import Prelude

import Color (cssStringHSLA)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import JSS (JSS, jss)
import Lumi.Components.Button as Button
import Lumi.Components.Color (colors)
import Lumi.Components.Column (column_)
import Lumi.Components.Icon (IconType(..), icon_)
import Lumi.Components.Row as Row
import React.Basic (Component, JSX, createComponent, element, empty, makeStateless)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_, preventDefault, stopPropagation)
import React.Basic.Events (handler)

type EditableTableProps row =
  { addLabel :: String
  , columns :: Array
      { label :: String
      , renderCell :: row -> JSX
      }
  , maxRows :: Int
  , onRowAdd :: Effect Unit
  , onRowRemove :: row -> Effect Unit
  , readonly :: Boolean
  , removeCell :: Maybe (row -> Effect Unit) -> row -> JSX
  , rows :: Either (Array row) (NonEmptyArray row)
  , rowEq :: row -> row -> Boolean
  , summary :: JSX
  }

editableTableDefaults :: forall row. Eq row => EditableTableProps row
editableTableDefaults =
  { addLabel: "Add row"
  , columns: []
  , maxRows: top
  , onRowAdd: pure unit
  , onRowRemove: \_ -> pure unit
  , readonly: false
  , removeCell
  , rows: Left []
  , rowEq: eq
  , summary: empty
  }
  where
  removeCell onRowRemove item =
    onRowRemove # Array.foldMap \onRowRemove' ->
      R.a
        { children: [ icon_ Bin ]
        , className: "lumi"
        , onClick: capture_ $ onRowRemove' item
        , role: "button"
        , style: R.css { fontSize: "20px", lineHeight: "20px", textDecoration: "none" }
        }

component :: forall row. Component (EditableTableProps row)
component = createComponent "EditableTableExample"

editableTable :: forall row. EditableTableProps row -> JSX
editableTable = makeStateless component render
  where
    render props =
      container
        [ header props.columns
        , body case props.rows of
            Left rows -> map (row_ (not props.readonly)) rows
            Right rows ->
              if NonEmptyArray.length rows == 1
                then
                  [ row_ false (NonEmptyArray.head rows)
                  ]
                else
                  map (row_ (not props.readonly)) (NonEmptyArray.toArray rows)
        , let
            lengthRows = case _ of
              Left arr -> Array.length arr
              Right arr -> NonEmptyArray.length arr
            canAddRows = not props.readonly && lengthRows props.rows < props.maxRows
          in
            footer
              canAddRows
              props.onRowAdd
              props.addLabel
              props.summary
              (Array.length props.columns + 1)
        ]
      where
        row_ = row props.columns props.onRowRemove props.removeCell


    container children =
      editableTableElement
        { children: R.table { children, className: "lumi" }
        }

    header columns =
      R.thead_
        [ R.tr_ $
            (columns <#> \column -> R.th_ [ R.text column.label ])
              <> [ R.th_ [ {- removal column -} ] ]
        ]

    body =
      R.tbody_

    row columns onRowRemove removeCell isRemovable item =
      R.tr_ $
        (cell item <$> columns)
          <> [ R.td_
                [ if isRemovable
                    then removeCell (Just onRowRemove) item
                    else removeCell Nothing item
                ]
              ]

    cell item column =
      R.td_ [ column_ [ column.renderCell item ] ]


    footer canAddRows onRowAdd addLabel summary columnCount =
      R.tfoot_
        [ R.tr_
            [ R.td
                { children:
                    [ Row.row
                        { children:
                            [ summary
                            , if not canAddRows
                                then empty
                                else Button.iconButton Button.iconButtonDefaults
                                  { title = addLabel
                                  , onPress =
                                      handler
                                        (preventDefault >>> stopPropagation)
                                        \_ -> onRowAdd
                                  , iconLeft = Just Plus
                                  }
                            ]
                        , style: R.css
                            { justifyContent: "space-between"
                            , flexFlow: "row-reverse wrap"
                            }
                        }
                    ]
                , colSpan: toNumber columnCount
                }
            ]
        ]

    editableTableElement = element $ R.unsafeCreateDOMComponent "lumi-editable-table"

styles :: JSS
styles = jss
  { "@global":
      { "lumi-editable-table":
          { boxSizing: "border-box"
          , display: "flex"
          , flexFlow: "column"
          , alignSelf: "stretch"

          , "& > table.lumi":
              { borderCollapse: "separate"
              , borderSpacing: "0"
              , width: "100%"

              , "& thead > tr > th, & tbody > tr > td":
                  { borderBottom: [ "1px", "solid", cssStringHSLA colors.black4 ]
                  }

              , "& th, & td":
                  { "&:not(:first-child)": { paddingLeft: "8px" }
                  , "&:not(:last-child)": { paddingRight: "8px" }
                  , padding: "16px 0"
                  }

              , "& th":
                  { color: cssStringHSLA colors.black1
                  , fontSize: "13px"
                  , lineHeight: "16px"
                  , fontWeight: "400"
                  , textAlign: "left"
                  , whiteSpace: "nowrap"
                  , overflow: "hidden"
                  , textOverflow: "ellipsis"
                  }

              , "& td > lumi-column":
                  { "& > input.lumi": { width: "100%" }
                  , justifyContent: "center"
                  , minHeight: "32px"
                  , fontSize: "14px"
                  , lineHeight: "20px"
                  , fontWeight: "400"
                  }

              , "& tfoot": { padding: "16px 0" }
              }
          }
      }
  }
