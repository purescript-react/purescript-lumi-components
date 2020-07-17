module Lumi.Components.EditableTable where

import Prelude

import Color (cssStringHSLA)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import JSS (JSS, jss)
import Lumi.Components (($$$))
import Lumi.Components.Color (colors)
import Lumi.Components.Column (column_)
import Lumi.Components.Icon (IconType(..), icon, icon_)
import Lumi.Components.Text (nbsp)
import Lumi.Components2.Box (row)
import Lumi.Components2.Button (button, _linkStyle)
import Lumi.Components2.Text as T
import Lumi.Styles as S
import Lumi.Styles.Box (FlexAlign(..), _align, _justify, _row)
import Lumi.Styles.Theme (LumiTheme(..))
import React.Basic.Classic (Component, JSX, createComponent, element, empty, makeStateless)
import React.Basic.DOM as R

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
  , removeCell: defaultRemoveCell
  , rows: Left []
  , rowEq: eq
  , summary: empty
  }

defaultRemoveCell :: forall row. Maybe (row -> Effect Unit) -> row -> JSX
defaultRemoveCell onRowRemove item =
  onRowRemove # Array.foldMap \onRowRemove' ->
    button
    $ _linkStyle
    $ S.style
        ( \(LumiTheme { colors }) ->
          S.css
            { fontSize: S.px 20
            , lineHeight: S.px 20
            , textDecoration: S.important S.none
            , color: S.color colors.black1
            , "&:hover": S.nested $ S.css
                { color: S.color colors.black
                }
            , "lumi-font-icon::before": S.nested $ S.css
                { verticalAlign: S.str "baseline"
                }
            }
        )
    $ _ { onPress = onRowRemove' item
        , content = [ icon_ Bin ]
        }

component :: forall row. Component (EditableTableProps row)
component = createComponent "EditableTableExample"

editableTable :: forall row. EditableTableProps row -> JSX
editableTable = makeStateless component render
  where
    render props =
      let
        lengthRows = case _ of
          Left arr -> Array.length arr
          Right arr -> NonEmptyArray.length arr
      in container
        [ if props.readonly && lengthRows props.rows == 0
            then mempty
            else header props.columns
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
            canAddRows = not props.readonly && lengthRows props.rows < props.maxRows
          in
            footer
              canAddRows
              props.onRowAdd
              props.addLabel
              props.summary
              (Array.length props.columns + 1)
        ]
        props.readonly
      where
        row_ = tableRow props.columns props.onRowRemove props.removeCell


    container children readonly =
      editableTableElement
        { children: R.table { children, className: "lumi" }
        , "data-readonly": readonly
        }

    header columns =
      R.thead_
        [ R.tr_ $
            (columns <#> \column -> R.th_ [ R.text column.label ])
              <> [ R.th_ [ {- removal column -} ] ]
        ]

    body =
      R.tbody_

    tableRow columns onRowRemove removeCell isRemovable item =
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
                    [ row
                      $ _align Start
                      $ _justify SpaceBetween
                      $ S.style_ (S.css { flexFlow: S.str "row-reverse wrap" })
                      $$$ [ summary
                          , guard canAddRows
                              $ button
                              $ _linkStyle
                              $ _row
                              $ _align Baseline
                              $ S.style_
                                  ( S.css
                                      { fontSize: S.px 14
                                      , lineHeight: S.px 17
                                      , "lumi-font-icon::before": S.nested $ S.css
                                          { verticalAlign: S.str "baseline"
                                          }
                                      }
                                  )
                              $ _ { onPress = onRowAdd
                                  , content =
                                      [ icon
                                          { type_: Plus
                                          , style: R.css { fontSize: "11px" }
                                          }
                                      , T.text $$$ nbsp <> nbsp <> addLabel
                                      ]
                                  }
                          ]
                    ]
                , colSpan: columnCount
                }
            ]
        ]

    editableTableElement = element $ unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-editable-table"

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
                  , padding: "12px 0"
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
          , "&[data-readonly=\"true\"]":
              { "& > table.lumi":
                  { "& th, & td":
                    { padding: "8px 0"
                    }
                  }
              }
          }
      }
  }
