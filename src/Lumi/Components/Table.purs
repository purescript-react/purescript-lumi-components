module Lumi.Components.Table
  ( TableProps
  , ColumnName(..)
  , SortString(..)
  , table
  , labelText
  , styles
  ) where

import Prelude

import Color (cssStringHSLA, hsla, toHSLA)
import Data.Array (delete, elem, find, fold, length, mapMaybe, null, snoc)
import Data.Array as Array
import Data.Either (either)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.Monoid (guard)
import Data.Newtype (class Newtype, un)
import Data.Nullable (Nullable, toMaybe)
import Data.Nullable as Nullable
import Data.String (contains, joinWith, Pattern(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import Foreign.Object (fromHomogeneous)
import JSS (JSS, jss)
import Lumi.Components.Color (colors)
import Lumi.Components.Icon (IconType(ArrowUp, ArrowDown), icon_)
import Lumi.Components.Input (CheckboxState(..), checkbox, input)
import Lumi.Components.Link as Link
import Lumi.Components.Table.FilterDropdown (Item, filterDropdown)
import Lumi.Components.Text (subtext_)
import Lumi.Components.ZIndex (ziTableHeader, ziTableHeaderMenu, ziTableLockedColumn, ziTableLockedColumnHeader)
import Lumi.Components2.ScrollObserver (scrollObserver)
import React.Basic.Classic (Component, JSX, createComponent, element, empty, keyed, make, readProps, readState)
import React.Basic.DOM as R
import React.Basic.DOM.Components.GlobalEvents (windowEvent)
import React.Basic.DOM.Components.Ref (QuerySelector(..), selectorRef)
import React.Basic.DOM.Events (nativeEvent, preventDefault, stopPropagation, targetChecked)
import React.Basic.Events (SyntheticEvent, syntheticEvent)
import React.Basic.Events as Events
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON, writeJSON)
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)
import Web.DOM (Node)
import Web.Event.Event (EventType(..))
import Web.Event.Internal.Types (Event)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)

newtype ColumnName = ColumnName String
derive instance eqColumnName :: Eq ColumnName
derive instance ntColumnName :: Newtype ColumnName _
derive newtype instance rfColumnName :: ReadForeign ColumnName
derive newtype instance wfColumnName :: WriteForeign ColumnName

newtype SortString = SortString String
derive instance eqSortString :: Eq SortString
derive instance ntSortString :: Newtype SortString _


-- helper to ease the use of label text since label and filterLabel both require
-- a Maybe JSX
labelText :: String -> Maybe JSX
labelText = Just <<< subtext_

type TableProps row =
  { name :: String
  , tableInnerStyle :: R.CSS
  , dropdownMenu :: Boolean
  , sortable :: Boolean
  , sort :: Nullable SortString
  , sortBy :: Nullable ColumnName
  , updateSort :: EffectFn2 SortString (Nullable ColumnName) Unit
  , selectable :: Boolean
  , selected :: Nullable (Array String)
  , onSelect :: EffectFn1 (Array String) Unit
  , rows :: Array row
  , getRowKey :: row -> String
  , rowEq :: row -> row -> Boolean
  , variant :: Nullable String
  , primaryColumn ::
      Nullable
        { name :: ColumnName
        , label :: Maybe JSX
        , filterLabel :: Maybe JSX
        , sortBy :: Nullable ColumnName
        , style :: R.CSS
        , onRowClick :: row -> Maybe (Effect Unit)
        , renderCell :: row -> JSX
        , sticky :: Boolean
        }
  , columns ::
      Array
        { required :: Boolean
        , name :: ColumnName
        , label :: Maybe JSX
        , filterLabel :: Maybe JSX
        , sortBy :: Nullable ColumnName
        , style :: R.CSS
        , renderCell :: row -> JSX
        , hidden :: Boolean
        , sticky :: Boolean
        }
  , onColumnChange ::
      Nullable
        ( EffectFn1
          ( Array
              { required :: Boolean
              , name :: ColumnName
              , label :: Maybe JSX
              , filterLabel :: Maybe JSX
              , sortBy :: Nullable ColumnName
              , style :: R.CSS
              , renderCell :: row -> JSX
              , hidden :: Boolean
              , sticky :: Boolean
              }
          )
          Unit
        )
  }

component :: forall row. Component (TableProps row)
component = createComponent "Table"

data Action
  = SyncProps (Maybe (Array { name :: ColumnName, hidden :: Boolean }))
  | OpenMenu { top :: String, left :: String }
  | CloseMenu
  | SetColumnSort (Array Item)
  | OnSelect { shift :: Boolean, key :: String, checked :: Boolean }
  | OnSelectAll Boolean

table :: forall a. TableProps a -> JSX
table = make component
  { initialState
  , didMount: syncProps
  , didUpdate
  , render
  }
  where
    columnSaveKey name = "lumi-table-sort--" <> name

    initialState =
      { columns: []
      , selected: []
      , lastSelected: Nothing
      , showMenu: false
      , menuStyle: { top: "0px", left: "0px" }
      }

    didUpdate self _ = do
      case toMaybe self.props.selected of
        Nothing -> pure unit
        Just selected ->
          when (selected /= self.state.selected) do
            syncProps self

    syncProps self = do
      when (isNothing (toMaybe self.props.onColumnChange) && null self.state.columns) do
        maybeSavedColumnSort <- loadColumnState $ columnSaveKey self.props.name
        self.setState \state -> state
          { columns =
              case maybeSavedColumnSort of
                Nothing -> self.props.columns
                Just savedColumnSort -> sortColumnsBy self.props.columns savedColumnSort
          }
      for_ (toMaybe self.props.selected) \selected ->
        self.setState \state -> state
          { selected = selected
          }

    openMenu self menuStyle = do
      self.setState \state -> state
        { showMenu = true
        , menuStyle = menuStyle
        }

    closeMenu self = do
      self.setState \state -> state { showMenu = false }

    setColumnSort self newColumnOrder = do
      case toMaybe self.props.onColumnChange of
        Nothing ->
          self.setStateThen (\state ->
            let
              columnsSorted = state.columns `sortColumnsBy` newColumnOrder
            in
              state { columns = columnsSorted })
            do
              props <- readProps self
              state <- readState self
              saveColumnState
                (columnSaveKey props.name)
                (getColumnSortFields <$> state.columns)
        Just onColumnChange ->
          runEffectFn1 onColumnChange $ self.props.columns `sortColumnsBy` newColumnOrder

    onSelect self { shift, key, checked } = do
      self.setStateThen (\state ->
        let
          newSelected =
            if checked
              then case shift, state.lastSelected of
                true, Just lastKey ->
                  let
                    rowKeys =
                      map self.props.getRowKey self.props.rows
                    indexedRowKeys =
                      Array.mapWithIndex { ix: _, key: _ } rowKeys
                    keyIndex =
                      _.ix <$> Array.find (eq key <<< _.key) indexedRowKeys
                    lastKeyIndex =
                      _.ix <$> Array.find (eq lastKey <<< _.key) indexedRowKeys
                    newSelectedKeys =
                      map _.key $ Array.filter
                        (\v ->
                          fromMaybe false $
                            betweenSorted <$> keyIndex <*> lastKeyIndex <*> Just v.ix)
                        indexedRowKeys
                  in
                    Array.filter (flip Array.elem (newSelectedKeys <> state.selected)) rowKeys
                _, _ ->
                  snoc state.selected key
              else delete key state.selected
        in
          state
            { selected = newSelected
            , lastSelected = if Array.elem key newSelected then Just key else Nothing
            })
        do
          props <- readProps self
          state <- readState self
          runEffectFn1 props.onSelect state.selected

    onSelectAll self checked = do
      self.setStateThen
        (let
          newSelected =
            if not checked
              then []
              else map self.props.getRowKey self.props.rows
        in
          _ { selected = newSelected })
        do
          props <- readProps self
          state <- readState self
          runEffectFn1 props.onSelect state.selected

    sortColumnsBy columns newColumnOrder =
      let
        matches =
          newColumnOrder `flip Array.mapMaybe` \newCol -> do
            matchedCol <- columns # find (\c -> c.name == newCol.name)
            pure $ matchedCol { hidden = newCol.hidden }
        newColumnOrderNames = map _.name newColumnOrder
        nonMatches =
          columns
            # Array.filter (\c -> c.name `not elem` newColumnOrderNames)
            # map _ { hidden = true }
      in
        matches <> nonMatches

    getColumnSortFields { name, hidden } = { name, hidden }

    render self =
      let
        columns =
          if isNothing $ toMaybe self.props.onColumnChange
            then self.state.columns
            else self.props.columns
      in
        renderLumiTable self columns \tableRef ->
          [ scrollObserver _
              { node = Nullable.notNull tableRef
              , render = \{ hasScrolledY, hasScrolledX } ->
                  R.table
                    { className:
                        let
                          isCompact = contains (Pattern "compact") (show self.props.variant)
                          isFixed = contains (Pattern "fixed") (show self.props.variant)
                        in
                          joinWith " "
                            $ [ "lumi" ]
                            <> guard isCompact [ "compact" ]
                            <> guard isFixed [ "fixed" ]
                            <> guard hasScrolledX [ "has-scrolled-x" ]
                            <> guard hasScrolledY [ "has-scrolled-y" ]
                            <> guard self.props.selectable [ "selectable" ]
                    , children:
                        [ renderTableHead columns tableRef
                        , R.tbody_
                            let
                              tableProps =
                                { columns
                                , primaryColumn
                                , selectable: self.props.selectable
                                , getRowKey: self.props.getRowKey
                                , rowEq: self.props.rowEq
                                , onSelect: onSelect self
                                }
                            in
                              self.props.rows # map \row ->
                                keyed
                                  (tableProps.getRowKey row)
                                  (tableRow
                                    { tableProps
                                    , row
                                    , isSelected:
                                        tableProps.selectable &&
                                        tableProps.getRowKey row `elem` selected
                                    })
                        ]
                    }
              }
          ]
      where
        selected = fromMaybe self.state.selected (toMaybe self.props.selected)

        primaryColumn = toMaybe self.props.primaryColumn

        renderTableHead columns tableRef =
          R.thead
            { onContextMenu: Events.handler (preventDefault >>> stopPropagation) \e -> do
                { x, y } <- runEffectFn2 getMouseEventPositionWithOffset tableRef e
                openMenu self
                  { top: show (y - 2.0) <> "px"
                  , left: show (x - 2.0) <> "px"
                  }
            , children:
                [ R.tr_ $
                    [ renderHeadCheckbox ]
                    <> (maybe [] (pure <<< renderHeadPrimaryCell) primaryColumn)
                    <> (map renderHeadCell columns)
                ]
            }

        renderHeadCheckbox =
          let
            noneChecked = length selected == 0
            allChecked = length selected == dataSize self.props.rows
          in
            if not self.props.selectable
              then empty
              else R.th
                { style: R.css { width: "20px" }
                , onClick: Events.handler (stopPropagation) (const (pure unit))
                , children:
                    [ input checkbox
                        { checked =
                            if noneChecked
                              then Off
                              else if allChecked
                                then On
                                else Indeterminate
                        , onChange = Events.handler targetChecked \checked -> do
                            onSelectAll self $ fromMaybe false checked
                        , style = R.css { position: "static" }
                        }
                    ]
                }

        renderHeadPrimaryCell col = renderHeadCell
          { required: true
          , name: col.name
          , label: col.label
          , filterLabel: col.filterLabel
          , sortBy: col.sortBy
          , style: col.style
          , renderCell: col.renderCell
          , hidden: false
          , sticky: col.sticky
          }

        renderHeadCell col =
          let
            label    = fromMaybe (R.text "") col.label
            sort     = fromMaybe (SortString "asc") (toMaybe self.props.sort)
            sortBy   = toMaybe self.props.sortBy
            flippedSort = if self.props.sortBy /= col.sortBy || sort /= SortString "asc"
                           then SortString "asc"
                           else SortString "desc"
          in
            if col.hidden
              then empty
              else
                R.th
                  { key: un ColumnName col.name
                  , style: col.style
                  , _data: fromHomogeneous { required: show col.required }
                  , className: guard col.sticky "sticky-column"
                  , children:
                      [ case self.props.sortable, toMaybe col.sortBy of
                          true, Just colSortBy ->
                            lumiRowEl
                              { style: R.css { cursor: "pointer", alignItems: "center" }
                              , onClick: Events.handler_ (runEffectFn2 self.props.updateSort flippedSort col.sortBy)
                              , children:
                                  [ R.span
                                      { style: R.css { marginRight: "5px" }
                                      , children: [ label ]
                                      }
                                  , R.span
                                      { style: R.css
                                          { fontSize: "10px"
                                            , visibility:
                                                if sortBy == Just colSortBy
                                                  then "visible"
                                                  else "hidden"
                                          }
                                      , children: [ icon_ if sort == SortString "desc" then ArrowDown else ArrowUp ]
                                      }
                                  ]
                              }
                          _   , _ ->
                            label
                      ]
                  }

    renderLumiTable self columns renderChildren =
      lumiTableEl
        { children:
            [ if not self.state.showMenu
                then empty
                else guard self.props.dropdownMenu $ renderFilterDropdown
                  { close: closeMenu self
                  , reorderItems: setColumnSort self <<< map \{ name, hidden } ->
                      { name: ColumnName name
                      , hidden
                      }
                  , items: columns <#> \{ name, label, filterLabel, hidden } ->
                      { name: un ColumnName name
                      , label
                      , filterLabel
                      , hidden
                      }
                  , style: R.css self.state.menuStyle
                  }
            , selectorRef (QuerySelector "lumi-table-inner") \maybeTableRef ->
                lumiTableInnerEl
                  { children:
                      case maybeTableRef of
                        Nothing       -> []
                        Just tableRef -> renderChildren tableRef
                  , style: self.props.tableInnerStyle
                  }
            ]
        }

    renderFilterDropdown { close, reorderItems, items, style } =
      selectorRef (QuerySelector "lumi-filter-dropdown") \maybeMenuRef ->
        windowEvent
          { eventType: EventType "click"
          , handler: case maybeMenuRef of
              Nothing -> \_ -> pure unit
              Just menuRef -> \e -> do

                isEventTargetInTree <- runEffectFn2 checkIsEventTargetInTree menuRef e
                when (not isRightClick e && not isEventTargetInTree) close
          , options: { capture: false, once: false, passive: false }
          }
          $ filterDropdown
              { items
              , onChange: reorderItems
              , style
              }

    lumiTableEl = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-table")
    lumiTableInnerEl = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-table-inner")
    lumiRowEl = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-row")

type TableRowProps row col_ pcol_ =
  { tableProps ::
      { columns :: Array
          { name :: ColumnName
          , style :: R.CSS
          , required :: Boolean
          , renderCell :: row -> JSX
          , hidden :: Boolean
          , sticky :: Boolean
          | col_
          }
      , primaryColumn :: Maybe
          { name :: ColumnName
          , renderCell :: row -> JSX
          , style :: R.CSS
          , onRowClick :: row -> Maybe (Effect Unit)
          , sticky :: Boolean
          | pcol_
          }
      , selectable :: Boolean
      , getRowKey :: row -> String
      , rowEq :: row -> row -> Boolean
      , onSelect :: { shift :: Boolean, key :: String, checked :: Boolean } -> Effect Unit
      }
  , row :: row
  , isSelected :: Boolean
  }

tableRowComponent :: forall row col_ pcol_. Component (TableRowProps row col_ pcol_)
tableRowComponent = createComponent "TableRow"

tableRow :: forall row col_ pcol_. TableRowProps row col_ pcol_ -> JSX
tableRow = make tableRowComponent { initialState: unit, shouldUpdate, render }
  where
    shouldUpdate { props } { nextProps } =
      props.isSelected /= nextProps.isSelected ||
      not props.tableProps.rowEq props.row nextProps.row ||
      -- the column array is cached in Table state
      not unsafeRefEq props.tableProps.columns nextProps.tableProps.columns

    render { props: { tableProps, row, isSelected } } =
      R.tr
        { className: joinWith " "
            $  guard (tableProps.selectable && isSelected) [ "active" ]
            <> guard (isJust $ (\pc -> pc.onRowClick row) =<< tableProps.primaryColumn) [ "active-row" ]
        , onClick:
            fromMaybe (Events.handler_ $ pure unit) do
              { onRowClick } <- tableProps.primaryColumn
              onRowClickAction <- onRowClick row
              pure $ Events.handler syntheticEvent \_ -> do
                s <- hasWindowSelection
                when (not s) $ onRowClickAction
        , children:
            [ if not tableProps.selectable
                then empty
                else
                  renderBodyRowCheckbox
                    { checked: isSelected
                    , onChange: Events.handler (Events.merge { nativeEvent, targetChecked }) \{ nativeEvent, targetChecked } -> do
                        let
                          shift = fromMaybe false (toMaybe (unsafeCoerce nativeEvent).shiftKey)
                          key = tableProps.getRowKey row
                        tableProps.onSelect { shift, key, checked: fromMaybe false targetChecked }
                    }
            ]
            <> maybe [] (pure <<< renderPrimaryCell row) tableProps.primaryColumn
            <> tableProps.columns `flip mapMaybe` \col ->
                if col.hidden
                  then Nothing
                  else Just (renderBodyRowCell row col)
        }

    renderBodyRowCheckbox { checked, onChange } =
      R.td
        { key: "_checkbox"
        , style: R.css { width: "20px" }
        , onClick: Events.handler stopPropagation (const (pure unit))
        , children:
            [ input checkbox
                { style = R.css { position: "static" }
                , checked = if checked then On else Off
                , onChange = onChange
                }
            ]
        }

    renderPrimaryCell row col =
      R.td
        { key: un ColumnName col.name
        , className: joinWith " " $ fold
            [ ["primary-cell"]
            , guard col.sticky ["sticky-column"]
            ]
        , style: col.style
        , _data: fromHomogeneous { required: show true }
        , children:
            [ fromMaybe (col.renderCell row) do
                onRowClickAction <- col.onRowClick row
                pure $ Link.link Link.defaults
                  { navigate = Just onRowClickAction
                  , text = col.renderCell row
                  , className = Just "primary-cell-link"
                  }
            ]
        }

    renderBodyRowCell row col =
      R.td
        { key: un ColumnName col.name
        , style: col.style
        , className: guard col.sticky "sticky-column"
        , _data: fromHomogeneous { required: show col.required }
        , children: [ col.renderCell row ]
        }

betweenSorted :: forall a. Ord a => a -> a -> a -> Boolean
betweenSorted a b = between (min a b) (max a b)

saveColumnState
  :: String
  -> Array { name :: ColumnName, hidden :: Boolean }
  -> Effect Unit
saveColumnState key cols = do
  ls <- localStorage =<< window
  setItem key (writeJSON $ map cleanHiddenFlags cols) ls
  where
    cleanHiddenFlags a = a { hidden = not not a.hidden }

loadColumnState
  :: String
  -> Effect (Maybe (Array { name :: ColumnName, hidden :: Boolean }))
loadColumnState key = do
  ls <- localStorage =<< window
  maybeColsStr <- getItem key ls
  pure do
    colsStr <- maybeColsStr
    either (const Nothing) Just (readJSON colsStr)

foreign import dataSize :: forall a. Array a -> Int

foreign import getMouseEventPositionWithOffset :: EffectFn2 Node SyntheticEvent { x :: Number, y :: Number }

foreign import checkIsEventTargetInTree :: EffectFn2 Node Event Boolean

foreign import isRightClick :: Event -> Boolean

foreign import hasWindowSelection :: Effect Boolean

styles :: JSS
styles = jss
  { "@global":
      { "lumi-table":
          { width: "100%"
          , maxHeight: "100%"
          , display: "flex"
          , flexFlow: "column"
          , position: "relative"
          }

      , "lumi-table-inner":
          { width: "100%"
          , maxHeight: "100%"
          , overflow: "auto"
          , backgroundColor: cssStringHSLA colors.white
          , display: "block"
          , whiteSpace: "nowrap"

          , "& table.lumi":
              { width: "100%"
              , maxHeight: "100%"
              , fontSize: "14px"
              , borderCollapse: "separate"
              , borderSpacing: "0"

              , "& th, & td":
                  { padding: "12px 8px"
                  , "&:first-child": { paddingLeft: "16px" }
                  , "&:last-child": { paddingRight: "16px" }
                  }
              , "&.fixed": { tableLayout: "fixed" }
              , "&.compact":
                  { "& th, & td":
                      { padding: "10px 8px"
                      , "&:first-child": { paddingLeft: "0" }
                      , "&:last-child": { paddingRight: "0" }
                      }
                  }

                -- sticky header
              , "& th":
                  { position: "sticky"
                  , top: "0"
                  , zIndex: ziTableHeader
                  }

                -- sticky columns
              , "@media (min-width: 448px)":
                  { "&.selectable th:first-child, &.selectable td:first-child, & th.sticky-column, & td.sticky-column":
                      { position: "sticky"
                      , left: "0"
                      }
                  , "&.selectable .sticky-column": { left: "44px" }
                  , "&.selectable th:first-child, & th.sticky-column":
                      { zIndex: ziTableLockedColumnHeader
                      }
                  , "&.selectable td:first-child, & td.sticky-column":
                      { zIndex: ziTableLockedColumn
                      }
                  , "& .sticky-column":
                      { transition: "box-shadow 200ms ease-in-out"
                      }
                  , "&.has-scrolled-x .sticky-column":
                      { boxShadow: "4px 0 0px 0px " <>
                          cssStringHSLA (alpha 0.04 colors.black)
                      }
                  }

              , "& tr th":
                  { lineHeight: "inherit"
                  , textAlign: "left"
                  , color: cssStringHSLA colors.black1
                  , borderBottom: [ "1px", "solid", cssStringHSLA colors.black4 ]
                  , backgroundColor: cssStringHSLA colors.white
                  }

              , "& tbody":
                  { "& tr, & td":
                      { "&.active-row":
                          { "&:hover":
                              { cursor: "pointer"
                              , backgroundColor: cssStringHSLA (alpha 0.02 colors.black)
                              }
                          , "&.active":
                              { backgroundColor: cssStringHSLA (alpha 0.05 colors.primary)
                              }
                          }
                      }
                  , "& tr td":
                      { lineHeight: "inherit"
                      , textAlign: "left"
                      , borderBottom: [ "1px", "solid", cssStringHSLA colors.black4 ]
                      , maxWidth: "250px"
                      , overflow: "hidden"
                      , textOverflow: "ellipsis"
                      , backgroundColor: cssStringHSLA colors.white

                      , "&.primary-cell a.lumi.primary-cell-link":
                          { color: cssStringHSLA colors.primary
                          }
                      }
                  }
              , "& a.lumi":
                  { color: cssStringHSLA colors.black
                  , "&:hover": { textDecoration: "underline" }
                  }
              , "& input.lumi[type=\"checkbox\"]:not([data-variant=\"switch\"])":
                  { margin: "0"
                  }
              }

          , "@media (max-width: 860px)":
              { "& table.lumi":
                  { "& [data-required=\"false\"]":
                      { display: "none"
                      }
                  }
              }
          }

      , "lumi-filter-dropdown":
          { boxSizing: "border-box"
          , position: "absolute"
          , zIndex: ziTableHeaderMenu
          , backgroundColor: cssStringHSLA colors.white
          , border: [ "1px", "solid", cssStringHSLA colors.black4 ]
          , borderRadius: "3px"
          , display: "flex"
          , flexFlow: "column nowrap"
          , fontSize: "10px"
          , maxWidth: "300px"
          , minWidth: "200px"
          }
      }
  }
  where
    alpha a = toHSLA >>> \{ h, s, l } -> hsla h s l a
