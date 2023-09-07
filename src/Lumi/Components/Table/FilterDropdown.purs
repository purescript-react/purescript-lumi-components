module Lumi.Components.Table.FilterDropdown where

import Prelude hiding (div)

import Color (cssStringHSLA)
import Control.Alt ((<|>))
import Data.Array (drop, mapWithIndex, take, (!!))
import Data.Foldable (for_)
import Data.Int as Int
import Data.Maybe (Maybe, fromMaybe, maybe)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components.Color (colors)
import Lumi.Components.Icon (IconType(Rearrange), icon)
import Lumi.Components.Input (CheckboxState(..), checkbox, input)
import Lumi.Components.Size (small)
import React.Basic.DOM (CSS, css, div, text, unsafeCreateDOMComponent)
import React.Basic.DOM.Events (targetChecked)
import React.Basic.Events as Events
import React.Basic.Hooks (JSX, component, element, keyed)
import React.Basic.Hooks as React
import React.Basic.ReactDND (mergeTargets, useDrag, useDrop)

type FilterDropdownProps =
  { items :: Array Item
  , onChange :: Array Item -> Effect Unit
  , style :: CSS
  }

filterDropdown :: FilterDropdownProps -> JSX
filterDropdown =
  unsafePerformEffect do
    component "TableFilterDropdown" render
  where
    render props =
      pure
        $ filterDropdownEl
            { style: props.style
            , children:
                props.items # mapWithIndex \index item ->
                  keyed item.name $ filterItem_
                    { index
                    , item
                    , items: props.items
                    , onChange: props.onChange
                    , onDrag
                    }
            }
      where
        onDrag (DragIndex dragIndex) (HoverIndex hoverIndex) = do
          props.onChange (moveItem dragIndex hoverIndex props.items)

    filterDropdownEl = element (unsafePerformEffect $ unsafeCreateDOMComponent "lumi-filter-dropdown")

moveItem :: forall a. Int -> Int -> Array a -> Array a
moveItem fromIndex toIndex items =
  let
    item = items !! fromIndex
    items' = take fromIndex items <> drop (fromIndex + 1) items
  in
    take toIndex items'
      <> maybe [] pure item
      <> drop toIndex items'

filterDragDropType :: String
filterDragDropType = "FILTER_ITEM"

newtype DragIndex = DragIndex Int

newtype HoverIndex = HoverIndex Int

type Item =
  { name :: String
  , label :: Maybe JSX
  , filterLabel :: Maybe JSX
  , hidden :: Boolean
  }

type FilterItemProps =
  { item :: Item
  , index :: Int
  , items :: Array Item
  , onChange :: Array Item -> Effect Unit
  , onDrag :: DragIndex -> HoverIndex -> Effect Unit
  }

filterItem_ :: FilterItemProps -> JSX
filterItem_ =
  unsafePerformEffect do
    component "FilterItem" render
  where
    render { onChange, onDrag, items, item, index } = React.do
      { isDragging, connectDrag } <- useDrag { type: filterDragDropType, id: show index }
      { id: maybeDragItem, isOver, connectDrop } <-
        useDrop
          { accept: filterDragDropType
          , onDrop: Int.fromString >>> handleDrop onDrag index
          }
      pure
        $ row
            { ref: mergeTargets connectDrag connectDrop
            , className: if item.hidden then "" else "active"
            , style:
              let
                borderStyle :: (Int -> Int -> Boolean) -> String
                borderStyle compare' =
                  if isOver && maybe false (_ `compare'` index) (Int.fromString =<< maybeDragItem)
                  then "2px solid " <> cssStringHSLA colors.primary
                  else "2px solid " <> cssStringHSLA colors.transparent
                borderTop = borderStyle (>)
                borderBottom = borderStyle (<)
              in
                css
                  { padding: "0 8px"
                  , alignItems: "center"
                  , borderTop
                  , borderBottom
                  , opacity: if isDragging then 0.1 else 1.0
                  }
            , children:
                [ renderInput onChange items item
                , renderLabel item
                , renderDragIcon
                ]
            }

    handleDrop onDrag index id = do
      for_ id \dragIndex ->
        onDrag (DragIndex dragIndex) (HoverIndex index)

    renderInput onChange items item =
      input checkbox
        { size = small
        , disabled = not maybe false (const true) item.label
        , checked = if item.hidden then Off else On
        , onChange =
            Events.handler targetChecked
              (fromMaybe false >>> handleCheckboxChange onChange items item)
        }

    handleCheckboxChange onChange items item checked = do
      onChange $ items <#> \item_ ->
        if item.name == item_.name
        then item_ { hidden = not checked }
        else item_

    renderLabel item =
      div
        { style: css
            { paddingLeft: "8px"
            , paddingRight: "8px"
            , fontSize: "12px"
            , flex: 1
            , whiteSpace: "nowrap"
            , overflow: "hidden"
            , textOverflow: "ellipsis"
            }
        , children: [ fromMaybe (text "") (item.filterLabel <|> item.label) ]
        }

    renderDragIcon =
      icon
        { type_: Rearrange
        , style: css { cursor: "move" }
        }

    row = element (unsafePerformEffect $ unsafeCreateDOMComponent "lumi-row")
