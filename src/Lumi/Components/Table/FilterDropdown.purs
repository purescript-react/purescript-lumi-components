module Lumi.Components.Table.FilterDropdown where

import Prelude hiding (div)

import Color (cssStringHSLA)
import Control.Alt ((<|>))
import Data.Array (drop, mapWithIndex, take, (!!))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components.Color (colors)
import Lumi.Components.Icon (IconType(Rearrange), icon)
import Lumi.Components.Input (CheckboxState(..), checkbox, input)
import Lumi.Components.Size (small)
import React.Basic.DOM (CSS, css, div, text, unsafeCreateDOMComponent)
import React.Basic.DOM.Events (targetChecked)
import React.Basic.Events as Events
import React.Basic.Hooks (JSX, component, element, keyed, useState', (/\))
import React.Basic.Hooks as React
import React.Basic.ReactDND (DragDrop, DragDropItemType(..), createDragDrop)

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

filterDragDropType :: DragDropItemType
filterDragDropType = DragDropItemType "FILTER_ITEM"

dnd :: DragDrop { name :: String, index :: Int }
dnd = createDragDrop filterDragDropType

newtype DragIndex = DragIndex Int

newtype HoverIndex = HoverIndex Int

type Item =
  { name :: String
  , label :: Nullable String
  , filterLabel :: Nullable String
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
      isDragging /\ setIsDragging <- useState' false
      pure
        $ dnd.dragSource
            { beginDrag: \_ -> pure
                { name: item.name
                , index
                }
            , endDrag: const (pure unit)
            , canDrag: const (pure true)
            , isDragging: \{ item: draggingItem } ->
                pure $ maybe false (\i -> i.name == item.name) draggingItem
            , render: \{ connectDragSource, isDragging } ->
                dnd.dropTarget
                  { drop: handleDrop onDrag index
                  , hover: const (pure unit)
                  , canDrop: const (pure true)
                  , render: \{ connectDropTarget, isOver, item: maybeDragItem } ->
                      connectDragSource $ connectDropTarget $
                        row
                          { className: if item.hidden then "" else "active"
                          , style:
                            let
                              borderStyle :: (Int -> Int -> Boolean) -> String
                              borderStyle compare' =
                                if isOver && maybe false (\dragItem -> dragItem.index `compare'` index) maybeDragItem
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
                  }
            }

    handleDrop onDrag index { item: dragItem } = do
      for_ (_.index <$> dragItem) \dragIndex ->
        onDrag (DragIndex dragIndex) (HoverIndex index)
      pure Nothing

    renderInput onChange items item =
      input checkbox
        { size = small
        , disabled = not maybe false (const true) (toMaybe item.label)
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
        , children: [ text (fromMaybe "" (toMaybe item.filterLabel <|> toMaybe item.label)) ]
        }

    renderDragIcon =
      icon
        { type_: Rearrange
        , style: css { cursor: "move" }
        }
    
    row = element (unsafePerformEffect $ unsafeCreateDOMComponent "lumi-row")
