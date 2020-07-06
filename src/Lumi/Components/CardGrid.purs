module Lumi.Components.CardGrid
  ( cardGrid
  , styles
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Monoid (guard)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import JSS (JSS, jss)
import Lumi.Components.Card (card, cardMinWidth)
import Lumi.Components.Input (CheckboxState(..), checkbox, input)
import Lumi.Components.Size (Size(..))
import React.Basic.Classic (JSX, createComponent, element, keyed, make, readProps)
import React.Basic.DOM as R
import React.Basic.DOM.Events (stopPropagation, targetChecked)
import React.Basic.Events (handler)
import Unsafe.Reference (unsafeRefEq)
import Web.HTML.History (URL)

cardGrid
  :: { items :: Array
        { key :: String
        , title :: String
        , subtitle :: String
        , children :: Array JSX
        , href :: Maybe URL
        }
     , onNavigate :: URL -> Effect Unit
     , selection :: Maybe
        { selectedKeys :: Array String
        , onSelect :: Array String -> Effect Unit
        }
     }
  -> JSX
cardGrid =
  make (createComponent "CardGrid")
    { initialState: unit
    , shouldUpdate
    , render
    }
  where
    shouldUpdate { props } { nextProps } =
      not (unsafeRefEq props.items nextProps.items) ||
      fromMaybe true ado
        { selectedKeys } <- props.selection
        { selectedKeys: nextSelectedKeys } <- nextProps.selection
        in not (unsafeRefEq selectedKeys nextSelectedKeys)

    lumiCardGrid = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-card-grid")

    render self@{ props } =
      lumiCardGrid
        { className: "lumi"
        , children: props.items <#> \{ key, title, subtitle, children, href } ->
            case props.selection of
              Nothing ->
                keyed key $ cardGridCell
                  { title
                  , subtitle
                  , children
                  , href
                  , onNavigate: props.onNavigate
                  , selectable: false
                  , selected: false
                  , onSelect: mempty
                  }
              Just { selectedKeys, onSelect } ->
                keyed key $ cardGridCell
                  { title
                  , subtitle
                  , children
                  , href
                  , onNavigate: props.onNavigate
                  , selectable: isJust props.selection
                  , selected: Array.elem key selectedKeys
                  , onSelect: \selected -> do
                      props' <- readProps self
                      fromMaybe mempty do
                        s <- props'.selection
                        pure $
                          if selected
                            then s.onSelect (Array.snoc s.selectedKeys key)
                            else s.onSelect (Array.filter (_ /= key) s.selectedKeys)
                  }
        }

cardGridCell
  :: { title :: String
     , subtitle :: String
     , children :: Array JSX
     , href :: Maybe URL
     , onNavigate :: URL -> Effect Unit
     , selectable :: Boolean
     , selected :: Boolean
     , onSelect :: Boolean -> Effect Unit
     }
  -> JSX
cardGridCell =
  make (createComponent "CardGridCell")
    { initialState: unit
    , shouldUpdate
    , render
    }
  where
    shouldUpdate { props } { nextProps } =
      props.selected /= nextProps.selected

    lumiCardGridCell = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-card-grid-cell")
    lumiCardGridCheckbox = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-card-grid-checkbox")

    render { props } =
      lumiCardGridCell
        { children:
            [ card
                { title: props.title
                , subtitle: props.subtitle
                , children: props.children
                , href: props.href
                , onNavigate: props.onNavigate
                , selected: props.selectable && props.selected
                }
            , guard (props.selectable) $
                lumiCardGridCheckbox
                  { onClick: handler stopPropagation mempty
                  , "data-selected": props.selected
                  , children:
                      [ input checkbox
                          { style = R.css { margin: "0" }
                          , size = Medium
                          , disabled = false
                          , checked = if props.selected then On else Off
                          , onChange = handler targetChecked $ foldMap props.onSelect
                          }
                      ]
                  }
            ]
        }

styles :: JSS
styles = jss
  { "@global":
      { "lumi-card-grid":
          { display: "grid"
          , gridGap: "20px"
          , gridTemplateColumns: "repeat(auto-fill, minmax(" <> cardMinWidth <> ", 1fr))"

          , "@media (max-width: 448px)":
              { gridGap: "12px"
              , gridTemplateColumns: "repeat(auto-fill, minmax(" <> cardMinWidth <> " - 32px, 1fr))"
              }

          , alignSelf: "stretch"

          , "& > lumi-card-grid-cell":
              { display: "flex"
              , flexFlow: "column nowrap"
              , position: "relative"

              , "& > lumi-card":
                  { flex: "1"
                  , "@media (max-width: 448px)":
                      { minWidth: cardMinWidth <> " - 32px"
                      }
                  }

              , "& > lumi-card-grid-checkbox":
                  { position: "absolute"
                  , top: "16px"
                  , left: "16px"

                  , visibility: "hidden"
                  , "@media (max-width: 860px)":
                      { visibility: "visible"
                      }

                  , "&[data-selected=\"true\"]":
                      { visibility: "visible"
                      }
                  }

              , "&:hover > lumi-card-grid-checkbox":
                  { visibility: "visible"
                  }
              }
          }
      }
  }
