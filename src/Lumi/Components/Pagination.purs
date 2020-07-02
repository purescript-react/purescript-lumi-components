module Lumi.Components.Pagination where

import Prelude

import Color (cssStringHSLA)
import Data.Array as Array
import Data.Foldable (fold, foldMap)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import JSS (JSS, jss)
import Lumi.Components.Button as Button
import Lumi.Components.Color (colors)
import React.Basic.Classic (JSX, createComponent, element, fragment, keyed, makeStateless)
import React.Basic.DOM as R
import React.Basic.Events (handler_)

type PaginationProps =
  { previousLabel :: String
  , nextLabel :: String
  , currentPage :: Int
  , onChange :: Int -> Effect Unit
  , pages :: Int
  , focusWindow :: Int
  , marginPages :: Int
  , details :: Maybe JSX
  }

pagination :: PaginationProps -> JSX
pagination = makeStateless (createComponent "Pagination") render
  where
    lumiPagination = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-pagination")
    lumiPaginationDetails = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-pagination-details")

    render props =
      let
        boundedPage = clamp 0 (props.pages - 1) props.currentPage
        rangeMin = max 0 (boundedPage - (props.focusWindow / 2))
        rangeMax = max 0 $ min (props.pages - 1) (boundedPage + (props.focusWindow / 2))

        prevButton =
          keyed "prev" $ R.li_
            [ Button.button Button.secondary
                { title = props.previousLabel
                , onPress = handler_ $ props.onChange (boundedPage - 1)
                }
            ]

        nextButton =
          keyed "next" $ R.li_
            [ Button.button Button.secondary
                { title = props.nextLabel
                , onPress = handler_ $ props.onChange (boundedPage + 1)
                }
            ]

        pageButtons start end =
          fragment $
            Array.range start end <#> \i ->
              keyed (show i) $ R.li_
                [ Button.button Button.secondary
                    { title = show (i + 1)
                    , onPress = handler_ $ props.onChange i
                    , buttonState = if i == props.currentPage then Button.Disabled else Button.Enabled
                    }
                ]

        leftMargin = props.marginPages - 1
        rightMargin = props.pages - props.marginPages

        separator = R.li_ [ R.text "..." ]
      in
        lumiPagination
          { className: "lumi"
          , role: "navigation"
          , "aria-label": "Pagination"
          , children:
              [ R.ul_ $ fold
                  -- Previous page button
                  [ guard (props.currentPage > 0)
                      [ prevButton
                      ]
                  -- Left margin
                  , guard (rangeMin > 0)
                      [ pageButtons 0 (min (rangeMin - 1) leftMargin)
                      ]
                  , guard (rangeMin > leftMargin + 1)
                      [ separator
                      ]
                  -- Middle pages
                  , [ pageButtons rangeMin rangeMax
                    ]
                  -- Right margin
                  , guard (rangeMax < rightMargin - 1)
                      [ separator
                      ]
                  , guard (rangeMax < props.pages - 1)
                      [ pageButtons (max (rangeMax + 1) rightMargin) (props.pages - 1)
                      ]
                  -- Next page button
                  , guard (props.currentPage < props.pages - 1)
                      [ nextButton
                      ]
                  ]
              , props.details # foldMap \details ->
                  lumiPaginationDetails
                    { children: [ details ]
                    }
              ]
          }

defaults :: PaginationProps
defaults =
  { previousLabel: "Prev"
  , nextLabel: "Next"
  , currentPage: 0
  , onChange: mempty
  , pages: 0
  , focusWindow: 2
  , marginPages: 2
  , details: Nothing
  }

styles :: JSS
styles = jss
  { "@global":
      { "lumi-pagination":
          { alignSelf: "stretch"
          , display: "flex"
          , flexFlow: "row wrap"
          , alignItems: "center"
          , justifyContent: "flex-start"

          , padding: "16px 0"

          , "& > ul":
              { padding: "0"
              , margin: "0 20px 0 0"
              , listStyle: "none"

              , display: "flex"
              , flexFlow: "row nowrap"
              , alignItems: "center"

              , "& > li":
                  { margin: "0 4px"
                  , "&:first-child": { marginLeft: "0" }
                  , "&:last-child": { marginRight: "0" }
                  , "& > button.lumi[data-color=\"secondary\"]":
                      { minWidth: "0"
                      , paddingLeft: "10px"
                      , paddingRight: "10px"
                      , fontSize: lumiPaginationFontSize
                      , color: cssStringHSLA colors.black1

                      , "&:disabled":
                          { color: cssStringHSLA colors.black
                          , background: cssStringHSLA colors.black3
                          }
                      }
                  }
              }

          , "& > lumi-pagination-details":
              { color: cssStringHSLA colors.black1
              , fontSize: lumiPaginationFontSize
              }
          }
      }
  }
  where
    lumiPaginationFontSize = "12.5px"
