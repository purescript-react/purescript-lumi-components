module Lumi.Components.Select
  ( SelectProps
  , SelectProps_Common
  , SelectProps_Single
  , SelectProps_SingleAsync
  , SelectProps_Multi
  , SelectProps_MultiAsync
  , select
  , singleSelect
  , asyncSingleSelect
  , multiSelect
  , asyncMultiSelect
  , module Lumi.Components.Select.Backend
  , styles
  ) where

import Prelude

import Color (cssStringHSLA, hsla, toHSLA)
import Data.Array as Array
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid (guard)
import Data.Nullable (toNullable)
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Unsafe (unsafePerformEffect)
import JSS (JSS, important, jss)
import Lumi.Components (($$$))
import Lumi.Components.Color (colors)
import Lumi.Components.Icon as Icon
import Lumi.Components.Input (lumiInputDisabledStyles, lumiInputFocusStyles, lumiInputHoverStyles, lumiInputStyles)
import Lumi.Components.Loader (loader)
import Lumi.Components.Select.Backend (SelectBackendProps, SelectOption, SelectOptions(..), selectBackend)
import Lumi.Components.ZIndex (ziSelect)
import Lumi.Components2.Text as T
import React.Basic.Classic (Component, JSX, createComponent, element, elementKeyed, empty, makeStateless)
import React.Basic.DOM (CSS, css)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_, currentTarget, targetValue)
import React.Basic.Events as Events
import Web.DOM.Element as Element
import Web.DOM.Node as Node
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML.HTMLElement as HTMLElement

type SelectProps_Common option =
  ( className :: String
  , disabled :: Boolean
  , id :: String
  , loading :: Boolean
  , name :: String
  , noResultsText :: String
  , optionRenderer :: option -> JSX
  , optionSort :: Maybe (String -> option -> option -> Ordering)
  , placeholder :: String
  , searchable :: Boolean
  , style :: CSS
  , toSelectOption :: option -> SelectOption
  )

type SelectProps_Single option =
  { value :: Maybe option
  , options :: Array option
  , onChange :: Maybe option -> Effect Unit
  | SelectProps_Common option
  }

type SelectProps_SingleAsync option =
  { value :: Maybe option
  , loadOptions :: String -> Aff (Array option)
  , onChange :: Maybe option -> Effect Unit
  | SelectProps_Common option
  }

type SelectProps_Multi option =
  { value :: Array option
  , options :: Array option
  , onChange :: Array option -> Effect Unit
  | SelectProps_Common option
  }

type SelectProps_MultiAsync option =
  { value :: Array option
  , loadOptions :: String -> Aff (Array option)
  , onChange :: Array option -> Effect Unit
  | SelectProps_Common option
  }

type SelectProps option =
  { allowMultiple :: Boolean
  , loadOnMount :: Boolean
  , loadOptions :: String -> Aff (Array option)
  , onChange :: Array option -> Effect Unit
  , value :: Array option
  | SelectProps_Common option
  }

singleSelect :: forall option. SelectProps_Single option -> JSX
singleSelect = select <<< mapProps
  where
    mapProps props =
      { value: maybe mempty pure props.value
      , loadOnMount: false
      , loadOptions: \searchTerm ->
          let lowerSearchTerm = String.Pattern $ String.toLower searchTerm
              byOptionLabel =
                String.contains lowerSearchTerm
                  <<< String.toLower
                  <<< _.label
                  <<< props.toSelectOption
          in pure $ Array.filter byOptionLabel props.options
      , onChange: props.onChange <<< Array.head
      , allowMultiple: false
      , className: props.className
      , style: props.style
      , searchable: props.searchable
      , id: props.id
      , name: props.name
      , noResultsText: props.noResultsText
      , placeholder: props.placeholder
      , disabled: props.disabled
      , loading: props.loading
      , optionRenderer: props.optionRenderer
      , optionSort: props.optionSort
      , toSelectOption: props.toSelectOption
      }

asyncSingleSelect :: forall option. SelectProps_SingleAsync option -> JSX
asyncSingleSelect = select <<< mapProps
  where
    mapProps props =
      { value: maybe mempty pure props.value
      , loadOnMount: false
      , loadOptions: props.loadOptions
      , onChange: props.onChange <<< Array.head
      , allowMultiple: false
      , className: props.className
      , style: props.style
      , searchable: props.searchable
      , id: props.id
      , name: props.name
      , noResultsText: props.noResultsText
      , placeholder: props.placeholder
      , disabled: props.disabled
      , loading: props.loading
      , optionRenderer: props.optionRenderer
      , optionSort: props.optionSort
      , toSelectOption: props.toSelectOption
      }

multiSelect :: forall option. SelectProps_Multi option -> JSX
multiSelect = select <<< mapProps
  where
    mapProps props =
      { value: props.value
      , loadOnMount: false
      , loadOptions: \searchTerm ->
          let lowerSearchTerm = String.Pattern $ String.toLower searchTerm
              byOptionLabel =
                String.contains lowerSearchTerm
                  <<< String.toLower
                  <<< _.label
                  <<< props.toSelectOption
          in pure $ Array.filter byOptionLabel props.options
      , onChange: props.onChange
      , allowMultiple: true
      , className: props.className
      , style: props.style
      , searchable: props.searchable
      , id: props.id
      , name: props.name
      , noResultsText: props.noResultsText
      , placeholder: props.placeholder
      , disabled: props.disabled
      , loading: props.loading
      , optionRenderer: props.optionRenderer
      , optionSort: props.optionSort
      , toSelectOption: props.toSelectOption
      }

asyncMultiSelect :: forall option. SelectProps_MultiAsync option -> JSX
asyncMultiSelect = select <<< mapProps
  where
    mapProps props =
      { value: props.value
      , loadOnMount: false
      , loadOptions: props.loadOptions
      , onChange: props.onChange
      , allowMultiple: true
      , className: props.className
      , style: props.style
      , searchable: props.searchable
      , id: props.id
      , name: props.name
      , noResultsText: props.noResultsText
      , placeholder: props.placeholder
      , disabled: props.disabled
      , loading: props.loading
      , optionRenderer: props.optionRenderer
      , optionSort: props.optionSort
      , toSelectOption: props.toSelectOption
      }

component :: forall option. Component (SelectProps option)
component = createComponent "Select"

select :: forall option. SelectProps option -> JSX
select = makeStateless component render
  where
    render props =
      lumiSelectElement
        { "class": props.className
        , "data-disabled": props.disabled
        , "data-multi": props.allowMultiple
        , style: props.style
        , children:
            [ selectBackend
                { isOpen: Nothing
                , loadOnMount: props.loadOnMount
                , loadOptions: props.loadOptions
                , optionSort: props.optionSort
                , toSelectOption: props.toSelectOption
                , allowMultiple: props.allowMultiple
                , disabled: props.disabled
                , onChange: props.onChange
                , value: props.value
                , render: \selectState ->
                    lumiSelectInnerElement
                      { "data-focus": selectState.isActive
                      , children:
                          [ renderInput props selectState
                          , if selectState.isOpen
                              then renderMenu props selectState
                              else empty
                          ]
                      , onClick: Events.handler currentTarget \ct -> do
                          selectState.openSelect
                          traverse_ focusChildInput $ Node.fromEventTarget ct
                      , onFocus: Events.handler_ selectState.openSelect
                      }
                }
            ]
        }

    renderInput props selectState =
      lumiSelectInputElement
        { children:
            [ renderSelectedValues
            , if Array.null props.value
                then empty
                else renderClearButton
            ]
        }
      where
        renderSelectedValues =
          if props.loading
            then loader { style: css { width: "20px", height: "20px", borderWidth: "2px" }, testId: toNullable Nothing }
            else lumiSelectInputSelectedValuesElement
              { children:
                  let renderedSelectedValues =
                        if Array.null props.value && String.null selectState.searchTerm
                          then
                            [ lumiSelectInputPlaceholder
                                { key: "lumi-select-placeholder"
                                , title: props.placeholder
                                , children:
                                    T.text
                                      $ T.truncate
                                      $$$ props.placeholder
                                }
                            ]
                          else
                            props.value <#> \value ->
                              lumiSelectInputSelectedValueElement
                                { key: "lumi-select-selected-value-" <> (props.toSelectOption value).value
                                , children:
                                    [ lumiSelectInputSelectedValueTextElement
                                        { children: [ props.optionRenderer value ]
                                        , title: (props.toSelectOption value).label
                                        }
                                    , lumiSelectClearIconElement
                                        { key: "--createElementKeyed-shouldnt-require-this--"
                                        , children: Icon.icon_ Icon.Remove
                                        , onClick: capture_ do
                                            selectState.removeSelectedOption value
                                        }
                                    ]
                                }
                  in
                    (guard
                      (props.allowMultiple || String.null selectState.searchTerm)
                      renderedSelectedValues)
                      <> (guard props.searchable [ renderNativeInput ])
              }

        renderNativeInput =
          lumiSelectNativeInputElement
            { key: "lumi-select-native-input"
            , className: "select-native-input"
            , id: props.id
            , name: props.name
            , disabled: props.disabled
            , onChange: Events.handler targetValue \mValue -> do
                selectState.setSearchTerm (fromMaybe "" mValue)
                selectState.openSelect
            , onKeyDown: selectState.keydownEventHandler
            , value: selectState.searchTerm
            }

        renderClearButton =
          lumiSelectClearIconElement
            { key: "--createElementKeyed-shouldnt-require-this--"
            , children: Icon.icon_ Icon.Remove
            , onClick: capture_ do
                selectState.removeAllSelectedOptions
            }

    renderMenu props selectState =
      lumiSelectMenuAnchorElement
        { children:
            [ lumiSelectMenuElement
                { children:
                    case selectState.options of
                      Loading ->
                        [ lumiSelectMenuLoaderElement
                            { key: "loading-text"
                            , children:
                                loader { style: css { width: "20px", height: "20px", borderWidth: "2px" }, testId: toNullable Nothing }
                            }
                        ]
                      Failed error ->
                        [ lumiSelectNonOptionElement
                            { key: "error-text"
                            , children: R.text error
                            }
                        ]
                      Ready options_ -> renderOptions options_
                }
            ]
        , onBlur: Events.handler_ selectState.closeSelect
        }
        where
          renderOptions [] =
            [ lumiSelectNonOptionElement
                { key: "no-results-text"
                , children: R.text props.noResultsText
                }
            ]
          renderOptions options_ = options_ # Array.mapWithIndex \index option ->
            let
              { label, value } = props.toSelectOption option
            in
              lumiSelectOptionElement
                { key: "lumi-select-menu-option-" <> value
                , "data-focus": maybe false (_ == index) selectState.focusedIndex
                , children: props.optionRenderer option
                , title: label
                , onClick: capture_ do
                    selectState.addSelectedOption option
                }

    lumiSelectElement =
      element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-select")
    lumiSelectInnerElement =
      element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-select-inner")
    lumiSelectInputElement =
      element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-select-input")
    lumiSelectNativeInputElement =
      elementKeyed (unsafePerformEffect $ R.unsafeCreateDOMComponent "input")
    lumiSelectInputPlaceholder =
      elementKeyed (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-select-input-placeholder")
    lumiSelectInputSelectedValuesElement =
      element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-select-input-selected-values")
    lumiSelectInputSelectedValueElement =
      elementKeyed (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-select-input-selected-value")
    lumiSelectInputSelectedValueTextElement =
      element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-select-input-selected-value-text")
    lumiSelectClearIconElement =
      element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-select-clear-icon")
    lumiSelectMenuAnchorElement =
      element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-select-menu-anchor")
    lumiSelectMenuElement =
      element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-select-menu")
    lumiSelectMenuLoaderElement =
      elementKeyed (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-select-menu-loader")
    lumiSelectOptionElement =
      elementKeyed (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-select-option")
    lumiSelectNonOptionElement =
      elementKeyed (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-select-non-option")

focusChildInput :: Node.Node -> Effect Unit
focusChildInput node = do
  mInput <- case Element.toParentNode <$> Element.fromNode node of
    Nothing         -> pure Nothing
    Just parentNode -> querySelector (QuerySelector "input") parentNode
  traverse_ HTMLElement.focus (HTMLElement.fromElement =<< mInput)

styles :: JSS
styles = jss
  { "@global":
      { "lumi-select":
          { boxSizing: "border-box"
          , margin: "0"
          , display: "inline-flex"
          , flexFlow: "column"
          , width: "100%"
          , cursor: "pointer"

          , "& lumi-select-input":
              { extend: lumiInputStyles
              , display: "flex"
              , flexFlow: "row"
              , flex: "0 0 100%"
              , minHeight: "32px" -- input height (-8px from standard input sizing to account for select wrapper)
              , lineHeight: "32px"
              , "@media (min-width: 860px)":
                  { minHeight: "24px"
                  , lineHeight: "24px"
                  , backgroundPositionY: "12px"
                  }
              , backgroundImage: "url(\"data:image/svg+xml;charset=utf8,%3C?xml version='1.0' encoding='UTF-8'?%3E%3Csvg width='11px' height='5px' viewBox='0 0 11 5' version='1.1' xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink'%3E%3C!-- Generator: Sketch 49.1 (51147) - http://www.bohemiancoding.com/sketch --%3E%3Ctitle%3ESlice 1%3C/title%3E%3Cdesc%3ECreated with Sketch.%3C/desc%3E%3Cdefs%3E%3Cpath d='M5.417,3.519 C5.797,3.187 6.307,2.733 6.912,2.185 L6.974,2.129 C7.70584693,1.46645784 8.43485361,0.800785071 9.161,0.132 C9.29247374,0.0108869595 9.47857352,-0.0308857001 9.64919735,0.0224173781 C9.81982119,0.0757204563 9.94904726,0.216001266 9.98819736,0.390417384 C10.0273475,0.564833501 9.97047374,0.746886966 9.839,0.868 C9.11068658,1.53896706 8.37934578,2.20664054 7.645,2.871 L7.583,2.927 C5.376,4.922 5.287,5 5,5 C4.713,5 4.624,4.922 2.417,2.927 L2.355,2.871 C1.62081041,2.20646869 0.889470368,1.5387959 0.161,0.868 C-0.0422407879,0.68077547 -0.0552245302,0.364240788 0.132,0.161 C0.31922453,-0.0422407879 0.635759212,-0.0552245302 0.839,0.132 C1.5649901,0.800955473 2.29399753,1.46662893 3.026,2.129 L3.088,2.185 C3.693,2.733 4.203,3.187 4.583,3.519 C4.75,3.665 4.89,3.785 5,3.877 C5.11,3.785 5.25,3.665 5.417,3.519 Z' id='path-1'%3E%3C/path%3E%3C/defs%3E%3Cg id='Page-1' stroke='none' stroke-width='1' fill='none' fill-rule='evenodd'%3E%3Cg id='arrow-down'%3E%3Cg id='a-link' fill='%2342413F' fill-rule='nonzero'%3E%3Cpath d='M5.417,3.519 C5.797,3.187 6.307,2.733 6.912,2.185 L6.974,2.129 C7.70584693,1.46645784 8.43485361,0.800785071 9.161,0.132 C9.29247374,0.0108869595 9.47857352,-0.0308857001 9.64919735,0.0224173781 C9.81982119,0.0757204563 9.94904726,0.216001266 9.98819736,0.390417384 C10.0273475,0.564833501 9.97047374,0.746886966 9.839,0.868 C9.11068658,1.53896706 8.37934578,2.20664054 7.645,2.871 L7.583,2.927 C5.376,4.922 5.287,5 5,5 C4.713,5 4.624,4.922 2.417,2.927 L2.355,2.871 C1.62081041,2.20646869 0.889470368,1.5387959 0.161,0.868 C-0.0422407879,0.68077547 -0.0552245302,0.364240788 0.132,0.161 C0.31922453,-0.0422407879 0.635759212,-0.0552245302 0.839,0.132 C1.5649901,0.800955473 2.29399753,1.46662893 3.026,2.129 L3.088,2.185 C3.693,2.733 4.203,3.187 4.583,3.519 C4.75,3.665 4.89,3.785 5,3.877 C5.11,3.785 5.25,3.665 5.417,3.519 Z' id='a'%3E%3C/path%3E%3C/g%3E%3Cg id='Clipped'%3E%3Cmask id='mask-2' fill='white'%3E%3Cuse xlink:href='%23path-1'%3E%3C/use%3E%3C/mask%3E%3Cg id='a'%3E%3C/g%3E%3Cg id='Group' mask='url(%23mask-2)' fill='%23292827' fill-rule='nonzero'%3E%3Cg transform='translate(-5.000000, -8.000000)' id='Shape'%3E%3Cpolygon points='0 0 20 0 20 20 0 20'%3E%3C/polygon%3E%3C/g%3E%3C/g%3E%3C/g%3E%3C/g%3E%3C/g%3E%3C/svg%3E\")"
              , backgroundRepeat: "no-repeat"
              , backgroundPositionY: "16px"
              , backgroundPositionX: "calc(100% - 10px)"
              , backgroundSize: "10px 6px"
              , padding: important "3px 24px 3px 3px"
              }

          , "&:hover lumi-select-input": lumiInputHoverStyles

          , "&:focus-within lumi-select-input, & lumi-select-inner[data-focus=\"true\"] lumi-select-input":
              { extend: lumiInputFocusStyles
              , "& lumi-select-input-placeholder":
                  { display: "none"
                  }
              }

          , "&[data-disabled=\"true\"] lumi-select-input": lumiInputDisabledStyles

          , "& lumi-select-input-selected-values":
              { margin: "-2px" -- (gap)

              , display: "flex"
              , position: "relative"
              , flexFlow: "row wrap"
              , flex: "1 1 auto"
              , minWidth: 0

              , "& > lumi-select-input-selected-value, & > lumi-select-input-placeholder, & > input.select-native-input":
                  { margin: "2px" -- (gap)
                  }
              }

          , "& lumi-select-input-placeholder":
              { color: cssStringHSLA colors.black2
              , lineHeight: "32px"
              , flex: "1 1 0%"
              , display: "block"
              , position: "absolute"
              , width: "100%"
              , maxWidth: "calc(100% - " <> clearIconWidth <> ")"
              , overflow: "hidden"
              , whiteSpace: "nowrap"
              , textOverflow: "ellipsis"
              , "@media (min-width: 860px)":
                  { lineHeight: "24px"
                  }
              , padding: "0 7px"
              }

          , "& lumi-select-input-selected-value":
              { display: "flex"
              , flex: "0 1 auto"
              , flexFlow: "row"
              , alignItems: "center"
              , padding: "0 7px"
              , minWidth: 0
              , lineHeight: "32px"
              , "@media (min-width: 860px)":
                  { lineHeight: "24px"
                  }
              , "& > lumi-select-input-selected-value-text":
                  { display: "block"
                  , lineHeight: "inherit"
                  , overflow: "hidden"
                  , whiteSpace: "nowrap"
                  , textOverflow: "ellipsis"
                  }
              , "& > lumi-select-clear-icon": { display: "none" }
              }
          , "& lumi-select-input-selected-value + input.select-native-input":
              { visibility: "visible"
              , padding: "0 7px 0 1px"
              }

          , "& input.select-native-input":
              { flex: "1 1 0%"
              , minWidth: "40px"
              , appearance: "none"
              , border: "none"
              , font: "inherit"
              , padding: "0 7px"
              , background: "transparent"
              , "&:focus": { outline: "none" }
              }

          , "& lumi-select-clear-icon":
              { display: "flex"
              , alignItems: "center"
              , justifyContent: "center"
              , fontSize: "8px"
              , cursor: "pointer"
              , height: "26px"
              , width: clearIconWidth
              , flex: "0 0 auto"
              , paddingTop: "3px"
              , "@media (min-width: 860px)":
                  { paddingTop: "0"
                  }
              }

          , "&[data-multi=\"true\"]":
              { "& lumi-select-input":
                  { height: important "unset"
                  }

              , "& lumi-select-input-selected-value":
                  { backgroundColor: cssStringHSLA colors.primary3
                  , borderRadius: "3px"
                  , padding: "0 0 0 7px"
                  , fontSize: "13px"
                  , "& > lumi-select-clear-icon":
                      { display: "flex"
                      , color: cssStringHSLA colors.primary
                      , marginLeft: "2px"
                      , height: "20px"
                      , paddingTop: "0"
                      }
                  }

                -- hide the blank input line when not in focus
              , "& lumi-select-inner:not([data-focus=\"true\"]) input.select-native-input[value=\"\"]":
                  { width: 0
                  , minWidth: 0
                  , padding: 0
                  , flex: "0 1 0%"
                  }
              }

          , "&[data-disabled=\"true\"] lumi-select-input-selected-value > lumi-select-clear-icon":
              { color: cssStringHSLA colors.black1
              }

          , "& lumi-select-menu-anchor":
              { display: "block"
              , position: "relative"
              }

          , "& lumi-select-menu":
              { boxSizing: "border-box"
              , margin: "0"

              , position: "absolute"
              , top: "4px"
              , zIndex: ziSelect

              , display: "flex"
              , flexFlow: "column"
              , minWidth: "100%"
              , maxWidth: "100%"
              , padding: "0"
              , borderColor: cssStringHSLA colors.black4
              , backgroundColor: cssStringHSLA colors.white
              , boxShadow: "0 0 20px rgba(0, 0, 0, 0.2)"
              , fontSize: "14px"
              , lineHeight: "20px"
              , minHeight: "32px"
              , maxHeight: "300px"
              , overflowY: "auto"
              }

          , "& lumi-select-menu-loader":
              { boxSizing: "border-box"
              , margin: "0"

              , position: "absolute"

              , display: "flex"
              , flexFlow: "row"
              , justifyContent: "center"
              , alignItems: "center"
              , minWidth: "100%"
              , maxWidth: "100%"
              , height: "100%"
              , overflow: "hidden"
              , backgroundColor:
                  colors.white
                    # toHSLA
                    # _ { a = 0.7 }
                    # \{ h, s, l, a } -> hsla h s l a
                    # cssStringHSLA
              }

          , "& lumi-select-non-option": lumiSelectNonOption

          , "& lumi-select-option":
              { extend: lumiSelectNonOption
              , cursor: "pointer"
              , "&:hover":
                  { backgroundColor: cssStringHSLA colors.primary4
                  }
              , "&:focus, &[data-focus=\"true\"]":
                  { backgroundColor: cssStringHSLA colors.primary3
                  }
              }
          }
      }
  }
  where
    lumiSelectNonOption =
      { boxSizing: "border-box"
      , margin: "0"

      , display: "flex"
      , flexFlow: "column"
      , justifyContent: "center"
      , padding: "6px 9px"
      , flex: "none"
      , minHeight: "32px"
      , cursor: "initial"
      , overflow: "hidden"
      , textOverflow: "ellipsis"
      }

    clearIconWidth = "20px"
