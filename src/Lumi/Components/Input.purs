module Lumi.Components.Input
  ( CheckboxState(..)
  , checkOn
  , checkOff
  , checkIndeterminate
  , InputVariant(..)
  , variantNone
  , variantSwitch
  , InputStep(..)
  , InputProps(..)
  , input
  , text_
  , username
  , email
  , password
  , newPassword
  , ccName
  , ccNumber
  , ccExpMonth
  , ccExpYear
  , ccCode
  , telephone
  , search
  , hidden
  , date
  , number
  , radio
  , checkbox
  , color
  , switch
  , range
  , label
  , label_
  , inputRow
  , inputRow_
  , alignToInput
  , styles
  , lumiInputStyles
  , lumiInputPlaceholderStyles
  , lumiInputHoverStyles
  , lumiInputFocusStyles
  , lumiInputInvalidStyles
  , lumiInputFocusInvalidStyles
  , lumiInputDisabledStyles
  ) where

import Prelude

import Color (cssStringHSLA)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, notNull, toMaybe, toNullable)
import Effect.Uncurried (mkEffectFn1)
import Effect.Unsafe (unsafePerformEffect)
import JSS (JSS, important, jss)
import Lumi.Components.Color (colors)
import Lumi.Components.Size (Size(..))
import Lumi.Components.Text (body_)
import React.Basic.Classic (Component, JSX, ReactComponent, createComponent, element, makeStateless)
import React.Basic.DOM (CSS, css)
import React.Basic.DOM as R
import React.Basic.Events (EventHandler)

data CheckboxState = On | Off | Indeterminate

derive instance eqCheckboxState :: Eq CheckboxState

checkOn :: CheckboxState
checkOn = On

checkOff :: CheckboxState
checkOff = Off

checkIndeterminate :: CheckboxState
checkIndeterminate = Indeterminate

data InputVariant = None | Switch

variantNone :: InputVariant
variantNone = None

variantSwitch :: InputVariant
variantSwitch = Switch

instance showInputVariant :: Show InputVariant where
  show None = ""
  show Switch = "switch"

data InputStep = Any | Step Number

displayStep :: InputStep -> String
displayStep Any = "any"
displayStep (Step e) = show e

type InputProps =
  { "type" :: String
  , autoComplete :: String
  , autoFocus :: Boolean
  , checked :: CheckboxState
  , disabled :: Boolean
  , max :: Nullable Number
  , min :: Nullable Number
  , maxLength :: Nullable Int
  , minLength :: Nullable Int
  , step :: Nullable InputStep
  , name :: String
  , onBlur :: Nullable EventHandler
  , onChange :: EventHandler
  , onFocus :: Nullable EventHandler
  , onKeyUp :: Nullable EventHandler
  , pattern :: Nullable String
  , placeholder :: String
  , readOnly :: Boolean
  , required :: Boolean
  , size :: Size
  , style :: CSS
  , testId :: Nullable String
  , value :: String
  , variant :: InputVariant
  }

component :: Component InputProps
component = createComponent "Input"

input :: InputProps -> JSX
input = makeStateless component $ element lumiInputElement <<< mapProps
  where
    mapProps props =
      { "data-size": show props.size
      , "data-testid": props.testId
      , "data-variant": show props.variant
      , "type": props."type"
      , autoComplete: props.autoComplete
      , autoFocus: props.autoFocus
      , checked: props.checked == On
      , className: "lumi"
      , disabled: props.disabled
      , indeterminate: props.checked == Indeterminate
      , max: props.max
      , min: props.min
      , maxLength: props.maxLength
      , minLength: props.minLength
      , step: toNullable $ map displayStep $ toMaybe props.step
      , name: props.name
      , onBlur: props.onBlur
      , onChange: props.onChange
      , onFocus: props.onFocus
      , onKeyUp: props.onKeyUp
      , pattern: props.pattern
      , placeholder: props.placeholder
      , required: props.required
      , readOnly: props.readOnly
      , style: props.style
      , value: props.value
      }

text_ :: InputProps
text_ =
  { "type": "text"
  , autoComplete: "on"
  , autoFocus: false
  , checked: Off
  , disabled: false
  , max: toNullable Nothing
  , min: toNullable Nothing
  , maxLength: toNullable Nothing
  , minLength: toNullable Nothing
  , step: toNullable Nothing
  , name: ""
  , onBlur: toNullable Nothing
  , onChange: mkEffectFn1 $ pure <<< const unit
  , onFocus: toNullable Nothing
  , onKeyUp: toNullable Nothing
  , pattern: toNullable Nothing
  , placeholder: ""
  , required: false
  , readOnly: false
  , size: Medium
  , style: css {}
  , testId: toNullable Nothing
  , value: ""
  , variant: None
  }

username :: InputProps
username = text_
  { autoComplete = "username"
  }

email :: InputProps
email = text_
  { "type" = "email"
  , autoComplete = "email"
  }

password :: InputProps
password = text_
  { "type" = "password"
  , autoComplete = "current-password"
  }

newPassword :: InputProps
newPassword = password
  { autoComplete = "new-password"
  }

ccName :: InputProps
ccName = text_
  { autoComplete = "cc-name"
  }

ccNumber :: InputProps
ccNumber = text_
  { autoComplete = "cc-number"
  }

ccExpMonth :: InputProps
ccExpMonth = text_
  { autoComplete = "cc-exp-month"
  }

ccExpYear :: InputProps
ccExpYear = text_
  { autoComplete = "cc-exp-year"
  }

ccCode :: InputProps
ccCode = text_
  { autoComplete = "cc-csc"
  }

telephone :: InputProps
telephone = text_
  { "type" = "tel"
  , autoComplete = "tel"
  }

search :: InputProps
search = text_
  { "type" = "search"
  }

hidden :: InputProps
hidden = text_
  { "type" = "hidden"
  }

date :: InputProps
date = text_
  { "type" = "date"
  }

number :: InputProps
number = text_
  { "type" = "number"
  , step = notNull Any
  }

radio :: InputProps
radio = text_
  { "type" = "radio"
  }

checkbox :: InputProps
checkbox = text_
  { "type" = "checkbox"
  }

switch :: InputProps
switch = text_
  { "type" = "checkbox"
  , variant = Switch
  }

range :: InputProps
range = text_
  { "type" = "range"
  }

color :: InputProps
color = text_
  { "type" = "color"
  }

label :: { children :: Array JSX, for :: Nullable String, style :: CSS } -> JSX
label = lumiLabelElement <<< mapProps
  where
    lumiLabelElement = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "label")
    mapProps { children, for, style } =
      { className: "lumi"
      , children
      , htmlFor: for
      , style
      }

label_ :: Array JSX -> JSX
label_ children = label
  { children
  , for: toNullable Nothing
  , style: R.css {}
  }

inputRow :: { labelText :: String, leftAligned :: Boolean, style :: CSS } -> InputProps -> JSX
inputRow props opts = label
  { style:
      R.mergeStyles
        [ props.style
        , R.css { flexDirection: "row" }
        ]
  , for: toNullable Nothing
  , children:
      [ input opts
          { style =
              R.mergeStyles
                [ opts.style
                , css { marginRight: 8, marginLeft: if props.leftAligned then 0 else 8 }
                ]
          }
      , alignToInput $ body_ props.labelText
      ]
  }

inputRow_ :: String -> InputProps -> JSX
inputRow_ labelText opts =
  inputRow { labelText: labelText, leftAligned: false, style: R.css { flexDirection: "row" } } opts

alignToInputComponent :: Component JSX
alignToInputComponent = createComponent "AlignToInput"

alignToInput :: JSX -> JSX
alignToInput = makeStateless alignToInputComponent render
  where
    lumiAlignToInputElement = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-align-to-input")
    render child = lumiAlignToInputElement { children: child }

-- | Sneaky FFI
foreign import lumiInputElement :: forall props. ReactComponent props

styles :: JSS
styles = jss
  { "@global":
      { "input.lumi":
          { appearance: "none"

          , "&:not([type=\"range\"])":
              { extend: lumiInputStyles
              , "&:hover": lumiInputHoverStyles
              , "&:invalid": lumiInputInvalidStyles
              , "&:focus":
                  { extend: lumiInputFocusStyles
                  , "&:invalid": lumiInputFocusInvalidStyles
                  }
              , "&:disabled": lumiInputDisabledStyles

              -- Warning -- don't group browser-specific selectors
              , "&::-webkit-input-placeholder": lumiInputPlaceholderStyles
              , "&::-moz-placeholder": lumiInputPlaceholderStyles
              , "&:-ms-input-placeholder": lumiInputPlaceholderStyles
              , "&::placeholder": lumiInputPlaceholderStyles
              }

          , "&[type=\"checkbox\"]:not([data-variant=\"switch\"])":
              { margin: [ inputPaddingYMobile, "0" ]
              , height: important "20px"
              , width: "20px"
              , padding: important "0"
              , alignSelf: "center"
              , borderRadius: "2px"
              , display: "flex"
              , justifyContent: "center"
              , alignItems: "center"
              , flexShrink: "0"
              , color: cssStringHSLA colors.white
              , backgroundColor: cssStringHSLA colors.white
              , cursor: "pointer"

              , "&:checked, &:indeterminate":
                  { backgroundColor: cssStringHSLA colors.primary
                  , border: [ "1px", "solid", cssStringHSLA colors.primary ]
                  , backgroundPosition: "center"
                  , backgroundSize: "80%"
                  , backgroundRepeat: "no-repeat"
                  , fontSize: "12px"
                  , "&::before":
                      { fontFamily: "lumi-font-icons"
                      , lineHeight: "1"
                      }
                  }
              , "&::before":
                  { content: "'•'"
                  , visibility: "hidden"
                  }
              , "&:checked":
                  { "&::before":
                      { content: "'✓'"
                      , visibility: "visible"
                      }
                  }
              , "&:indeterminate":
                  { "&::before":
                      { content: "'⎻'"
                      , visibility: "visible"
                      }
                  }
              , "&:disabled":
                  { opacity: "0.25"
                  }

              , "@media (min-width: 860px)":
                  { margin: [ inputPaddingYDesktop, "0" ]
                  , "&[data-size=\"small\"]":
                      { height: important "14px"
                      , width: "14px"

                      , "&:checked":
                          { fontSize: "9px"
                          }
                      , "&:indeterminate":
                          { fontSize: "9px"
                          }
                      }
                  }
              }

          , "&[type=\"radio\"]":
              { appearance: "none"
              , height: important "20px"
              , width: "20px"
              , padding: important "0"
              , alignSelf: "center"
              , borderRadius: "20px"
              , display: "flex"
              , justifyContent: "center"
              , alignItems: "center"
              , margin: [ inputPaddingYMobile, "0" ]
              , color: cssStringHSLA colors.white
              , backgroundColor: cssStringHSLA colors.white
              , cursor: "pointer"

              , "&:checked":
                  { border: [ "1px", "solid", cssStringHSLA colors.primary ]
                  }
              , "&:checked::after":
                  { content: "' '"
                  , backgroundColor: cssStringHSLA colors.primary
                  , height: "12px"
                  , width: "12px"
                  , borderRadius: "12px"
                  }

              , "@media (min-width: 860px)":
                  { margin: [ inputPaddingYDesktop, "0" ]
                  , "&[data-size=\"small\"]":
                      { height: important "16px"
                      , width: "16px"
                      , "&:checked::after":
                          { height: "8px"
                          , width: "8px"
                          , borderRadius: "8px"
                          }
                      }
                  }
              }

          , "&[type=\"checkbox\"][data-variant=\"switch\"]":
              { appearance: "none"
              , display: "flex"
              , flexFlow: "row"
              , justifyContent: "space-between"
              , alignItems: "center"
              , boxSizing: "border-box"
              , border: [ "2px", "solid", cssStringHSLA colors.black2 ]
              , backgroundColor: cssStringHSLA colors.black2
              , borderRadius: "16px"
              , height: important "16px"
              , width: "26px"
              , padding: important "0"
              , margin: "13px 0 11px 26px"
              , "@media (min-width: 860px)":
                  { margin: "9px 0 7px 26px"
                  }
              , cursor: "pointer"
              , color: cssStringHSLA colors.black2
              , position: "relative"
              , transitionProperty: "color, background-color, border, border-color, box-shadow"
              , transitionDuration: "0.15s"
              , transitionTimingFunction: "ease-in-out"

              , "&::before":
                  { content: "'Off'"
                  , position: "absolute"
                  , left: "-29px"
                  , top: "-3px"
                  }
              , "&::after":
                  { content: "' '"
                  , display: "flex"
                  , boxSizing: "border-box"
                  , height: "10px"
                  , width: "10px"
                  , borderRadius: "10px"
                  , backgroundColor: cssStringHSLA colors.white
                  , transform: "translateX(1px)"
                  , transition: "transform 80ms ease-in-out"
                  }
              , "&:disabled":
                  { opacity: "0.25"
                  }

              , "&:checked":
                  { color: cssStringHSLA colors.primary
                  , border: [ "2px", "solid", cssStringHSLA colors.primary ]
                  , backgroundColor: cssStringHSLA colors.primary

                  , "&::before":
                      { content: "'On'"
                      }
                  , "&::after":
                      { transform: "translateX(11px)"
                      }
                  }

              , "&[data-size=\"large\"]":
                  { borderRadius: "22px"
                  , height: important "22px"
                  , width: "35px"
                  , margin: "10px 0 8px 26px"
                  , "@media (min-width: 860px)":
                      { margin: "6px 0 4px 26px"
                      }

                  , "&::before":
                      { top: "-2px"
                      }

                  , "&::after":
                      { height: "13px"
                      , width: "13px"
                      , borderRadius: "13px"
                      , transform: "translateX(2px)"
                      }

                  , "&:checked":
                      { "&::after":
                          { transform: "translateX(16px)"
                          }
                      }
                  }
              }

            , "&[type=\"range\"]":
                { appearance: "none"
                , cursor: "pointer"
                , backgroundColor: "transparent"
                , border: "none"
                , height: "40px"
                , "@media (min-width: 860px)":
                    { height: "32px"
                    }

                -- Warning -- don't group browser-specific selectors
                , "&::-webkit-slider-runnable-track":
                    { background: cssStringHSLA colors.black4
                    , height: "4px"
                    }
                , "&::-moz-range-track":
                    { background: cssStringHSLA colors.black4
                    , height: "4px"
                    }
                , "&::-ms-track":
                    { height: "4px"
                    }

                  -- Warning -- don't group browser-specific selectors
                  , "&::-webkit-slider-thumb":
                      { appearance: "none"
                      , width: "16px"
                      , height: "16px"
                      , borderRadius: "8px"
                      , background: cssStringHSLA colors.primary
                      , border: "none"
                      , marginTop: "-6px"
                      }
                  , "&::-moz-range-thumb":
                      { appearance: "none"
                      , width: "16px"
                      , height: "16px"
                      , borderRadius: "8px"
                      , background: cssStringHSLA colors.primary
                      , border: "none"
                      , marginTop: "-6px"
                      }
                  , "&::-ms-thumb":
                      { height: "16px"
                      , width: "16px"
                      , borderRadius: "8px"
                      , background: cssStringHSLA colors.primary
                      , border: "none"
                      , marginTop: "2px"
                      }

                  -- Warning -- don't group browser-specific selectors
                  , "&::-moz-range-progress":
                      { background: cssStringHSLA colors.primary
                      }
                  , "&::-ms-fill-lower":
                      { background: cssStringHSLA colors.primary
                      }
                  , "&::-ms-fill-upper":
                      { backgroundColor: cssStringHSLA colors.black4
                      }

                  -- Warning -- don't group browser-specific selectors
                  , "&::-ms-ticks-after":
                      { display: "none"
                      }
                  , "&::-ms-ticks-before":
                      { display: "none"
                      }
                  , "&::-ms-tooltip":
                      { display: "none"
                      }

                  -- Warning -- don't group browser-specific selectors
                  , "&::-moz-focus-outer":
                      { border: "0"
                      }
                  , "&:focus":
                      { outline: "none"

                      -- Warning -- don't group browser-specific selectors
                      , "&::-webkit-slider-thumb": lumiInputFocusStyles
                      , "&::-moz-range-thumb": lumiInputFocusStyles
                      , "&::-ms-thumb": lumiInputFocusStyles
                      }
                  }
          }

      , "label.lumi":
          { boxSizing: "border-box"

          , fontSize: "14px"
          , lineHeight: "20px"
          , touchAction: "manipulation"
          , display: "flex"
          , flexFlow: "column"
          , cursor: "pointer"
          , userSelect: "none"
          }

      , "lumi-align-to-input":
          { boxSizing: "border-box"

          , display: "flex"
          , flexFlow: "row"
          , alignItems: "center"
          , flexShrink: "1"
          , fontSize: "14px"
          , lineHeight: "20px"
          , padding: inputAlignToPaddingMobile
          , "@media (min-width: 860px)":
              { padding: inputAlignToPaddingDesktop
              }
          }

      }
  }
  where
    inputAlignToPaddingMobile = "10px 12px 10px 0" -- vertical-only; extra padding for input borders
    inputAlignToPaddingDesktop = "6px 12px 6px 0"

inputPaddingYMobile :: String
inputPaddingYMobile = "9px"

inputPaddingYDesktop :: String
inputPaddingYDesktop = "5px"

lumiInputStyles :: JSS
lumiInputStyles = jss
  { boxSizing: "border-box"
  , width: "100%"
  , margin: "0"
  , boxShadow: "0 0 0 0 " <> cssStringHSLA colors.primary3
  , backgroundColor: cssStringHSLA colors.white
  , borderRadius: "3px"
  , border: [ "1px", "solid", cssStringHSLA colors.black3 ]
  , color: cssStringHSLA colors.black
  , fontSize: "14px"
  , lineHeight: "20px"
  , fontFamily: "inherit"
  , height: "40px"
  , padding: [ inputPaddingYMobile, "10px" ]
  , "@media (min-width: 860px)":
      { height: "32px"
      , padding: [ inputPaddingYDesktop, "10px" ]
      }
  , outline: "none"
  , touchAction: "manipulation"
  }

lumiInputPlaceholderStyles :: JSS
lumiInputPlaceholderStyles = jss
  { color: cssStringHSLA colors.black2
  }

lumiInputHoverStyles :: JSS
lumiInputHoverStyles = jss
  { borderColor: cssStringHSLA colors.black2
  }

lumiInputInvalidStyles :: JSS
lumiInputInvalidStyles = jss
  { borderColor: cssStringHSLA colors.accent3
  , boxShadow: "none"
  , outline: "none"
  }

lumiInputFocusStyles :: JSS
lumiInputFocusStyles = jss
  { boxShadow: "0 0 0 3px " <> cssStringHSLA colors.primary3
  , borderColor: cssStringHSLA colors.primary1
  , outline: "none"
  }

lumiInputFocusInvalidStyles :: JSS
lumiInputFocusInvalidStyles = jss
  { boxShadow: "0 0 0 3px " <> cssStringHSLA colors.accent33
  , borderColor: cssStringHSLA colors.accent3
  }

lumiInputDisabledStyles :: JSS
lumiInputDisabledStyles = jss
  { color: cssStringHSLA colors.black1
  , backgroundColor: cssStringHSLA colors.black5
  , border: [ "1px", "solid", cssStringHSLA colors.black3 ]
  }
