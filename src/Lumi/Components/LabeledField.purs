module Lumi.Components.LabeledField where

import Prelude

import Color (cssStringHSLA)
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (toNullable)
import Effect.Unsafe (unsafePerformEffect)
import JSS (JSS, jss)
import Lumi.Components.Color (colorNames, colors)
import Lumi.Components.Input (alignToInput)
import Lumi.Components.Input as Input
import Lumi.Components.Text (subtext, text, nbsp)
import React.Basic.Classic (Component, JSX, createComponent, element, empty, makeStateless)
import React.Basic.DOM (CSS, css, unsafeCreateDOMComponent)
import React.Basic.DOM as R

data RequiredField
  = Required
  | Optional
  | Neither

derive instance eqRequiredField :: Eq RequiredField

data ValidationMessage = Error String | Warning String

type LabeledFieldProps =
  { label :: JSX
  , value :: JSX
  , validationError :: Maybe ValidationMessage
  , required :: RequiredField
  , forceTopLabel :: Boolean
  , style :: CSS
  }

defaults :: LabeledFieldProps
defaults =
  { label: R.text "Label"
  , value: Input.input Input.text_
  , validationError: Nothing
  , required: Neither
  , forceTopLabel: false
  , style: css {}
  }

component :: Component LabeledFieldProps
component = createComponent "LabeledField"

labeledField :: LabeledFieldProps -> JSX
labeledField = makeStateless component render
  where
    _label = element (unsafePerformEffect $ unsafeCreateDOMComponent "label")

    _column = element (unsafePerformEffect $ unsafeCreateDOMComponent "lumi-column")

    render props =
      _label
        { className: "lumi labeled-field"
        , "data-force-top-label": props.forceTopLabel
        , style: props.style
        , children:
            [ columnLeft props.label props.required
            , columnRight props.value props.validationError
            ]
        }

    columnLeft label required =
      _column
        { "class": "labeled-field--left"
        , children:
            [ alignToInput
                case required of
                  Neither -> label
                  Optional ->
                    label <> R.text nbsp <>
                      text subtext
                        { children = [ R.text "- optional" ]
                        , color = toNullable (Just colorNames.black1)
                        }
                  Required ->
                    label <> R.text nbsp <>
                      text subtext
                        { children = [ R.text " *" ]
                        , color = toNullable (Just colorNames.accent3)
                        }
            ]
        }

    columnRight value validationError =
      _column
        { "class": "labeled-field--right"
        , children:
            [ value
            , maybe empty renderValidationError validationError
            ]
        }

    renderValidationError =
      case _ of
        Error error ->
          text subtext
            { className = toNullable (Just "labeled-field--validation-error")
            , children = [ R.text error ]
            }
        Warning warn ->
          text subtext
            { className = toNullable (Just "labeled-field--validation-warning")
            , children = [ R.text warn ]
            }

styles :: JSS
styles = jss
  { "@global":
      { "label.lumi.labeled-field":
          { display: "flex"
          , flexFlow: "row nowrap"

          , "& .labeled-field--left":
              { flex: "0 1 30%"
              , whiteSpace: "nowrap"
              }

          , "& .labeled-field--right":
              { flex: "0 1 70%"
              , maxWidth: "70%"
              }

          , "&[data-force-top-label=\"true\"]": labeledFieldTopLabelStyles

          , "@media (max-width: 448px)": labeledFieldTopLabelStyles

          , "& .labeled-field--validation-error": labeledFieldValidationErrorStyles

          , "& .labeled-field--validation-warning": labeledFieldValidationWarningStyles
          }
      }
  }

labeledFieldTopLabelStyles :: JSS
labeledFieldTopLabelStyles = jss
  { flexFlow: "column nowrap"

  , "& .labeled-field--left":
      { flex: "initial"
      , whiteSpace: "normal"
      }

  , "& .labeled-field--right":
      { flex: "initial"
      , maxWidth: "none"
      }
  }

labeledFieldValidationErrorStyles :: JSS
labeledFieldValidationErrorStyles = jss
  { color: cssStringHSLA colors.accent3
  , marginTop: "4px"
  }

labeledFieldValidationWarningStyles :: JSS
labeledFieldValidationWarningStyles = jss
  { color: cssStringHSLA colors.accent2
  , marginTop: "4px"
  }
