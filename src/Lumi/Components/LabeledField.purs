module Lumi.Components.LabeledField where

import Prelude

import Color (cssStringHSLA)
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (toNullable)
import JSS (JSS, jss)
import Lumi.Components.Color (colorNames, colors)
import Lumi.Components.Input (alignToInput)
import Lumi.Components.Input as Input
import Lumi.Components.Text (subtext, text, nbsp)
import React.Basic (Component, JSX, createComponent, element, empty, makeStateless)
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
    render props =
      element (unsafeCreateDOMComponent "label")
        { className: "lumi labeled-field"
        , "data-force-top-label": props.forceTopLabel
        , style: props.style
        , children:
            [ columnLeft props.label props.required
            , columnRight props.value props.validationError
            ]
        }

    columnLeft label required =
      element (unsafeCreateDOMComponent "lumi-column")
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
      element (unsafeCreateDOMComponent "lumi-column")
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

          , "&[data-force-top-label=\"true\"]":
              { flexFlow: "column nowrap"
              }

          , "@media (max-width: 448px)":
              { flexFlow: "column nowrap"
              }

          , "& .labeled-field--left":
              { flex: "3 5 0%"
              , whiteSpace: "nowrap"
              }

          , "& .labeled-field--right":
              { flex: "7 7 0%"
              }

          , "& .labeled-field--validation-error": labeledFieldValidationErrorStyles

          , "& .labeled-field--validation-warning": labeledFieldValidationWarningStyles
          }
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
