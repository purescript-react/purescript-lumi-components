module Lumi.Components.InputGroup where

import Prelude

import Color (cssStringHSLA)
import Data.Array (fromFoldable)
import Data.Nullable (Nullable, toMaybe)
import Effect.Unsafe (unsafePerformEffect)
import JSS (JSS, jss)
import Lumi.Components.Color (colors)
import Lumi.Components.ZIndex (ziInputGroup)
import React.Basic.Classic (Component, JSX, createComponent, element, makeStateless)
import React.Basic.DOM (CSS, unsafeCreateDOMComponent)
import React.Basic.DOM as R

type InputGroupProps =
  { addOnLeft :: Nullable JSX
  , addOnRight :: Nullable JSX
  , inputContent :: Nullable JSX
  , style :: CSS
  }

component :: Component InputGroupProps
component = createComponent "InputGroup"

inputGroup :: InputGroupProps -> JSX
inputGroup = makeStateless component render
  where
    render props =
      inputGroupElement
        { style: props.style
        , children:
            [ inputGroupAddOn { children: fromFoldable (toMaybe props.addOnLeft) }
            , R.a { className: "input-container", children: fromFoldable (toMaybe props.inputContent) }
            , inputGroupAddOn { children : fromFoldable (toMaybe props.addOnRight) }
            ]
        }

    inputGroupElement = element (unsafePerformEffect $ unsafeCreateDOMComponent "lumi-input-group")
    inputGroupAddOn = element (unsafePerformEffect $ unsafeCreateDOMComponent "input-group-addon")

styles :: JSS
styles = jss
  { "@global":
      { "lumi-input-group":
          { display: "flex"
          , width: "100%"

          , "& a.input-container":
              { flex: "1"
              , marginRight: "-1px"
              , "&:focus, &:hover": { zIndex: ziInputGroup }
              , "& input[type=\"text\"], & input.lumi[type=\"text\"]":
                  { borderRadius: "0"
                  , width: "100%"
                  }
              }

          , "& input-group-addon:first-child button.lumi":
              { marginRight: "-1px"
              , borderTopRightRadius: "0"
              , borderBottomRightRadius: "0"
              , borderTopLeftRadius: buttonBorderRadius
              , borderBottomLeftRadius: buttonBorderRadius
              }
          , "& input:not(:first-child) button.lumi":
              { borderTopLeftRadius: "0"
              , borderBottomLeftRadius: "0"
              }
          , "& input:not(:last-child) button.lumi":
              { marginRight: "-1px"
              , borderTopRightRadius: "0"
              , borderBottomRightRadius: "0"
              }
          , "& input-group-addon:last-child button.lumi":
              { borderTopRightRadius: buttonBorderRadius
              , borderBottomRightRadius: buttonBorderRadius
              , borderTopLeftRadius:"0"
              , borderBottomLeftRadius:"0"
              }

          , "& input-group-addon button.lumi":
                { "&[data-color=\"secondary\"]":
                    { backgroundColor: cssStringHSLA colors.transparent
                    }
                , "&:focus, &:hover": { zIndex: ziInputGroup }
                }
          }
      }
  }
  where
    buttonBorderRadius = "3px"
