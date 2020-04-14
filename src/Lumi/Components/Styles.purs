module Lumi.Components.Styles where

import Prelude

import Color (cssStringHSLA)
import Data.Traversable (traverse_)
import Effect (Effect)
import JSS (JSS, jss)
import JSS as JSS
import Lumi.Components.Badge as Badge
import Lumi.Components.Border as Border
import Lumi.Components.Breadcrumb as Breadcrumb
import Lumi.Components.Button (styles) as Button
import Lumi.Components.ButtonGroup as ButtonGroup
import Lumi.Components.Card as Card
import Lumi.Components.CardGrid as CardGrid
import Lumi.Components.Color (colors)
import Lumi.Components.Column as Column
import Lumi.Components.Details as Details
import Lumi.Components.Divider as Divider
import Lumi.Components.DropdownButton as DropdownButton
import Lumi.Components.EditableTable as EditableTable
import Lumi.Components.Form as Form
import Lumi.Components.Icon as Icon
import Lumi.Components.Images as Images
import Lumi.Components.Input as Input
import Lumi.Components.InputGroup as InputGroup
import Lumi.Components.LabeledField as LabeledField
import Lumi.Components.Layouts as Layouts
import Lumi.Components.Link as Link
import Lumi.Components.List as List
import Lumi.Components.Loader as Loader
import Lumi.Components.Lockup as Lockup
import Lumi.Components.Modal as Modal
import Lumi.Components.NativeSelect as NativeSelect
import Lumi.Components.Navigation as Navigation
import Lumi.Components.Pagination as Pagination
import Lumi.Components.Pill as Pill
import Lumi.Components.Row as Row
import Lumi.Components.Select as Select
import Lumi.Components.Slider as Slider
import Lumi.Components.StatusSlat as StatusSlat
import Lumi.Components.Tab as Tab
import Lumi.Components.Table as Table
import Lumi.Components.Text as Text
import Lumi.Components.Textarea as Textarea
import Lumi.Components.Toast as Toast
import Lumi.Components.Tooltip as Tooltip
import Lumi.Components.Upload as Upload

attachGlobalComponentStyles :: Effect Unit
attachGlobalComponentStyles = do
  jssInstance <- JSS.createInstance JSS.preset
  traverse_ (JSS.globalAttachStyleSheet <=< JSS.createStyleSheet jssInstance)
    [ globals
    , Badge.styles
    , Border.styles
    , Breadcrumb.styles
    , Button.styles
    , ButtonGroup.styles
    , Card.styles
    , CardGrid.styles
    , Column.styles
    , Details.styles
    , Divider.styles
    , DropdownButton.styles
    , EditableTable.styles
    , Form.styles
    , Icon.styles
    , Images.styles
    , Input.styles
    , InputGroup.styles
    , LabeledField.styles
    , Layouts.styles
    , Link.styles
    , List.styles
    , Loader.styles
    , Lockup.styles
    , Modal.styles
    , NativeSelect.styles
    , Navigation.styles
    , Pagination.styles
    , Pill.styles
    , Row.styles
    , Select.styles
    , Slider.styles
    , StatusSlat.styles
    , Tab.styles
    , Table.styles
    , Text.styles
    , Text.styles
    , Textarea.styles
    , Toast.styles
    , Tooltip.styles
    , Upload.styles
    ]

globals :: JSS
globals = jss
  { "@global":
      { "*.lumi":
          { boxSizing: "border-box"
          , margin: "0"
          }
      , "react-basic-ref":
          { display: "flex"
          , flexDirection: "column"
          , minHeight: "0"
          , minWidth: "0"
          }
      , "body":
          { color: cssStringHSLA colors.black
          , backgroundColor: cssStringHSLA colors.white
          , fontFeatureSettings: "tnum"
          , textRendering: "optimizeLegibility"
          , "WebkitFontSmoothing": "antialiased"
          }
      }
  }
