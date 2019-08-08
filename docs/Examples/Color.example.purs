module Lumi.Components.Examples.Color where

import Prelude
import Color (cssStringHSLA)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Nullable (toNullable)
import Data.String (Pattern(..), split)
import Lumi.Components.Button (button, defaults)
import Lumi.Components.Color (ColorMap, colorNames, colors)
import Lumi.Components.Column (column, column_)
import Lumi.Components.Row (row, row_)
import Lumi.Components.Size (Size(..))
import Lumi.Components.Text (body, body_, subtext, text)
import React.Basic (JSX)
import React.Basic.DOM (css)
import React.Basic.DOM as R

docs :: JSX
docs =
  column_
    [ row
        { style: css { flexWrap: "wrap" }
        , children:
          [ section
              [ intercalate divider
                  [ colorPanel _.black "Black"
                      """default text color
active nav links
active tabs
active grid/lines icon on items page"""
                  , colorPanel _.black1 "Black - 50%"
                      """secondary text color
subtext
inactive nav links
inactive tabs
column titles
column sort arrow icon color"""
                  , colorPanel _.black2 "Black - 30%"
                      """text in disabled buttons and form fields
icons in disabled buttons and form fields
placeholder text in form fields
toggle background
Form field border (Hover state)
Checkbox border color (Hover state)
Radio button border color (Hover state)
Grey status pill text color
inactive list/grid icon color"""
                  , colorPanel _.black3 "Black - 18%"
                      """Form field border (Regular/Inactive state)
Checkbox border color (Regular/Inactive state)
Radio button border color (Regular/Inactive state)"""
                  , colorPanel _.black4 "Black - 12%"
                      """Table/list line colors
Divider line color
Slat border color
Container box color
Thumbnail border color
Payment method border color
Grey status pill border color
Slider grey bar background color"""
                  , colorPanel _.black5 "Black - 6%"
                      """disabled input field background
disabled checkbox background color
disabled radio button background color
disabled toggle background color"""
                  , colorPanel _.black6 "Black - Background gray"
                      """Nav bar background color
Image background color"""
                  , colorPanel _.black7 "Black - 4%" """dropdown hover background color"""
                  , colorPanel _.black8 "Black - 2%" """table row hover background color"""
                  ]
              ]
          , section
              [ intercalate divider
                  [ colorPanel _.primary "Primary"
                      """primary actions
active states
links"""
                  , colorPanel _.primary1 "Primary - 35%" """1px active border for input fields, radio buttons, and checkboxes"""
                  , colorPanel _.primary2 "Primary - 25%" """Used for the disabled primary button background color"""
                  , colorPanel _.primary3 "Primary - 15%" """tag color for multi select dropdowns, the 3px focus border for inputs, and the selected row background color in input dropdowns"""
                  , colorPanel _.primary4 "Primary - 7%"
                      """Input dropdown row hover background color
selected table row background color"""
                  , colorPanel _.accent1 "Accent 1" """Indicates good or complete status"""
                  , colorPanel _.accent2 "Accent 2" """Indicates pending, needs attention"""
                  , colorPanel _.accent3 "Accent 3" """Indicates a problem, warning"""
                  , colorPanel _.accent33 "Accent 3 - 15%" """Faded for focus borders/backgrounds"""
                  , colorPanel _.white "White" """Primary button text"""
                  ]
              ]
          ]
        }
    ]
  where
  colorPanel :: (forall a. ColorMap a -> a) -> String -> String -> JSX
  colorPanel color title notes =
    row_
      [ column
          { style: css { padding: "5px", alignItems: "center", color: cssStringHSLA colors.black1 }
          , children:
            [ button defaults { size = Large, color = toNullable $ Just (color colorNames) }
            , text
                body
                  { children = [ R.text (unwrap <<< color $ colorNames) ]
                  }
            ]
          }
      , column
          { style: css { padding: "5px" }
          , children:
            [ body_ title ]
              <> ( split (Pattern "\n") notes
                    <#> \note ->
                        text
                          subtext
                            { style = css { color: cssStringHSLA colors.black1 }
                            , children = [ R.text note ]
                            }
                )
          }
      ]

  section panels =
    R.section
      { style: css { maxWidth: "700px", flex: "1 1 0" }
      , children: panels
      }

  divider = row { style: css { height: "20px" }, children: [] }
