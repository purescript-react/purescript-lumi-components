module Lumi.Components.Examples.Input where

import Prelude
import Data.Maybe (Maybe(Just))
import Data.Nullable (toNullable)
import Lumi.Components.Column (column_)
import Lumi.Components.Example (example)
import Lumi.Components.Input (CheckboxState(..), alignToInput, checkbox, input, radio, range, switch, text_)
import Lumi.Components.Row (row_)
import Lumi.Components.Size (Size(..))
import Lumi.Components.Text (body, h2_, h4_, p_, text)
import React.Basic (JSX)
import React.Basic.DOM as R

docs :: JSX
docs =
  column_
    [ h4_ "Simple text input"
    , example
        $ input text_ { placeholder = "Placeholder..." }
    , h4_ "Text input with a placeholder"
    , example
        $ input
            text_
              { placeholder = "Placeholder..."
              , value = "This input contains a value"
              }
    , h4_ "Invalid input value"
    , example
        $ input
            text_
              { placeholder = "Placeholder..."
              , pattern = toNullable $ Just "d+"
              , value = "An invalid value"
              }
    , h4_ "Disabled input"
    , example
        $ input
            text_
              { placeholder = "Placeholder..."
              , disabled = true
              }
    , h4_ "Disabled input with a value"
    , example
        $ input
            text_
              { placeholder = "Placeholder..."
              , value = "Disabled with a value"
              , disabled = true
              }
    , h4_ "Aligning components with input text"
    , p_ "`AlignToInput` applies the padding values inputs use to any child component. Use this to align elements with inputs instead of manually specifying padding."
    , example
        $ row_
            [ alignToInput
                $ text
                    body
                      { children = [ R.text "Some aligned text" ]
                      , style = R.css { whiteSpace: "nowrap" }
                      }
            , input
                text_
                  { placeholder = "Placeholder..."
                  , value = "Input value"
                  }
            ]
    , h2_ "Checkbox"
    , h4_ "Medium"
    , exampleRow $ input checkbox { checked = Off }
    , exampleRow $ input checkbox { checked = On }
    , exampleRow $ input checkbox { checked = Indeterminate }
    , h4_ "Small"
    , exampleRow
        $ input
            checkbox
              { size = Small
              , checked = Off
              }
    , exampleRow
        $ input
            checkbox
              { size = Small
              , checked = On
              }
    , exampleRow
        $ input
            checkbox
              { size = Small
              , checked = Indeterminate
              }
    , h2_ "Radio"
    , h4_ "Medium"
    , exampleRow $ input radio { checked = Off }
    , exampleRow $ input radio { checked = On }
    , h4_ "Small"
    , exampleRow
        $ input
            radio
              { size = Small
              , checked = Off
              }
    , exampleRow
        $ input
            radio
              { size = Small
              , checked = On
              }
    , h2_ "Switch"
    , exampleRow $ input switch { checked = Off }
    , exampleRow $ input switch { checked = On }
    , h2_ "Switch (Large)"
    , exampleRow $ input switch { checked = Off, size = Large }
    , exampleRow $ input switch { checked = On, size = Large }
    , h2_ "Range"
    , exampleRow $ input range
    ]
  where
  exampleRow child = example $ row_ [ child ]
