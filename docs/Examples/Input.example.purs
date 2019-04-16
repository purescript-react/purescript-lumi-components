module Lumi.Components.Examples.Input where

import Prelude

import Data.Maybe (Maybe(Just))
import Data.Nullable (toNullable)
import Lumi.Components.Size (Size(..))
import Lumi.Components.Column (column_)
import Lumi.Components.Input (CheckboxState(..), alignToInput, checkbox, input, inputRow, inputRow_, radio, range, switch, text_)
import Lumi.Components.Row (row_)
import Lumi.Components.Text (body_, h2_, h4_, p_)
import Lumi.Components.Example (example)
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
        $ input text_
            { placeholder = "Placeholder..."
            , value = "This input contains a value"
            }


    , h4_ "Invalid input value"
    , example
        $ input text_
            { placeholder = "Placeholder..."
            , pattern = toNullable $ Just "d+"
            , value = "An invalid value"
            }


    , h4_ "Disabled input"
    , example
        $ input text_
            { placeholder = "Placeholder..."
            , disabled = true
            }


    , h4_ "Disabled input with a value"
    , example
        $ input text_
            { placeholder = "Placeholder..."
            , value = "Disabled with a value"
            , disabled = true
            }


    , h4_ "Aligning components with input text"
    , p_ "`AlignToInput` applies the padding values inputs use to any child component. Use this to align elements with inputs instead of manually specifying padding."
    , example
        $ row_
            [ alignToInput $ body_ "Some aligned text"
            , input text_
                { placeholder = "Placeholder..."
                , value = "Input value"
                }
            ]


    , h2_ "Input Row"

    , h4_ "Padded"

    , example
        $ inputRow_ "Field label" checkbox { checked = Off }

    , example
        $ inputRow_ "Field label" checkbox
            { size = Small
            , checked = Off
            }

    , example
        $ inputRow_ "Field label" radio { checked = Off }

    , h4_ "Left aligned"

    , example
        $ inputRow { labelText: "Field label", leftAligned: true, style: R.css {} } checkbox { checked = Off }

    , example
        $ inputRow { labelText: "Field label", leftAligned: true, style: R.css {} } checkbox
            { size = Small
            , checked = Off
            }

    , example
        $ inputRow { labelText: "Field label", leftAligned: true, style: R.css {} } radio { checked = Off }


    , h2_ "Checkbox"

    , h4_ "Medium"
    , example
        $ inputRow_ "Field label" checkbox { checked = Off }


    , example
        $ inputRow_ "Field label" checkbox { checked = On }


    , example
        $ inputRow_ "Field label" checkbox { checked = Indeterminate }


    , h4_ "Small"
    , example
        $ inputRow_ "Field label" checkbox
            { size = Small
            , checked = Off
            }


    , example
        $ inputRow_ "Field label" checkbox
            { size = Small
            , checked = On
            }


    , example
        $ inputRow_ "Field label" checkbox
            { size = Small
            , checked = Indeterminate
            }


    , h2_ "Radio"

    , h4_ "Medium"
    , example
        $ inputRow_ "Field label" radio { checked = Off }


    , example
        $ inputRow_ "Field Label" radio { checked = On }


    , h4_ "Small"
    , example
        $ inputRow_ "Field Label" radio
            { size = Small
            , checked = Off
            }


    , example
        $ inputRow_ "Field label" radio
            { size = Small
            , checked = On
            }

    , h2_ "Switch"

    , example
        $ input switch { checked = Off }


    , example
        $ input switch { checked = On }

    , h2_ "Switch (Large)"

    , example
        $ input switch { checked = Off, size = Large }


    , example
        $ input switch { checked = On, size = Large }



    , h2_ "Range"

    , example
        $ input range


    ]
