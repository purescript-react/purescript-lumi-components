module Lumi.Components.Examples.LabeledField where

import Prelude

import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Lumi.Components.Column (column, column_)
import Lumi.Components.Divider (divider)
import Lumi.Components.Input as Input
import Lumi.Components.LabeledField (RequiredField(..), ValidationMessage(..), defaults, labeledField)
import Lumi.Components.Text (h2_)
import Lumi.Components.Example (example)
import React.Basic (JSX)
import React.Basic.DOM (css)
import React.Basic.DOM as R

docs :: JSX
docs =
  column_
    [ h2_ "Standard Form"
    , example
        $ column
            { style: css { alignSelf: "stretch" }
            , children: [ intercalate simpleDivider labeledFieldExamples ]
            }

    , h2_ "Forced Top Labels Form"
    , example
        $ column
            { style: css { alignSelf: "stretch" }
            , children: [ intercalate simpleDivider labeledFieldTopLabelExamples ]
            }

    , h2_ "Inline Table Form"
    , example
        $ column
            { style: css { alignSelf: "stretch" }
            , children:
                [ inlineTableDivider
                , intercalate inlineTableDivider labeledFieldExamples
                , inlineTableDivider
                ]
            }

    , h2_ "Inline Table Form (Nested)"
    , example
        $ column
            { style: css { alignSelf: "stretch" }
            , children:
                [ inlineTableDivider
                , intercalate inlineTableDivider inlineTableNestedExample
                , inlineTableDivider
                ]
            }
    ]
  where

    simpleDivider = divider { style: css { margin: "6px 0", visibility: "hidden" } }

    inlineTableDivider = divider { style: css { margin: "6px 0" } }

    makeLabeledFieldExamples forceTopLabel =
      [ labeledField defaults
          { label = R.text "First Name"
          , forceTopLabel = forceTopLabel
          , value = Input.input Input.text_ { value = "Asdf" }
          }
      , labeledField defaults
          { label = R.text "Last Name"
          , validationError = Just $ Error "Example validation error."
          , forceTopLabel = forceTopLabel
          }
      , labeledField defaults
          { label = R.text "Username"
          , forceTopLabel = forceTopLabel
          , required = Optional
          }
      , labeledField defaults
          { label = R.text "Password"
          , forceTopLabel = forceTopLabel
          , required = Required
          }
      , labeledField defaults
          { label = R.text "Admin?"
          , value = Input.input Input.switch
          , forceTopLabel = forceTopLabel
          }
      ]

    labeledFieldExamples = makeLabeledFieldExamples false

    labeledFieldTopLabelExamples = makeLabeledFieldExamples true

    inlineTableNestedExample =
      [ labeledField defaults
          { label = R.text "User Info"
          , value = intercalate inlineTableDivider labeledFieldExamples
          }
      , labeledField defaults
          { label = R.text "How many?"
          , value = Input.input Input.range
          }
      ]
