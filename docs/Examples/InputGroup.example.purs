module Lumi.Components.Examples.InputGroup where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Lumi.Components.Button (button, defaults, secondary)
import Lumi.Components.Column (column_)
import Lumi.Components.Input (input, text_)
import Lumi.Components.InputGroup (inputGroup)
import Lumi.Components.Text (h2_)
import Lumi.Components.DropdownButton (dropdownButton, dropdownButtonDefaults)
import Lumi.Components.Example (example)
import React.Basic (JSX)
import React.Basic.DOM as R

docs :: JSX
docs =
  column_
    [ example
        $ inputGroup
            { style: R.css { maxWidth: "70%"}
            , addOnLeft:
                toNullable $ Just $ R.div
                  { children:
                    [ button defaults { title = "Button", disabled = false }
                    ]
                  }
            , addOnRight:
                toNullable $ Just $ R.div
                  { children:
                    [ button defaults { title = "Button", disabled = false }
                    ]
                  }
            , inputContent: toNullable $ Just $ input text_ { placeholder = "Placeholder..." }
            }

    , h2_ "With DropdownButtons"
    , example
        $ inputGroup
            { style: R.css { maxWidth: "70%"}
            , addOnLeft:
                toNullable $ Just $ R.div
                  { children:
                    [ dropdownButton dropdownButtonDefaults
                      { label = "Dropdown Button"
                      , content = R.div
                          { style: R.css { width: "328px", padding: "12px" }
                          , children:
                            [ button secondary
                              { title = "I can be any element"
                              , style = R.css { width: "100%", marginBottom: "8px" }
                              }
                            , button secondary
                              { title = "I can be any element"
                              , style = R.css { width: "100%", marginBottom: "8px" }
                              }
                            , button defaults
                              { title = "I can be any element"
                              , style = R.css { width: "100%" }
                              }
                            ]
                          }
                      }
                    ]
                  }
            , addOnRight:
                toNullable $ Just $ R.div
                  { children:
                    [ dropdownButton dropdownButtonDefaults
                      { label = "Dropdown Button"
                      , content = R.div
                          { style: R.css { width: "328px", padding: "12px" }
                          , children:
                            [ button secondary
                              { title = "I can be any element"
                              , style = R.css { width: "100%", marginBottom: "8px" }
                              }
                            , button secondary
                              { title = "I can be any element"
                              , style = R.css { width: "100%", marginBottom: "8px" }
                              }
                            , button defaults
                              { title = "I can be any element"
                              , style = R.css { width: "100%" }
                              }
                            ]
                          }
                      }
                    ]
                  }
            , inputContent: toNullable $ Just $ input text_ { placeholder = "Placeholder..." }
            }
    ]
