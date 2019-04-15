module Lumi.Components.Examples.StatusSlat where

import Prelude

import Data.Nullable (notNull, null)
import Lumi.Components.Column (column_)
import Lumi.Components.Example (example)
import Lumi.Components.Status (Status(..))
import Lumi.Components.StatusSlat (statusSlat)
import React.Basic (JSX)

docs :: JSX
docs =
  column_
    [ example $
        statusSlat { data: statusSlat_one }
    , example $
        statusSlat { data: statusSlat_two }
    , example $
        statusSlat { data: statusSlat_three }
    , example $
        statusSlat { data: statusSlat_four }
    ]
  where
    statusSlat_one =
      [ { label: "Status"
        , content: "Complete"
        , status: notNull Active
        }
      , { label: "Subtext"
        , content: "Product name"
        , status: null
        }
      , { label: "Date ordered"
        , content: "2018-02-13"
        , status: null
        }
      , { label: "Total"
        , content: "$20.000"
        , status: null
        }
      ]

    statusSlat_two =
      [ { label: "Status"
        , content: "Complete"
        , status: notNull Active
        }
      , { label: "Payment status"
        , content: "Paid"
        , status: null
        }
      ]

    statusSlat_three =
      [ { label: "Status"
        , content: "Cancelled"
        , status: notNull Error
        }
      , { label: "Price"
        , content: "$0.01"
        , status: null
        }
      , { label: "Due date"
        , content: "2/14/18"
        , status: notNull Unknown
        }
      ]

    statusSlat_four =
      [ { label: "Status"
        , content: "In progress"
        , status: notNull Warning
        }
      , { label: "Weight"
        , content: "5.00 lb"
        , status: null
        }
      , { label: "Size"
        , content: "Medium"
        , status: null
        }
      , { label: "Price"
        , content: "$6.54"
        , status: null
        }
      ]
