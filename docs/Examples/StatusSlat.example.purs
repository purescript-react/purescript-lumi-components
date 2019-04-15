module Lumi.Components.Examples.StatusSlat where

import Prelude

import Data.Nullable (notNull, null)
import Lumi.Components.Status (Status(..))
import Lumi.Components.Column (column_)
import Lumi.Components.Text (h4_)
import Lumi.Components.StatusSlat (statusSlat)
import Lumi.Components.Example (example)
import React.Basic (JSX)

docs :: JSX
docs =
  column_
    [ h4_ "Product Lockup"

    , example $
        statusSlat { "data": statuSlat_one }
    , example $
        statusSlat { "data": statuSlat_two }
    , example $
        statusSlat { "data": statuSlat_three }
    , example $
        statusSlat { "data": statuSlat_four }
    ]
  where
    statuSlat_one =
      [ { label: "Status"
        , content: "Produced"
        , status: notNull Active
        }
      , { label: "Subtext"
        , content: "Unfulfilled (0/2)"
        , status: notNull Warning
        }
      , { label: "Placed"
        , content: "02/13/18 6:15 PM"
        , status: null
        }
      , { label: "Total"
        , content: "$0.000"
        , status: null
        }
      ]

    statuSlat_two =
      [ { label: "Status"
        , content: "Produced"
        , status: notNull Active
        }
      , { label: "Payment status"
        , content: "Paid"
        , status: notNull Active
        }
      ]

    statuSlat_three =
      [ { label: "Status"
        , content: "Paid"
        , status: notNull Active
        }
      , { label: "Amout due"
        , content: "$0.00"
        , status: null
        }
      , { label: "Due date"
        , content: "2/14/18"
        , status: null
        }
      ]

    statuSlat_four =
      [ { label: "Status"
        , content: "Paid"
        , status: null
        }
      , { label: "Total weight"
        , content: "5.00 lb"
        , status: null
        }
      , { label: "Actual shipping cost"
        , content: "$6.54"
        , status: null
        }
      , { label: "Delivery status"
        , content: "Unknown"
        , status: null
        }
      ]
