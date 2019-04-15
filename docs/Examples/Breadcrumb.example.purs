module Lumi.Components.Examples.Breadcrumb where

import Prelude

import Data.Array.NonEmpty (cons')
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Lumi.Components.Breadcrumb (breadcrumb, compactBreadcrumb)
import Lumi.Components.Column (column_)
import Lumi.Components.Text (h2_)
import Lumi.Components.Example (example)
import React.Basic (JSX)
import Web.HTML.History (URL(..))

docs :: JSX
docs =
  column_
    [ example
        $ breadcrumb
            { onNavigate: toNullable Nothing
            , items: cons'
                { label: "Lorem", href: URL "#/input" }
                [ { label: "Ipsum", href: URL "#/link" }
                , { label: "Dolor", href: URL "" }
                ]
            }

    , h2_ "Compact variation"
    , example
        $ compactBreadcrumb
            { onNavigate: toNullable Nothing
            , items: cons'
                { label: "Lorem", href: URL "#/input" }
                [ { label: "Ipsum", href: URL "#/link" }
                ]
            }

    , example
        $ compactBreadcrumb
            { onNavigate: toNullable Nothing
            , items: cons'
                { label: "Lorem", href: URL "#/input" }
                []
            }
    ]
