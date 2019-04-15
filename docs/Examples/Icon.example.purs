module Lumi.Components.Examples.Icon where

import Prelude

import Lumi.Components.Column (column_)
import Lumi.Components.Icon (IconType(..), icon_)
import Lumi.Components.Text (body, body_, text)
import Lumi.Components.Example (example)
import React.Basic (JSX)

docs :: JSX
docs =
  column_
    [ text body
        { children =
            [ body_ "Arrow Down"
            , example $ icon_ ArrowDown
            , body_ "Arrow Left"
            , example $ icon_ ArrowLeft
            , body_ "Arrow Right"
            , example $ icon_ ArrowRight
            , body_ "Arrow Up"
            , example $ icon_ ArrowUp
            , body_ "Bin"
            , example $ icon_ Bin
            , body_ "Cart"
            , example $ icon_ Cart
            , body_ "Check"
            , example $ icon_ Check
            , body_ "Close"
            , example $ icon_ Close
            , body_ "Currency"
            , example $ icon_ Currency
            , body_ "Grid"
            , example $ icon_ Grid
            , body_ "Hamburger"
            , example $ icon_ Hamburger
            , body_ "Indeterminate"
            , example $ icon_ Indeterminate
            , body_ "Info"
            , example $ icon_ Info
            , body_ "List"
            , example $ icon_ List
            , body_ "Minus"
            , example $ icon_ Minus
            , body_ "Overflow"
            , example $ icon_ Overflow
            , body_ "Percent"
            , example $ icon_ Percent
            , body_ "Plus"
            , example $ icon_ Plus
            , body_ "Rearrange"
            , example $ icon_ Rearrange
            , body_ "Remove"
            , example $ icon_ Remove
            , body_ "Search"
            , example $ icon_ Search
            ]
        }
    ]
