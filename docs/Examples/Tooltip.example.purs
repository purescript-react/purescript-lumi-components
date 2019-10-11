module Lumi.Components.Examples.Tooltip where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Lumi.Components.Size (Size(..))
import Lumi.Components.Column (column_)
import Lumi.Components.Link as Link
import Lumi.Components.Row (row_)
import Lumi.Components.Text (body_)
import Lumi.Components.Text as T
import Lumi.Components.Example (example)
import Lumi.Components.Tooltip (tooltip)
import React.Basic (JSX)
import React.Basic.DOM as R
import Web.HTML.History (URL(..))

docs :: JSX
docs =
  column_
    [ example
        $ tooltip
            { variant: "basic"
            , style: R.css {}
            , text: R.text "Lorem ipsum"
            , content: body_ "Basic example"
            , size: toNullable Nothing
            }

    , example
        $ tooltip
            { variant: "top"
            , style: R.css {}
            , text: R.text "Lorem ipsum"
            , content: body_ "Top example"
            , size: toNullable Nothing
            }

    , example
        $ tooltip
            { variant: "bottom"
            , style: R.css {}
            , text: R.text "Lorem ipsum"
            , content: body_ "Bottom example"
            , size: toNullable Nothing
            }

    , example
        $ tooltip
            { variant: "left"
            , style: R.css {}
            , text: R.text "Lorem ipsum"
            , content: body_ "Left example"
            , size: toNullable Nothing
            }

    , example
        $ row_
              [ body_ "Hello, world see"
              , tooltip
                  { variant: "top"
                  , style: R.css { padding: "0 2px", textDecoration: "underline" }
                  , text: R.text "Lorem ipsum"
                  , content: body_ "here"
                  , size: toNullable Nothing
                  }
              , body_ "for more."
              ]

    , example
        $ row_
              [ body_ "Hello, world see"
              , tooltip
                  { variant: "top"
                  , style: R.css { padding: "0 2px", textDecoration: "underline" }
                  , text: R.text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Duis auctor libero non libero consequat, at iaculis diam venenatis. Donec nec porttitor tellus."
                  , content: body_ "here"
                  , size: toNullable $ Just $ Large
                  }
              , body_ "for more."
              ]

    , example
        $ row_
              [ body_ "Hello, world see"
              , tooltip
                  { variant: "top"
                  , style: R.css { padding: "0 2px", textDecoration: "underline" }
                  , text:
                      T.text T.body
                        { style = R.css {}
                        , children =
                            [ R.text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Duis auctor libero non libero consequat, at iaculis diam venenatis. Donec nec porttitor tellus "
                            , Link.link Link.defaults
                                { href = URL "#/link"
                                , text = R.text "click here"
                                , target = Just "_blank"
                                }
                            , R.text "."
                            ]
                        }
                  , content: body_ "here"
                  , size: toNullable $ Just $ Large
                  }
              , body_ "for more (includes link in tooltip)."
              ]
    ]
