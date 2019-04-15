module Lumi.Components.Examples.Text where

import Prelude

import Lumi.Components.Column (column_)
import Lumi.Components.Text (body_, h1_, h2_, h3_, h4_, h5_, h6_, mainHeader_, p_, sectionHeader_, span_, subsectionHeader_, subtext_, title_, paragraph_)
import Lumi.Components.Example (example)
import React.Basic (JSX)

docs :: JSX
docs =
  column_
    [ p_ "There are six text sizes, each displayed below with its name. The lumi tags have no default padding. These sizes have also been mapped onto the browser tags with default padding."
    , example
        $ column_
            [ mainHeader_ "MainHeader"
            , title_ "Title"
            , sectionHeader_ "SectionHeader"
            , subsectionHeader_ "SubsectionHeader"
            , body_ "Body"
            , paragraph_ "Paragraph"
            , subtext_ "Subtext"

            , h1_ "h1"
            , h2_ "h2"
            , h3_ "h3"
            , h4_ "h4"
            , h5_ "h5"
            , h6_ "h6"
            , p_ "p"
            , span_ "span"
            ]
    ]
