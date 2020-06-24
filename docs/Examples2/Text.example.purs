module Lumi.Components2.Examples.Text where

import Prelude

import Lumi.Components (($$$))
import Lumi.Components.Divider (flexDivider_)
import Lumi.Components.Example (example)
import Lumi.Components.Spacing (Space(..), hspace)
import Lumi.Components2.Box (column, row)
import Lumi.Components2.Text as T
import Lumi.Styles (StyleModifier)
import Lumi.Styles as S
import Lumi.Styles.Box (_flex)
import React.Basic.Classic (JSX, fragment)

docs :: JSX
docs =
  column
  $$$ [ T.sectionHeader $$$ "Paragraphs"
      , T.paragraph_ $$$ "Paragraphs are used for laying out chunks of text as concrete, individual and independent elements in a page."
      , example
          $ fragment
          $ [ T.paragraph_ $$$ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed lacinia imperdiet turpis, blandit mollis purus venenatis non. Curabitur tincidunt magna at purus facilisis tempus in ut massa. Donec egestas orci lacus, eu rhoncus ipsum finibus et. Curabitur eu dapibus massa."
            , T.paragraph_ $ T.subtext $$$ "Sed a arcu nibh. Aliquam rhoncus luctus gravida. Morbi pharetra sollicitudin arcu sit amet euismod. Maecenas blandit nisi ac metus tristique, ut euismod erat rhoncus. Donec semper ut est ac sodales. Sed elementum laoreet laoreet."
            ]

      , T.sectionHeader $$$ "Headers"
      , T.paragraph_ $$$ "Headers are block-level elements used for marking/separating sections of a layout or for displaying important pieces of information in a context."
      , example
          $ fragment
          $ [ T.mainHeader $$$ "MainHeader"
            , T.title $$$ "Title"
            , T.sectionHeader $$$ "SectionHeader"
            , T.subsectionHeader $$$ "SubsectionHeader"
            ]

      , T.paragraph_ $$$ "Text elements such as headers and paragraphs can be composed inside other block level elements such as boxes, columns and rows."
      , example
        $ fragment
        $ [ T.sectionHeader $ T.strong $$$ "A tiny story"
          , T.subsectionHeader $$$ "Hello!"
          , T.paragraph
            $$$ [ T.text
                  $ T.emphasized
                  $$$ "How are you? "
                , T.text
                  $$$ "Hope you're doing "
                , T.text
                  $ T.strong
                  $$$ "fine. "
                , T.text
                  $ T.subtext
                  $ T.muted
                  $$$ "Yes, I do."
                ]
          , T.paragraph
            $$$ [ T.text
                  $$$ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam congue ligula et odio rutrum, eu imperdiet ante laoreet. Cras mollis faucibus urna, eu luctus ligula condimentum ut. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Vivamus et mi mattis, maximus urna id, luctus neque. Sed non lorem molestie nibh suscipit condimentum id quis enim. Nunc tortor elit, posuere eu metus sed, finibus sagittis est. Fusce dapibus lacus vitae augue vulputate, in convallis lectus congue. "
                , T.text
                  $ T.strong
                  $$$ "Hi! "
                , T.text
                  $ T.subtext
                  $$$ "Hey!"
                ]
          , row
            $$$ [ T.paragraph
                  $ T.emphasized
                  $ _flex
                  $$$ [ T.text
                        $$$ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam congue ligula et odio rutrum, eu imperdiet ante laoreet. Cras mollis faucibus urna, eu luctus ligula condimentum ut. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Vivamus et mi mattis, maximus urna id, luctus neque. Sed non lorem molestie nibh suscipit condimentum id quis enim. Nunc tortor elit, posuere eu metus sed, finibus sagittis est. Fusce dapibus lacus vitae augue vulputate, in convallis lectus congue. "
                      , T.text
                        $ nonItalic
                        $$$ "Hi! "
                      , T.text
                        $ T.subtext
                        $ nonItalic
                        $$$ "Hey!"
                      ]
                , hspace S16
                , flexDivider_
                , hspace S16
                , column
                  $ _flex
                  $$$ [ T.paragraph_
                        $$$ "Vestibulum eu arcu eget lectus interdum maximus feugiat sed velit. Integer ullamcorper urna quis cursus mattis. Nam vel hendrerit purus. Aliquam fringilla dictum nunc at ornare. Morbi ornare blandit tincidunt. Etiam sodales fringilla libero, vitae pulvinar sapien luctus ac. Proin condimentum vitae risus id vestibulum. Sed sed turpis leo. Quisque ligula leo, facilisis eget metus ullamcorper, aliquet mollis tortor. Donec purus metus, maximus rutrum nunc eget, rhoncus tempor erat. Sed efficitur tellus id velit ullamcorper, ut dignissim neque pretium. Sed id metus porta, efficitur est non, vulputate sapien."
                      , T.paragraph_
                        $ T.subtext
                        $$$ "Donec maximus commodo ipsum vel elementum. Sed at nunc dapibus, vulputate tellus eu, finibus ante. Nunc dolor ante, auctor et rutrum quis, sagittis ut nulla. Integer vel tempus ipsum, vel laoreet orci. Curabitur eu sem bibendum, rhoncus risus vel, tristique mauris. Cras lobortis elit sit amet quam semper pretium. Nullam fermentum ut velit ac cursus."
                      ]
                ]
          ]
      ]

nonItalic :: StyleModifier
nonItalic =
  S.style_ (S.css { fontStyle: S.str "normal" })
