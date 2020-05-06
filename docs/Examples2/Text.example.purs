module Lumi.Components2.Examples.Text where

import Prelude

import Lumi.Components (($$$))
import Lumi.Components.Example (example)
import Lumi.Components2.Box (box)
import Lumi.Components2.Text as T
import React.Basic (JSX, fragment)

docs :: JSX
docs =
  box
  $$$ [ example
        $ fragment
        $ [ T.sectionHeader $$$ "A tiny story"
          , T.subsectionHeader $$$ "Hello!"
          , T.paragraph
            $$$ [ T.text
                  $ T._emphasis
                  $$$ "How are you? "
                , T.text
                  $$$ "Hope you're doing "
                , T.text
                  $ T._strong
                  $$$ "fine. "
                , T.text
                  $ T._subtext
                  $ T._muted
                  $$$ "Yes, I do."
                ]
          , T.paragraph
            $$$ [ T.text $$$ "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam congue ligula et odio rutrum, eu imperdiet ante laoreet. Cras mollis faucibus urna, eu luctus ligula condimentum ut. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos himenaeos. Vivamus et mi mattis, maximus urna id, luctus neque. Sed non lorem molestie nibh suscipit condimentum id quis enim. Nunc tortor elit, posuere eu metus sed, finibus sagittis est. Fusce dapibus lacus vitae augue vulputate, in convallis lectus congue. "
                , T.text $ T._strong $$$ "Hi! "
                , T.text $ T._subtext $$$ "Hey!"
                ]
          , T.paragraph_
            $$$ "Vestibulum eu arcu eget lectus interdum maximus feugiat sed velit. Integer ullamcorper urna quis cursus mattis. Nam vel hendrerit purus. Aliquam fringilla dictum nunc at ornare. Morbi ornare blandit tincidunt. Etiam sodales fringilla libero, vitae pulvinar sapien luctus ac. Proin condimentum vitae risus id vestibulum. Sed sed turpis leo. Quisque ligula leo, facilisis eget metus ullamcorper, aliquet mollis tortor. Donec purus metus, maximus rutrum nunc eget, rhoncus tempor erat. Sed efficitur tellus id velit ullamcorper, ut dignissim neque pretium. Sed id metus porta, efficitur est non, vulputate sapien."
          , T.paragraph_
            $ T._subtext
            $$$ "Donec maximus commodo ipsum vel elementum. Sed at nunc dapibus, vulputate tellus eu, finibus ante. Nunc dolor ante, auctor et rutrum quis, sagittis ut nulla. Integer vel tempus ipsum, vel laoreet orci. Curabitur eu sem bibendum, rhoncus risus vel, tristique mauris. Cras lobortis elit sit amet quam semper pretium. Nullam fermentum ut velit ac cursus."
          ]
      ]
