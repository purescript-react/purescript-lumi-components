module Lumi.Components.Examples.ButtonGroup where

import Prelude

import Effect.Console (log)
import Effect.Uncurried (mkEffectFn1)
import Lumi.Components.Button (button, primary, secondary)
import Lumi.Components.ButtonGroup (buttonGroup)
import Lumi.Components.Column (column_)
import Lumi.Components.NativeSelect (nativeSelect, defaults)
import Lumi.Components.Text (h2_)
import Lumi.Components.Example (example)
import React.Basic (JSX)
import React.Basic.DOM as R

docs :: JSX
docs =
  column_
    [ h2_ "Not Joined"
    , example
        $ buttonGroup
            { joined: false
            , style: R.css {}
            , children:
                [ button primary { title = "Button" }
                , button secondary { title = "Button" }
                ]
            }
    , h2_ "Not Joined"
    , example
        $ buttonGroup
            { joined: false
            , style: R.css {}
            , children:
                [ button primary { title = "Button" }
                , button secondary { title = "Button" }
                , button secondary { title = "Button" }
                ]
            }
    , h2_ "Not Joined"
    , example
        $ buttonGroup
            { joined: false
            , style: R.css {}
            , children:
                [ button primary { title = "Button" }
                , nativeSelect defaults
                    { options = []
                    , onChange = mkEffectFn1 \_ -> log "onChange"
                    , value = "Foo bar"
                    }
                , button secondary { title = "Button" }
                ]
            }
    , h2_ "Joined"
    , example
        $ buttonGroup
            { joined: true
            , style: R.css {}
            , children:
                [ button secondary { title = "Button" }
                , button secondary { title = "Button" }
                ]
            }
    , h2_ "Joined"
    , example
        $ buttonGroup
            { joined: true
            , style: R.css {}
            , children:
                [ button secondary { title = "Button" }
                , button secondary { title = "Button" }
                , button secondary { title = "Button" }
                ]
            }
    ]
