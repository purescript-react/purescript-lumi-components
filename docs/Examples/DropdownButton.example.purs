module Lumi.Components.Examples.DropdownButton where

import Prelude

import Color (cssStringHSLA)
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Effect.Console (log)
import Effect.Uncurried (mkEffectFn1)
import Lumi.Components.Button (button, defaults, secondary)
import Lumi.Components.Color (colors)
import Lumi.Components.Column (column, column_)
import Lumi.Components.DropdownButton (dropdownButton, dropdownButtonDefaults, dropdownIcon, dropdownIconDefaults, dropdownMenu, dropdownMenuDefaults)
import Lumi.Components.Example (example)
import Lumi.Components.Input (input, text_)
import Lumi.Components.InputGroup (inputGroup)
import Lumi.Components.Spacing (Space(..), vspace)
import Lumi.Components.Text (body_, h2_)
import React.Basic (JSX)
import React.Basic.DOM as R

docs :: JSX
docs =
  column_
    [ h2_ "left/default"
    , example $
        dropdownButton dropdownButtonDefaults
          { label = "Dropdown Button"
          , content = \closeSelf -> R.div
              { style: R.css { width: "328px", padding: "12px" }
              , children:
                [ button secondary
                  { title = "I can be any element"
                  , style = R.css { width: "100%", marginBottom: "8px" }
                  , onPress = mkEffectFn1 \_ -> closeSelf
                  }
                , button secondary
                  { title = "I can be any element"
                  , style = R.css { width: "100%", marginBottom: "8px" }
                  , onPress = mkEffectFn1 \_ -> closeSelf
                  }
                , button defaults
                  { title = "I can be any element"
                  , style = R.css { width: "100%" }
                  , onPress = mkEffectFn1 \_ -> closeSelf
                  }
                ]
              }
          }

    , h2_ "right"
    , example $
        dropdownButton dropdownButtonDefaults
          { label = "Dropdown Button"
          , alignment = toNullable (Just "right")
          , content = \closeSelf -> R.div
              { style: R.css { width: "328px", padding: "12px" }
              , children:
                [ button secondary
                  { title = "I can be any element"
                  , style = R.css { width: "100%", marginBottom: "8px" }
                  , onPress = mkEffectFn1 \_ -> closeSelf
                  }
                , button secondary
                  { title = "I can be any element"
                  , style = R.css { width: "100%", marginBottom: "8px" }
                  , onPress = mkEffectFn1 \_ -> closeSelf
                  }
                , button defaults
                  { title = "I can be any element"
                  , style = R.css { width: "100%" }
                  , onPress = mkEffectFn1 \_ -> closeSelf
                  }
                ]
              }
          }

    , h2_ "inside an InputGroup"
    , example $
        inputGroup
          { style: R.css { maxWidth: "70%"}
          , addOnLeft:
              toNullable $ Just $ R.div
                { children:
                  [ dropdownButton dropdownButtonDefaults
                    { label = "Dropdown Button"
                    , content = \closeSelf -> R.div
                        { style: R.css { width: "328px", padding: "12px" }
                        , children:
                          [ button secondary
                            { title = "I can be any element"
                            , style = R.css { width: "100%", marginBottom: "8px" }
                            , onPress = mkEffectFn1 \_ -> closeSelf
                            }
                          , button secondary
                            { title = "I can be any element"
                            , style = R.css { width: "100%", marginBottom: "8px" }
                            , onPress = mkEffectFn1 \_ -> closeSelf
                            }
                          , button defaults
                            { title = "I can be any element"
                            , style = R.css { width: "100%" }
                            , onPress = mkEffectFn1 \_ -> closeSelf
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
                    , content = \closeSelf -> R.div
                        { style: R.css { width: "328px", padding: "12px" }
                        , children:
                          [ button secondary
                            { title = "I can be any element"
                            , style = R.css { width: "100%", marginBottom: "8px" }
                            , onPress = mkEffectFn1 \_ -> closeSelf
                            }
                          , button secondary
                            { title = "I can be any element"
                            , style = R.css { width: "100%", marginBottom: "8px" }
                            , onPress = mkEffectFn1 \_ -> closeSelf
                            }
                          , button defaults
                            { title = "I can be any element"
                            , style = R.css { width: "100%" }
                            , onPress = mkEffectFn1 \_ -> closeSelf
                            }
                          ]
                        }
                    }
                  ]
                }
          , inputContent: toNullable $ Just $ input text_ { placeholder = "Placeholder..." }
          }

    , h2_ "as DropdownMenu"
    , example $
        dropdownMenu dropdownMenuDefaults
          { label = "Dropdown Menu"
          , items =
              [ [ { label: "Export all results to CSV", action: Just $ log "hello" }
                , { label: "Export selected results to CSV", action: Just $ log "hola" }
                , { label: "Assign manager", action: Nothing }
                ]
              , [ { label: "Archive", action: Just $ log "olá" }
                ]
              ]
          }
    , example $
        dropdownMenu dropdownMenuDefaults
          { label = "Dropdown Menu (right)"
          , alignment = toNullable (Just "right")
          , items =
              [ [ { label: "Export all results to CSV", action: Just $ log "hello" }
                , { label: "Export selected results to CSV", action: Just $ log "hola" }
                , { label: "Assign manager", action: Nothing }
                ]
              , [ { label: "Archive", action: Just $ log "olá" }
                ]
              ]
          }

    , h2_ "Inside a scrollable div"
    , example $
        column
          { style: R.css
              { alignItems: "flex-start"
              , overflow: "scroll"
              , padding: "12px"
              , height: "112px"
              , border: "1px solid " <> cssStringHSLA colors.black3
              , borderRadius: "5px"
              }
          , children:
              [ body_
                  """
                  Lorem ipsum dolor sit amet, consectetur adipiscing elit.
                  Suspendisse eget egestas massa. Curabitur semper, justo et
                  blandit fermentum, enim purus lacinia augue, et tincidunt
                  ipsum quam ut elit. Cras sem ligula, vestibulum sit amet
                  sapien at, euismod iaculis urna. Morbi dignissim ultrices
                  orci quis aliquet. Nulla quis nibh vehicula, scelerisque quam
                  id, tempus odio. Sed id dolor porttitor, imperdiet ligula a,
                  pharetra diam. Nulla quis tempor ante. Cras ut mauris orci.
                  Praesent tristique efficitur nibh vel varius. In rutrum
                  rhoncus elementum.
                  """
              , vspace S12
              , dropdownMenu dropdownMenuDefaults
                  { label = "Dropdown Menu"
                  , items =
                      [ [ { label: "Export all results to CSV", action: Just $ log "hello" }
                        , { label: "Export selected results to CSV", action: Just $ log "hola" }
                        , { label: "Assign manager", action: Nothing }
                        ]
                      ]
                  }
              , vspace S24
              , body_
                  """
                  Aliquam vel egestas lectus, non pellentesque ante. Vestibulum
                  tempor eleifend aliquet. Donec a aliquam nulla, ac interdum
                  elit. Nulla eu semper mauris. Donec a pulvinar leo. Aliquam
                  erat volutpat.
                  """
              ]
          }

    , h2_ "using an arbitrary icon in place of the button"
    , example $
        dropdownIcon dropdownIconDefaults
          { content = \closeSelf -> R.div
              { style: R.css { width: "328px", padding: "12px" }
              , children:
                [ button secondary
                  { title = "I can be any element"
                  , style = R.css { width: "100%", marginBottom: "8px" }
                  , onPress = mkEffectFn1 \_ -> closeSelf
                  }
                , button secondary
                  { title = "I can be any element"
                  , style = R.css { width: "100%", marginBottom: "8px" }
                  , onPress = mkEffectFn1 \_ -> closeSelf
                  }
                , button defaults
                  { title = "I can be any element"
                  , style = R.css { width: "100%" }
                  , onPress = mkEffectFn1 \_ -> closeSelf
                  }
                ]
              }
          }

    ]
