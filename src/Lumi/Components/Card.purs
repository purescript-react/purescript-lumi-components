module Lumi.Components.Card where

import Prelude

import Color (cssStringHSLA)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import JSS (JSS, jss)
import Lumi.Components.Color (colors)
import Lumi.Components.Link as Link
import Lumi.Components.Text (body_, subtext_)
import React.Basic.Classic (JSX, createComponent, element, fragment, makeStateless)
import React.Basic.DOM as R
import Web.HTML.History (URL)

type CardProps =
  { title :: String
  , subtitle :: String
  , children :: Array JSX
  , href :: Maybe URL
  , onNavigate :: URL -> Effect Unit
  , selected :: Boolean
  }

card :: CardProps -> JSX
card = makeStateless (createComponent "Card") render
  where
    lumiCard = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-card")
    lumiCardImg = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-card-img")
    lumiCardText = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-card-text")

    render props =
      let
        linkWrapper href child =
          Link.link Link.defaults
            { href = href
            , navigate = Just (props.onNavigate href)
            , text = child
            }
      in
        lumiCard
          { className: "lumi"
          , "data-selected": props.selected
          , children:
              [ maybe identity linkWrapper props.href $ fragment
                  [ lumiCardImg
                      { children: props.children
                      }
                  , lumiCardText
                      { children:
                          [ body_ props.title
                          , subtext_ props.subtitle
                          ]
                      }
                  ]
              ]
        }

defaults :: CardProps
defaults =
  { title: ""
  , subtitle: ""
  , children: []
  , href: Nothing
  , onNavigate: mempty
  , selected: false
  }

styles :: JSS
styles = jss
  { "@global":
      { "lumi-card":
          { position: "relative"

          , minWidth: cardMinWidth

          , boxSizing: "border-box"
          , border: "2px solid transparent"
          , borderRadius: "4px"

          , display: "flex"
          , flexFlow: "column nowrap"

          , "& > a.lumi":
              { display: "flex"
              , flexFlow: "column nowrap"
              , flex: "1"
              , "&:hover":
                  { textDecoration: "none"
                  , lumiCardText: { color: cssStringHSLA colors.primary }
                  }
              }

          , "& lumi-card-img":
              { overflow: "hidden"
              , position: "relative"
              , paddingBottom: "100%"

              , display: "flex"
              , backgroundColor: cssStringHSLA colors.black6
              , backgroundImage: "url(\"data:image/svg+xml;charset=UTF-8,%3csvg width='112' height='99' xmlns='http://www.w3.org/2000/svg'%3e%3cg stroke-width='2' stroke='%23CBCBC9' fill='none' fill-rule='evenodd' opacity='.7'%3e%3cpath d='M111 95L80.9997013 43 56 81.1326499 31.0002987 60.3333333 1 95'/%3e%3cpath d='M1 98h110V1H1z'/%3e%3cpath d='M49 33c0 6.6314752-5.3685248 12-12 12-6.62359 0-12-5.3685248-12-12 0-6.62359 5.37641-12 12-12 6.6314752 0 12 5.37641 12 12z'/%3e%3c/g%3e%3c/svg%3e \")"
              , backgroundSize: "72px"
              , backgroundPosition: "center"
              , backgroundRepeat: "no-repeat"

              , boxSizing: "border-box"
              , border: [ "1px", "solid", cssStringHSLA colors.black3 ]
              , borderTopRightRadius: "4px"
              , borderTopLeftRadius: "4px"

              , "& > *":
                  { position: "absolute"
                  , maxWidth: "100%"
                  }
              }

          , "& lumi-card-text":
              { minHeight: "36px"

              , textAlign: "center"

              , display: "flex"
              , flexFlow: "column nowrap"
              , flex: "1"
              , alignItems: "center"
              , justifyContent: "center"
              , padding: "16px 12px"

              , boxSizing: "border-box"
              , border: [ "1px", "solid", cssStringHSLA colors.black3 ]
              , borderBottomRightRadius: "4px"
              , borderBottomLeftRadius: "4px"
              , borderTop: "none"

              , color: cssStringHSLA colors.black

              , "& lumi-body":
                  { maxHeight: "32px"
                  , lineHeight: "16px"
                  , marginBottom: "4px"
                  , overflow: "hidden"
                  }
              , "& lumi-subtext": { color: cssStringHSLA colors.secondary }
              }

          , "&[data-selected=\"true\"]":
              { border: [ "2px", "solid", cssStringHSLA colors.primary ]

              , "& lumi-card-text":
                  { color: cssStringHSLA colors.primary
                  , borderColor: "transparent"
                  }
              , "& lumi-card-img":
                  { borderColor: "transparent"
                  , borderBottom: [ "1px", "solid", cssStringHSLA colors.black3 ]
                  }
              }
          }
      }
  }

cardMinWidth :: String
cardMinWidth = "185px"
