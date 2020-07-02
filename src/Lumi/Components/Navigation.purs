module Lumi.Components.Navigation where

import Prelude

import Color (cssStringHSLA)
import Data.Array (length)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.NonEmpty (NonEmpty, oneOf)
import Data.Nullable (toNullable)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, mkEffectFn1, runEffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import JSS (JSS, jss)
import Lumi.Components.Color (colorNames, colors)
import Lumi.Components.Column (column, column_)
import Lumi.Components.Divider (divider_, divider)
import Lumi.Components.Icon (IconType(..), icon_, icon)
import Lumi.Components.Images (avatar)
import Lumi.Components.Link (link, defaults)
import Lumi.Components.Row (row)
import Lumi.Components.Size (Size(..))
import Lumi.Components.Text (body_, subtext_, text, subtext)
import Lumi.Components.ZIndex (ziNavigationBar, ziNavigationDropdown)
import React.Basic.Classic (Component, JSX, createComponent, element, empty, fragment, make)
import React.Basic.DOM (css)
import React.Basic.DOM as R
import React.Basic.DOM.Components.GlobalEvents (windowEvent)
import React.Basic.DOM.Components.Ref (ref)
import React.Basic.DOM.Events (capture_)
import React.Basic.DOM.SVG as RS
import Web.DOM (Node)
import Web.Event.Event (EventType(..))
import Web.Event.Internal.Types (Event)
import Web.HTML.History (URL(..))

component :: Component NavigationProps
component = createComponent "Navigation"

type NavigationProps =
  { navLinks :: NonEmpty Array
      { text :: String
      , href :: URL
      , subNavLinks :: Array
          { text :: String
          , href :: URL
          }
      }
  , user ::
      { src :: String
      , name :: Maybe String
      , email :: Maybe String
      , id :: String
      }
  , client ::
      { name :: Maybe String
      }
  , onLogoutClick :: Maybe (Effect Unit)
  , compact :: Boolean
  , onCartClick :: Maybe (Effect Unit)
  , cartAmount :: Maybe Int
  , logo :: JSX
  }

data Action
  = ToggleOpen
  | ToggleUserDropdown
  | CloseUserDropdown

navigation :: NavigationProps -> JSX
navigation = make component { initialState, render }
  where
    initialState =
      { isOpen: false
      , showUserDropdown: false
      }

    send self = case _ of
      ToggleOpen ->
        self.setState \state ->
          state { isOpen = if not state.isOpen then true else false }

      ToggleUserDropdown ->
        self.setState \state ->
          state { showUserDropdown = if not state.showUserDropdown then true else false }

      CloseUserDropdown ->
        self.setState \state ->
          state { showUserDropdown = false }

    isMobileDevice = false

    render self@{ props, state } =
      let
        userEmailBlock =
          link defaults
              { href = URL "/settings"
              , text =
                  column_
                    [ fromMaybe empty $ map body_ props.user.name
                    , text subtext
                      { style = css { }
                      , color = toNullable (Just colorNames.black1)
                      , children =
                          [ fromMaybe empty $ map R.text props.user.email
                          ]
                      }
                    ]
              }
        navUtilityLinks =
          fragment
            [ navLinkElement
                { style: css {}
                , children:
                    [ link defaults
                        { href = URL "/settings"
                        , text = body_ "Settings"
                        }
                    ]
                }
            , navLinkElement
                { style: css {}
                , children:
                    [ link defaults
                        { href = URL "/help"
                        , text = body_ "Help"
                        }
                    ]
                }
            , navLinkElement
                { style: css {}
                , children:
                    [ link defaults
                        { text = body_ "Logout"
                        , navigate = props.onLogoutClick
                        }
                    ]
                }
            ]
        toNavLinks { text, href, subNavLinks } =
          navLinkElement
            { style: css
                { position: "relative"
                }
            , children:
                [ link defaults
                    { href = href
                    , text = body_ text
                    }
                , fragment
                    [ if length subNavLinks > 0
                      then subNavLinkContainerElement
                          { style: css {}
                          , children:
                              column
                                { style: css {}
                                , children:
                                    let toSubNavLinks xs =
                                          xs # map \subNavLink ->
                                              link defaults
                                                { text = body_ subNavLink.text
                                                , href = subNavLink.href
                                                }
                                     in toSubNavLinks subNavLinks
                                }
                          }
                      else empty
                    ]
                ]
            }
      in
        navigationElement
          { "data-compact": props.compact
          , children:
              [ navLinkContainerElement
                { "data-open": state.isOpen
                , children:
                    [ R.div
                        { style: css
                            { display: if not props.compact then "block" else "none"
                            }
                        , children:
                            [ desktopLogoContainerElement
                                { children:
                                    [ link defaults
                                        { href = URL "/"
                                        , text = props.logo
                                        , style = css
                                            { display: "block"
                                            , width: "65px"
                                            , height: "23px"
                                            , marginRight: "29px"
                                            }
                                        }
                                    ]
                                }
                            , mobileCloseContainerElement
                                { children:
                                    [ icon_ Close ]
                                , onClick: capture_ $ send self ToggleOpen
                                , style: css { marginBottom: "24px" }
                                }
                            ]
                        }
                    , divider_
                    , mobileLinksContainerElement
                        { children:
                          [ row
                              { style: css { padding: "16px 0" }
                              , children:
                                  [ avatar
                                      { image: R.img { src: props.user.src }
                                      , size: Large
                                      , style: css { marginRight: "12px"}
                                      }
                                  , userEmailBlock
                                  ]
                              }
                          ]
                        }
                    , divider_
                    , fragment $ map toNavLinks (oneOf props.navLinks)
                    , mobileLinksContainerElement
                        { children:
                          [ divider_
                          , navUtilityLinks
                          ]
                        }
                    ]
                }
            , mobileHamburgerContainerElement
                { children: [ icon_ Hamburger ]
                , onClick: capture_ $ send self ToggleOpen
                }
            , mobileLogoContainerElement
                { style: css
                    { width: "65px"
                    , height: "23px"
                    }
                , children:
                    [ link defaults
                      { href = URL "/"
                      , text = props.logo
                      , style = css { display: "block" }
                      }
                    ]
                }
              , navContentContainerElement
                  { children:
                      row
                        { style: css { alignItems: "center" }
                        , children:
                          [ if not props.compact
                            then cartContainerElement
                              { style: css { margin: "0 12px" }
                              , children:
                                [ case props.cartAmount of
                                    Just cartAmount ->
                                      fragment
                                        [ if cartAmount > 0
                                          then RS.svg
                                            { xmlns: "http://www.w3.org/2000/svg"
                                            , height: "6"
                                            , width: "6"
                                            , style: R.css
                                                { position: "absolute"
                                                , top: 3
                                                , left: -8
                                                }
                                            , children:
                                                [ RS.circle
                                                    { fill: "#0044e4"
                                                    , fillRule: "evenodd"
                                                    , strokeMiterlimit: "round"
                                                    , cx: "3"
                                                    , cy: "3"
                                                    , r: "3"
                                                    }
                                                ]
                                            }
                                          else empty
                                        ]
                                    Nothing ->
                                      empty
                                , icon_ Cart
                                ]
                              , onClick:
                                  mkEffectFn1 \_ -> do
                                    case props.onCartClick of
                                      Just onCartClick  -> onCartClick
                                      Nothing           -> pure unit
                              }
                            else empty
                          , ref \maybeNavRef ->
                              navLockupContainerElement
                                { onClick: capture_ $ send self ToggleUserDropdown
                                , style: css { marginLeft: "12px" }
                                , children:
                                    [ avatar
                                        { image: R.img { src: props.user.src }
                                        , size: if props.compact then Small else Medium
                                        , style: css { }
                                        }
                                    , row
                                        { style: css
                                            { color: "#91908d"
                                            , marginLeft: "8px"
                                            , alignItems: "center"
                                            }
                                        , children:
                                            if props.compact
                                            then
                                              [ fromMaybe empty $ map subtext_ props.user.name ]
                                            else
                                              [ fromMaybe empty $ map subtext_ props.client.name
                                              , icon
                                                  { type_: if state.showUserDropdown then ArrowUp else ArrowDown
                                                  , style: css { marginLeft: "8px" }
                                                  }
                                              ]
                                        }
                                    , windowEvent
                                        { eventType: EventType "click"
                                        , options: { capture: false, once: false, passive: false }
                                        , handler: case maybeNavRef of
                                            Nothing      -> \e -> pure unit
                                            Just navRef  -> \e -> do
                                              isEventTargetInTree <- runEffectFn2 checkIsEventTargetInTree navRef e
                                              when (not isEventTargetInTree) do
                                                send self CloseUserDropdown
                                        }
                                        $ subNavLinkContainerElement
                                            { style: css
                                                { display: if state.showUserDropdown then "block" else "none"
                                                }
                                            , children:
                                                column
                                                  { style: css {}
                                                  , children:
                                                      if props.compact
                                                      then
                                                        [ link defaults
                                                            { href = URL "/"
                                                            , text = body_ "Notifications"
                                                            }
                                                        , link defaults
                                                            { href = URL ("/users/" <> props.user.id)
                                                            , text = body_ "Your Account"
                                                            }
                                                        , link defaults
                                                            { text = body_ "Logout"
                                                            , navigate = props.onLogoutClick
                                                            }
                                                        ]
                                                      else
                                                        [ userEmailBlock
                                                        , divider { style: css { margin: "6px 0"} }
                                                        , navUtilityLinks
                                                        ]
                                                  }
                                            }
                                    ]
                                }
                          ]
                        }
                  }
              ]
          }

    navigationElement = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-navigation")
    navLinkContainerElement = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-navlink-container")
    navLinkElement = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-nav-link")
    subNavLinkContainerElement = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-subnav-link-container")
    navContentContainerElement = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-navcontent-container")
    cartContainerElement = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-cart-container")
    navLockupContainerElement = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-nav-lockup-container") -- @TODO need a navLockup component

    desktopLogoContainerElement = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-desktop-nav-logo-container")
    mobileCloseContainerElement = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-mobile-nav-close-container")
    mobileLogoContainerElement = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-mobile-nav-logo-container")
    mobileHamburgerContainerElement = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-mobile-nav-hamburger-container")
    mobileLinksContainerElement = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-mobile-nav-links-container")

foreign import checkIsEventTargetInTree :: EffectFn2 Node Event Boolean

styles :: JSS
styles = jss
  { "@global":
      { "lumi-navigation":
          { boxSizing: "border-box"
          , zIndex: ziNavigationBar
          , position: "relative"
          , display: "flex"
          , flexFlow: "row"
          , justifyContent: "space-between"
          , alignItems: "center"
          , height: "56px"
          , width: "100vw"
          , padding: "0 24px"
          , backgroundColor: cssStringHSLA colors.black6

          , "& lumi-mobile-nav-logo-container, & lumi-mobile-nav-hamburger-container, & lumi-mobile-nav-links-container, & lumi-mobile-nav-close-container":
              { display: "none"
              }
          , "& lumi-nav-link":
              { display: "flex"
              , alignItems: "center"
              , height: "100%"
              , "& a.lumi": { width: "100%" }
              , "&:hover lumi-subnav-link-container": { display: "block" }
              }
          , "& lumi-navlink-container":
              { display: "flex"
              , flexFlow: "row"
              , alignItems: "center"
              , height: "100%"
              , "& lumi-nav-link":
                  { marginRight: "24px"
                  , "&:last-child": { marginRight: "0" }
                  }
              , "& a.lumi, & a.lumi:visited":
                  { color: cssStringHSLA colors.secondary
                  }
              , "& lumi-subnav-link-container":
                  { left: "-8px"
                  }
              }
          , "& lumi-subnav-link-container":
              { boxSizing: "border-box"
              , position: "absolute"
              , zIndex: ziNavigationDropdown
              , top: "calc(100% - 4px)"
              , right: "16px"
              , display: "none"
              , background: cssStringHSLA colors.white
              , border: [ "1px", "solid", cssStringHSLA colors.black4 ]
              , borderRadius: "3px"
              , minWidth: "115px"
              , color: cssStringHSLA colors.secondary
              , padding: "5px 0"
              , "& a.lumi, & a.lumi:visited":
                  { textDecoration: "none"
                  , color: cssStringHSLA colors.black
                  , padding: "6px 12px"
                  , "&:first-child": { paddingTop: "6px" }
                  , "&:last-child": { paddingBottom: "6px" }
                  , "&:hover": { backgroundColor: cssStringHSLA colors.black7 }
                  }
              }
          , "& lumi-cart-container":
              { display: "block"
              , position: "relative"
              , fontSize: "23px"
              , color: cssStringHSLA colors.secondary
              , cursor: "pointer"
              }
          , "& lumi-nav-lockup-container":
              { display: "flex"
              , flexFlow: "row"
              , alignItems: "center"
              , cursor: "pointer"
              }
          , "& a.lumi, & a.lumi:visited":
              { "&:hover":
                  { color: cssStringHSLA colors.black
                  , textDecoration: "none"
                  }
              }

          , "&[data-compact=\"true\"]":
              { height: "40px"
              , borderBottom: [ "1px", "solid", cssStringHSLA colors.black4 ]

              , "& lumi-navlink-container lumi-nav-link":
                  { marginRight: "16px"
                  }
              }
          , "&[data-compact=\"false\"]":
              { "@media (max-width: 860px)":
                  { padding: "0 16px"

                  , "& lumi-navlink-container":
                      { position: "absolute"
                      , top: "0"
                      , left: "0"
                      , zIndex: ziNavigationDropdown

                      , display: "none"
                      , flexFlow: "column"
                      , height: "100vh"
                      , width: "calc(100vw - 16px * 2)"
                      , background: cssStringHSLA colors.white
                      , color: cssStringHSLA colors.black
                      , padding: "16px"

                      , "&[data-open=\"true\"]": { display: "block" }
                      , "& lumi-nav-link":
                          { margin: "24px 0"
                          , height: "auto"
                          }
                      , "&:hover lumi-subnav-link-container":
                          { display: "none"
                          }
                      , "& a.lumi, & a.lumi:visited":
                          { color: cssStringHSLA colors.black
                          }
                      , "& lumi-mobile-nav-close-container":
                          { display: "block"
                          , fontSize: "20px"
                          , color: cssStringHSLA colors.secondary
                          }
                      }
                  , "& lumi-desktop-nav-logo-container, & lumi-nav-lockup-container":
                      { display: "none"
                      }
                  , "& lumi-mobile-nav-logo-container, & lumi-mobile-nav-hamburger-container, & lumi-mobile-nav-links-container":
                      { display: "block"

              , maxHeight: "100%"
              , maxWidth: "100%"
                      }
                  , "& lumi-mobile-nav-hamburger-container":
                      { fontSize: "20px"
                      , color: cssStringHSLA colors.secondary
                      , "& lumi-font-icon": { display: "flex" }
                      }
                  }
              }
          }
      }
  }
