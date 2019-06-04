module Lumi.Components.DropdownButton where

import Prelude

import Color (cssStringHSLA)
import Control.MonadZero (guard)
import Data.Foldable (fold, foldMap, intercalate)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Nullable as Nullable
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import JSS (JSS, jss)
import Lumi.Components.Button (button, secondary)
import Lumi.Components.Color (colorNames, colors)
import Lumi.Components.Column (column_)
import Lumi.Components.Divider (divider_)
import Lumi.Components.Link as Link
import Lumi.Components.Text (p_)
import Lumi.Components.Text as Text
import Lumi.Components.ZIndex (ziDropdownButton)
import Math as Math
import React.Basic (Component, JSX, createComponent, element, fragment, make, makeStateless, readProps, readState)
import React.Basic.DOM as R
import React.Basic.DOM.Components.GlobalEvents (windowEvent)
import React.Basic.DOM.Components.Ref (ref)
import React.Basic.DOM.Events (stopPropagation)
import React.Basic.Events (handler, handler_)
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)
import Web.DOM (Node)
import Web.DOM.Document (createElement) as DOM
import Web.DOM.Element (setId, toNode) as DOM
import Web.DOM.Node (appendChild) as DOM
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.Event.Event (EventType(..))
import Web.Event.Internal.Types (Event)
import Web.HTML (window) as HTML
import Web.HTML.HTMLDocument (body, toDocument, toNonElementParentNode) as HTML
import Web.HTML.HTMLElement (HTMLElement, fromNode, getBoundingClientRect, toNode) as HTML
import Web.HTML.Window (document, innerWidth, scrollX, scrollY) as HTML
import Web.HTML.Window (requestAnimationFrame)

type DropdownButtonProps =
 { label :: String
 , content :: JSX
 , className :: String
 , onOpen :: Effect Unit
 , alignment :: Nullable String
 }

dropdownButtonComponent :: Component DropdownButtonProps
dropdownButtonComponent = createComponent "DropdownButton"

dropdownButton :: DropdownButtonProps -> JSX
dropdownButton =
  make dropdownButtonComponent
    { initialState
    , shouldUpdate
    , didMount
    , render
    }
  where
    initialState =
      { isOpen: false
      , root: Nothing
      , position:
          { bottom: 0.0
          , left: 0.0
          , right: 0.0
          }
      }

    shouldUpdate { props, state } { nextProps, nextState } =
      props.label /= nextProps.label ||
      not (unsafeRefEq props.content nextProps.content) ||
      props.className /= nextProps.className ||
      props.alignment /= nextProps.alignment ||
      state.isOpen /= nextState.isOpen ||
      state.position /= nextState.position

    didMount self = do
      rootM <- DOM.getElementById "dropdown-root" <<< HTML.toNonElementParentNode =<< HTML.document =<< HTML.window
      case rootM of
        Nothing -> do
          rootEl <- DOM.createElement "div" <<< HTML.toDocument =<< HTML.document =<< HTML.window
          DOM.setId "dropdown-root" rootEl

          bodyM <- HTML.body =<< HTML.document =<< HTML.window
          bodyM # foldMap \body -> do
            void $ DOM.appendChild (DOM.toNode rootEl) (HTML.toNode body)
            self.setState _{ root = Just rootEl }

        Just rootEl ->
          self.setState _{ root = Just rootEl }

    capturePosition self ref p0 =
      HTML.fromNode ref # foldMap \el -> do
        props <- readProps self
        state <- readState self

        { bottom, left } <- HTML.getBoundingClientRect el
        let p0'@{ bottom: bottom0, left: left0 } = fromMaybe { bottom, left } p0

        -- Close the dropdown if the user has scrolled too far.
        if Math.abs (bottom - bottom0) > 64.0 || Math.abs (left - left0) > 64.0
          then close self
          else do
            p <- getAbsolutePosition el
            when state.isOpen $
              void $ requestAnimationFrame (capturePosition self ref (Just p0')) =<< HTML.window
            self.setState _{ position = p }

    toggleOpen self refM = do
      self.setStateThen
        (_ { isOpen = not self.state.isOpen })
        do
          props <- readProps self
          state <- readState self
          when state.isOpen props.onOpen
          refM # foldMap \ref ->
            capturePosition self ref Nothing

    close self = do
      window <- HTML.window
      self.setState _{ isOpen = false }

    render self@{ props, state } =
        lumiDropdownButton
          { "class": props.className <> " lumi"
          , "data-alignment": props.alignment
          , children:
              [ ref \maybeDropdownButtonRef ->
                  fragment
                    [ button secondary
                        { title = props.label
                        , onPress = handler_ $ toggleOpen self maybeDropdownButtonRef
                        }
                    , fold ado
                        rootEl <- state.root
                        guard state.isOpen
                      in
                        closeOnWindowClick maybeDropdownButtonRef <<< closeOnWindowResize
                          $ flip R.createPortal rootEl
                          $ lumiDropdownButtonContent
                              { style: R.mergeStyles
                                  [ R.css
                                      { top: show (state.position.bottom + 4.0) <> "px"
                                      }
                                  , if maybe false (_ == "right") (toMaybe props.alignment) then
                                      R.css { left: "auto", right: show (state.position.right - 1.0) <> "px" }
                                    else
                                      R.css { left: show (state.position.left - 1.0) <> "px" }
                                  ]
                              , onClick: handler stopPropagation mempty
                              , children: [ props.content ]
                              }
                    ]
              ]
          }
      where
        closeOnWindowClick refM =
          windowEvent
            { eventType: EventType "click"
            , options: { capture: false, once: false, passive: false }
            , handler: case refM of
                Nothing -> \_ -> pure unit
                Just dropdownButtonRef -> \e -> do
                  isEventTargetInTree <- runEffectFn2 checkIsEventTargetInTree dropdownButtonRef e
                  when (not isEventTargetInTree) do
                    close self
            }

        closeOnWindowResize =
          windowEvent
            { eventType: EventType "resize"
            , options: { capture: false, once: false, passive: false }
            , handler: \_ -> close self
            }

    lumiDropdownButton = element $ R.unsafeCreateDOMComponent "lumi-dropdown-button"
    lumiDropdownButtonContent = element $ R.unsafeCreateDOMComponent "lumi-dropdown-button-content"
    lumiDropdownButtonMarker = element $ R.unsafeCreateDOMComponent "lumi-dropdown-button-marker"

type DropdownMenuProps =
  { label :: String
  , className :: String
  , alignment :: Nullable String
  , items ::
     Array (Array
       { label :: String
       , action :: Effect Unit
       })
  }

dropdownMenuComponent :: Component DropdownMenuProps
dropdownMenuComponent = createComponent "DropdownMenu"

dropdownMenu :: DropdownMenuProps -> JSX
dropdownMenu = makeStateless dropdownMenuComponent render where
  render { label, className, items, alignment } =
    dropdownButton
      { label
      , className: "lumi-dropdown-menu " <> className
      , alignment: alignment
      , onOpen: pure unit
      , content:
          let
            fromItems xs =
              column_ $ xs <#> \item ->
                Link.link Link.defaults
                  { className = pure "lumi-dropdown-menu-item"
                  , text = p_ item.label
                  , navigate = pure $ item.action
                  }
          in
            fragment [ intercalate divider_ (map fromItems items) ]
      }

dropdownButtonDefaults :: DropdownButtonProps
dropdownButtonDefaults =
  { label: ""
  , content: mempty
  , className: ""
  , onOpen: pure unit
  , alignment: toNullable Nothing
  }

dropdownMenuDefaults :: DropdownMenuProps
dropdownMenuDefaults =
  { label: ""
  , className: ""
  , alignment: toNullable Nothing
  , items: []
  }

type DropdownIconProps =
 { icon :: JSX
 , content :: JSX
 , onOpen :: Effect Unit
 , alignment :: Nullable String
 }

dropdownIconDefaults :: DropdownIconProps
dropdownIconDefaults =
  { icon: Text.text
      { children: [ R.text "···" ]
      , className: Nullable.null
      , color: Nullable.notNull colorNames.black1
      , style: R.css { fontSize: "18px", fontWeight: "bold" }
      , tag: "lumi-body"
      , testId: Nullable.null
      }
  , content: mempty
  , onOpen: pure unit
  , alignment: toNullable Nothing
  }

dropdownIcon :: DropdownIconProps -> JSX
dropdownIcon props =
  dropdownButton
    { label: (unsafeCoerce :: JSX -> String) props.icon
    , content: props.content
    , className: "lumi-dropdown-icon"
    , onOpen: props.onOpen
    , alignment: props.alignment
    }

foreign import checkIsEventTargetInTree :: EffectFn2 Node Event Boolean

getAbsolutePosition :: HTML.HTMLElement -> Effect { bottom :: Number, left :: Number, right :: Number }
getAbsolutePosition el = do
  window <- HTML.window
  { bottom, left, right } <- HTML.getBoundingClientRect el
  scrollX <- map toNumber (HTML.scrollX window)
  scrollY <- map toNumber (HTML.scrollY window)
  innerWidth <- map toNumber (HTML.innerWidth window)
  pure
    { bottom: bottom + scrollY
    , left: left + scrollX
    , right: innerWidth - right + scrollX
    }

styles :: JSS
styles = jss
  { "@global":
      { "lumi-dropdown-button":
          { display: "inline-block"
          , position: "relative"
          }
      , "lumi-dropdown-button:not(.lumi-dropdown-icon)":
          { "& > react-basic-ref > button.lumi":
              { backgroundImage: "url(\"data:image/svg+xml;charset=utf8,%3C?xml version='1.0' encoding='UTF-8'?%3E%3Csvg width='11px' height='5px' viewBox='0 0 11 5' version='1.1' xmlns='http://www.w3.org/2000/svg' xmlns:xlink='http://www.w3.org/1999/xlink'%3E%3C!-- Generator: Sketch 49.1 (51147) - http://www.bohemiancoding.com/sketch --%3E%3Ctitle%3ESlice 1%3C/title%3E%3Cdesc%3ECreated with Sketch.%3C/desc%3E%3Cdefs%3E%3Cpath d='M5.417,3.519 C5.797,3.187 6.307,2.733 6.912,2.185 L6.974,2.129 C7.70584693,1.46645784 8.43485361,0.800785071 9.161,0.132 C9.29247374,0.0108869595 9.47857352,-0.0308857001 9.64919735,0.0224173781 C9.81982119,0.0757204563 9.94904726,0.216001266 9.98819736,0.390417384 C10.0273475,0.564833501 9.97047374,0.746886966 9.839,0.868 C9.11068658,1.53896706 8.37934578,2.20664054 7.645,2.871 L7.583,2.927 C5.376,4.922 5.287,5 5,5 C4.713,5 4.624,4.922 2.417,2.927 L2.355,2.871 C1.62081041,2.20646869 0.889470368,1.5387959 0.161,0.868 C-0.0422407879,0.68077547 -0.0552245302,0.364240788 0.132,0.161 C0.31922453,-0.0422407879 0.635759212,-0.0552245302 0.839,0.132 C1.5649901,0.800955473 2.29399753,1.46662893 3.026,2.129 L3.088,2.185 C3.693,2.733 4.203,3.187 4.583,3.519 C4.75,3.665 4.89,3.785 5,3.877 C5.11,3.785 5.25,3.665 5.417,3.519 Z' id='path-1'%3E%3C/path%3E%3C/defs%3E%3Cg id='Page-1' stroke='none' stroke-width='1' fill='none' fill-rule='evenodd'%3E%3Cg id='arrow-down'%3E%3Cg id='a-link' fill='%2342413F' fill-rule='nonzero'%3E%3Cpath d='M5.417,3.519 C5.797,3.187 6.307,2.733 6.912,2.185 L6.974,2.129 C7.70584693,1.46645784 8.43485361,0.800785071 9.161,0.132 C9.29247374,0.0108869595 9.47857352,-0.0308857001 9.64919735,0.0224173781 C9.81982119,0.0757204563 9.94904726,0.216001266 9.98819736,0.390417384 C10.0273475,0.564833501 9.97047374,0.746886966 9.839,0.868 C9.11068658,1.53896706 8.37934578,2.20664054 7.645,2.871 L7.583,2.927 C5.376,4.922 5.287,5 5,5 C4.713,5 4.624,4.922 2.417,2.927 L2.355,2.871 C1.62081041,2.20646869 0.889470368,1.5387959 0.161,0.868 C-0.0422407879,0.68077547 -0.0552245302,0.364240788 0.132,0.161 C0.31922453,-0.0422407879 0.635759212,-0.0552245302 0.839,0.132 C1.5649901,0.800955473 2.29399753,1.46662893 3.026,2.129 L3.088,2.185 C3.693,2.733 4.203,3.187 4.583,3.519 C4.75,3.665 4.89,3.785 5,3.877 C5.11,3.785 5.25,3.665 5.417,3.519 Z' id='a'%3E%3C/path%3E%3C/g%3E%3Cg id='Clipped'%3E%3Cmask id='mask-2' fill='white'%3E%3Cuse xlink:href='%23path-1'%3E%3C/use%3E%3C/mask%3E%3Cg id='a'%3E%3C/g%3E%3Cg id='Group' mask='url(%23mask-2)' fill='%23292827' fill-rule='nonzero'%3E%3Cg transform='translate(-5.000000, -8.000000)' id='Shape'%3E%3Cpolygon points='0 0 20 0 20 20 0 20'%3E%3C/polygon%3E%3C/g%3E%3C/g%3E%3C/g%3E%3C/g%3E%3C/g%3E%3C/svg%3E\")"
              , backgroundRepeat: "no-repeat"
              , backgroundPositionY: "center"
              , backgroundPositionX: "calc(100% - 14px)"
              -- text padding-right + arrow width + arrow padding-right
              , paddingRight: "calc(10px + 10px + 14px)"
              , "&[data-color=\"secondary\"]":
                  { "&:hover": { color: cssStringHSLA colors.black }
                  , "&:focus": { borderColor: cssStringHSLA colors.primary }
                  }
              }
          }
      , "lumi-dropdown-button.lumi-dropdown-icon":
          { "& > react-basic-ref > button.lumi":
              { border: "none"
              , padding: "0"
              , minWidth: "0"
              }
          }

      , "lumi-dropdown-button-content":
          { cursor: "default"
          , position: "absolute"
          , top: "calc(100% + 4px)"
          , left: "-1px"
          , border: [ "1px", "solid", cssStringHSLA colors.black4 ]
          , borderRadius: "3px"
          , boxShadow: "0 2px 4px 0 rgba(12, 0, 51, 0.06)"
          , background: cssStringHSLA colors.white
          , zIndex: ziDropdownButton

          , "& a.lumi.lumi-dropdown-menu-item":
              { boxSizing: "border-box"
              , width: "100%"
              , display: "inline-block"
              , textAlign: "left"
              , paddingLeft: "12px"
              , paddingRight: "12px"
              , whiteSpace: "nowrap"
              , color: cssStringHSLA colors.black
              , textDecoration: "none"
              , "&:hover":
                  { backgroundColor: cssStringHSLA colors.black7
                  }
              }
          }
      }
  }
