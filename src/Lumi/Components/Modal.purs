module Lumi.Components.Modal where

import Prelude

import Color (cssStringHSLA, hsla, toHSLA)
import Data.Foldable (foldMap, for_)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, mkEffectFn1, runEffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import JSS (JSS, jss)
import Lumi.Components.Button as Button
import Lumi.Components.Color (colors)
import Lumi.Components.Icon (IconType(..), icon_)
import Lumi.Components.Link as Link
import Lumi.Components.Size (Size(..))
import Lumi.Components.Text (sectionHeader_, subsectionHeader, text)
import Lumi.Components.ZIndex (ziModal)
import Prim.Row (class Nub, class Union)
import React.Basic.Classic (Component, JSX, ReactComponent, createComponent, element, empty, make, makeStateless, toReactComponent)
import React.Basic.DOM as R
import React.Basic.DOM.Components.GlobalEvents (windowEvent)
import React.Basic.DOM.Events (currentTarget, stopPropagation, target)
import React.Basic.Events as Events
import Record (merge)
import Web.Event.Event (Event, EventType(..))
import Web.Event.Event as E
import Web.UIEvent.KeyboardEvent (fromEvent, key)


foreign import toggleBodyClass :: EffectFn2 String Boolean Unit

type ModalLinkProps value props =
  { label :: JSX
  , title :: JSX
  , component :: { value :: value
                 , onChange :: value -> Effect Unit
                 | props
                 }
              -> JSX
  , actionButtonTitle :: String
  , value :: value
  , onChange :: value -> Effect Unit
  , componentProps :: { | props }
  , style :: R.CSS
  }

modalLinkComponent :: forall value props. Component (ModalLinkProps value props)
modalLinkComponent = createComponent "ModalLink"

-- | This component displays a link which displays an editor component in
-- | a modal popup.
modalLink
  :: forall value props
   . Union
       props
       ( value :: value
       , onChange :: value -> Effect Unit
       )
       ( value :: value
       , onChange :: value -> Effect Unit
       | props
       )
  => Nub
       ( value :: value
       , onChange :: value -> Effect Unit
       | props
       )
       ( value :: value
       , onChange :: value -> Effect Unit
       | props
       )
  => ModalLinkProps value props
  -> JSX
modalLink = make modalLinkComponent { initialState, didMount, render }
  where
    initialState =
      { modalOpen: false
      , value: Nothing
      }

    didMount self = do
      self.setState _ { value = Just self.props.value }

    render self@{ props, state } =
      lumiModalLink
        { style: props.style
        , children:
          [ Link.link Link.defaults
              { className = pure "lumi"
              , navigate = pure $ self.setState _ { modalOpen = true }
              , text = props.label
              }
          , case state.value of
              Nothing -> empty
              Just value_ ->
                modal
                  { modalOpen: state.modalOpen
                  , internalBorders: false
                  , title: props.title
                  , onActionButtonClick: toNullable $ Just do
                      self.setState _ { modalOpen = false }
                      props.onChange value_
                  , actionButtonTitle: props.actionButtonTitle
                  , actionButtonState: Button.Enabled
                  , size: Medium
                  , variant: ""
                  , closeButton: true
                  , onRequestClose: self.setState _ { modalOpen = false }
                  , children:
                      props.component $
                        merge
                          props.componentProps
                          ( { value: value_
                            , onChange: \value -> self.setState _ { value = Just value }
                            } :: { value :: value
                                 , onChange :: value -> Effect Unit
                                 }
                          )
                  }
          ]
        }

    lumiModalLink = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-modal-link")

type CommonProps rest =
  { modalOpen :: Boolean
  , onActionButtonClick :: Nullable (Effect Unit)
  , actionButtonTitle :: String
  , actionButtonState :: Button.ButtonState
  , size :: Size
  , variant :: String
  , children :: JSX
  , internalBorders :: Boolean
  , closeButton :: Boolean
  , title :: JSX
  | rest
  }

type ModalProps = CommonProps (onRequestClose :: Effect Unit)

foreign import modalBuilder :: ReactComponent ModalPortalProps -> ReactComponent ModalProps

modal_ :: ReactComponent ModalProps
modal_ = modalBuilder modalPortal

modal :: ModalProps -> JSX
modal = element modal_

modalTitle :: String -> JSX
modalTitle s = sectionHeader_ s

type DialogProps =
  { modalOpen :: Boolean
  , onRequestClose :: Effect Unit
  , onActionButtonClick :: Nullable (Effect Unit)
  , actionButtonTitle :: String
  , size :: Size
  , children :: JSX
  }

dialogComponent :: Component DialogProps
dialogComponent = createComponent "Dialog"

dialog :: DialogProps -> JSX
dialog = makeStateless dialogComponent render
  where
    render props =
      modal
        { modalOpen: props.modalOpen
        , closeButton: true
        , onRequestClose: props.onRequestClose
        , onActionButtonClick: props.onActionButtonClick
        , actionButtonTitle: props.actionButtonTitle
        , actionButtonState: Button.Enabled
        , size: props.size
        , variant: "dialog"
        , children:
            text subsectionHeader
              { style = R.css { fontWeight: 400 }
              , children = [ props.children ]
              }
        , internalBorders: false
        , title: modalTitle ""
        }

type ErrorModalProps =
  { modalOpen :: Boolean
  , onActionButtonClick :: Nullable (Effect Unit)
  , actionButtonTitle :: String
  , children :: JSX
  , onRequestClose :: Effect Unit
  }

errorModalComponent :: Component ErrorModalProps
errorModalComponent = createComponent "Error modal"

errorModal :: ErrorModalProps -> JSX
errorModal = makeStateless errorModalComponent render
  where
    render props =
      modal
        { modalOpen: props.modalOpen
        , closeButton: true
        , onRequestClose: props.onRequestClose
        , onActionButtonClick: props.onActionButtonClick
        , actionButtonTitle: props.actionButtonTitle
        , actionButtonState: Button.Enabled
        , size: Small
        , variant: ""
        , children: props.children
        , internalBorders: false
        , title: modalTitle "Error"
        }

type ModalPortalProps = CommonProps (requestClose :: Effect Unit)

modalPortalComponent :: Component ModalPortalProps
modalPortalComponent = createComponent "ModalPortal"

modalPortal :: ReactComponent ModalPortalProps
modalPortal = toReactComponent identity modalPortalComponent
  { didMount: checkBodyClass
  , didUpdate: \self _ -> checkBodyClass self
  , willUnmount: const closeModal
  , render
  }
  where
    checkBodyClass self =
      if self.props.modalOpen
        then openModal
        else closeModal

    openModal = runEffectFn2 toggleBodyClass "react-modal-open" true

    closeModal = runEffectFn2 toggleBodyClass "react-modal-open" false

    render { props, state } =
      if not props.modalOpen
      then empty
      else windowEvent
        { eventType: EventType "keydown"
        , options: { capture: false, once: false, passive: false }
        , handler: \e -> do
            keydownEventHandler e
        }
        $ lumiModalContainer
          { onClick: Events.handler (Events.merge { target, currentTarget })
              \{ target, currentTarget } -> do
                  props.requestClose
                  closeModal
              , children:
                  lumiModalOverlay
                    { "data-variant": props.variant
                    , children:
                        lumiModal
                          { "data-size": show props.size
                          , onClick: Events.handler stopPropagation \_ -> pure unit
                          , children:
                              [ lumiModalHeader
                                  { children:
                                      [ if props.variant == "dialog"
                                          then empty
                                          else lumiModalClose
                                            { children: icon_ Remove
                                            , onClick: mkEffectFn1 \_ -> do
                                                props.requestClose
                                                closeModal
                                            }
                                        , props.title
                                      ]
                                  }
                              , lumiModalContent
                                  { children: props.children
                                  , "data-internal-borders": props.internalBorders
                                  }
                              , lumiModalFooter
                                  { children:
                                      [ guard props.closeButton $
                                          Button.button Button.secondary
                                            { title =
                                                case toMaybe props.onActionButtonClick of
                                                  Nothing -> "Close"
                                                  Just _ -> "Cancel"
                                            , onPress = mkEffectFn1 \_ -> do
                                                props.requestClose
                                                closeModal
                                            }
                                      , toMaybe props.onActionButtonClick # foldMap \actionFn ->
                                          Button.button Button.primary
                                            { title = props.actionButtonTitle
                                            , buttonState = props.actionButtonState
                                            , onPress = mkEffectFn1 \_ -> actionFn
                                            , style = R.css { marginLeft: "12px" }
                                            }
                                      ]
                                  }
                              ]
                          }
                    }
          }
        where
          keydownEventHandler :: Event -> Effect Unit
          keydownEventHandler e = do
            let mKey = eventKey e
            for_ mKey case _ of
              "Escape" -> do
                E.preventDefault e
                E.stopPropagation e
                props.requestClose
                closeModal
              _ -> pure unit

    lumiModalContainer = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-modal-container")
    lumiModalOverlay = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-modal-overlay")
    lumiModal = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-modal")
    lumiModalClose = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-modal-close")
    lumiModalHeader = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-modal-header")
    lumiModalContent = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-modal-content")
    lumiModalFooter = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-modal-footer")

styles :: JSS
styles = jss
  { "@global":
      { -- locks normal content scrolling while the modal is open
        "body.react-modal-open":
          { overflow: "hidden"
          }

          -- fills screen with faded backdrop and positions the modal
      , "lumi-modal-container":
          { boxSizing: "border-box"
          , position: "fixed"
          , top: "0"
          , left: "0"
          , bottom: "0"
          , right: "0"
          , zIndex: ziModal
          , display: "flex"
          , flexFlow: "column"
          , alignItems: "center" -- horizontal centering, vertical centering is handled by lumi-modal-overlay > lumi-modal
          , overflowY: "auto"
          , background: cssStringHSLA (alpha 0.25 colors.black)
          , "WebkitOverflowScrolling": "touch" -- smooth momentum scrolling (iOS)

            -- invisible modal wrapper adds padding and vertical centering
          , "& lumi-modal-overlay":
              { boxSizing: "border-box"
              , display: "flex"
              , flexFlow: "column"
              , padding: "64px 0"
              , "@media (max-width: 860px)": { padding: "0" }
              , flex: "0 0 auto" -- expand to encompass large modals with scrolling
              , minHeight: "100%" -- vertical centering of small modals

                -- the modal itself
              , "& lumi-modal":
                  { boxSizing: "border-box"
                  , margin: "auto 0" -- vertical centering of small modals
                  , display: "flex"
                  , flexFlow: "column"
                  , position: "relative"
                  , padding: "24px 0"
                  , "&[data-size=\"small\"]":
                      { width: "400px"
                      }
                  , "&[data-size=\"medium\"]":
                      { width: "504px"
                      }
                  , "&[data-size=\"large\"]":
                      { width: "600px"
                      }
                  , "&[data-size=\"extra-large\"]":
                      { width: "848px"
                      }
                  , "&[data-size=\"extra-extra-large\"]":
                      { width: "1048px"
                      }
                  , maxWidth: "calc(100vw - (24px * 2))"
                  , background: "rgba(255, 255, 255, 1)"
                  , borderRadius: "4px"

                  , "& lumi-modal-close":
                      { boxSizing: "border-box"
                      , position: "absolute"
                      , top: "22px"
                      , right: "24px"
                      , display: "flex"
                      , flexFlow: "column"
                      , fontSize: "14px"
                      , color: cssStringHSLA colors.black2
                      , cursor: "pointer"
                      , "@media (max-width: 860px)":
                          { "& lumi-font-icon":
                              { fontSize: "16px"
                              , lineHeight: "24px"
                              }
                          }
                      }
                  , "& lumi-modal-header":
                      { boxSizing: "border-box"
                      , flex: "0 0 auto"
                      , display: "flex"
                      , flexFlow: "column"
                      , paddingLeft: "24px"
                      , paddingRight: "48px" -- avoid colliding with the close "X"
                      , "@media (max-width: 860px)":
                          { paddingLeft: "16px"
                          }
                      , padding: "0 48px 0 24px"
                      , "& > lumi-section-header":
                          { paddingTop: "0"
                          , marginTop: "0"
                          }
                      , lineHeight: "1"
                      }
                  , "& lumi-modal-content":
                      { boxSizing: "border-box"
                      , margin: "24px 0"
                      , flex: "1 0 auto"
                      , display: "flex"
                      , flexFlow: "column"
                      , paddingLeft: "24px"
                      , paddingRight: "24px"
                      , "@media (max-width: 860px)":
                          { paddingLeft: "16px"
                          , paddingRight: "16px"
                          }
                      , "&[data-internal-borders=\"true\"]":
                          { borderTop: [ "1px", "solid", cssStringHSLA colors.black4 ]
                          , borderBottom: [ "1px", "solid", cssStringHSLA colors.black4 ]
                          , paddingTop: "8px"
                          , paddingBottom: "8px"
                          }
                      }
                  , "& lumi-modal-footer":
                      { boxSizing: "border-box"
                      , flex: "0 0 auto"
                      , display: "flex"
                      , flexFlow: "row"
                      , justifyContent: "flex-end"
                      , paddingLeft: "24px"
                      , paddingRight: "24px"
                      , "@media (max-width: 860px)":
                          { paddingLeft: "16px"
                          , paddingRight: "16px"
                          , "& button.lumi":
                              { fontSize: "16px"
                              , lineHeight: "initial"
                              , padding: "10px 21px 10px"
                              }
                          }
                      }
                  }

                -- non-dialog modals fill the screen on mobile devices
              , "&:not([data-variant=\"dialog\"])":
                  { "@media (max-width: 860px)":
                      { maxHeight: "100%"
                      , "& lumi-modal":
                          { position: "fixed"
                          , top: "0"
                          , right: "0"
                          , bottom: "0"
                          , left: "0"
                          , height: "100%"
                          , width: "100vw"
                          , maxWidth: "100vw"
                          , borderRadius: "0"
                          , "&[data-size=\"small\"], &[data-size=\"medium\"], &[data-size=\"large\"]":
                              { width: "100vw"
                              }
                          , "& lumi-modal-content":
                              { flex: "1 1 auto"
                              , overflowY: "auto"
                              }
                          }
                      }
                  }

              , "&[data-variant=\"dialog\"] lumi-modal lumi-modal-content":
                  { margin: "0 0 16px"
                  }

                -- constrain total height while making the intenal content area flexible and scrollable
              , "&[data-variant=\"internal-scrolling\"]":
                  { maxHeight: "100%"
                  , "& lumi-modal":
                      { height: "100%"
                      }
                  , "& lumi-modal lumi-modal-content":
                      { flex: "1 1 auto"
                      , overflowY: "auto"
                      }
                  }

              , "&[data-variant=\"top-aligned\"] lumi-modal":
                  { margin: "0"
                  }
              }
          }
      }
  }
  where
    alpha a = toHSLA >>> \{ h, s, l } -> hsla h s l a

eventKey :: Event -> Maybe String
eventKey e = map key (fromEvent e)
