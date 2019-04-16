module Lumi.Components.Modal where

import Prelude

import Color (cssStringHSLA, hsla, toHSLA)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.String (null)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, mkEffectFn1, runEffectFn2)
import JSS (JSS, jss)
import Lumi.Components.Button as Button
import Lumi.Components.Color (colors)
import Lumi.Components.Icon (IconType(..), icon_)
import Lumi.Components.Link as Link
import Lumi.Components.Size (Size(..))
import Lumi.Components.Text (sectionHeader_, subsectionHeader, text)
import React.Basic (Component, JSX, ReactComponent, createComponent, element, empty, make, makeStateless, toReactComponent)
import React.Basic.DOM as R
import React.Basic.DOM.Events (currentTarget, stopPropagation, target)
import React.Basic.Events as Events
import Record (merge)
import Type.Row (class Nub, class Union)

foreign import toggleBodyClass :: EffectFn2 String Boolean Unit

type ModalLinkProps value props =
  { label :: JSX
  , title :: String
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
                  , actionButtonDisabled: false
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

    lumiModalLink = element (R.unsafeCreateDOMComponent "lumi-modal-link")

type CommonProps rest =
  { modalOpen :: Boolean
  , onActionButtonClick :: Nullable (Effect Unit)
  , actionButtonTitle :: String
  , actionButtonDisabled :: Boolean
  , size :: Size
  , variant :: String
  , children :: JSX
  , internalBorders :: Boolean
  , closeButton :: Boolean
  , title :: String
  | rest
  }

type ModalProps = CommonProps (onRequestClose :: Effect Unit)

foreign import modalBuilder :: ReactComponent ModalPortalProps -> ReactComponent ModalProps

modal_ :: ReactComponent ModalProps
modal_ = modalBuilder modalPortal

modal :: ModalProps -> JSX
modal = element modal_

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
        , actionButtonDisabled: false
        , size: props.size
        , variant: "dialog"
        , children:
            text subsectionHeader
              { style = R.css { fontWeight: 400 }
              , children = [ props.children ]
              }
        , internalBorders: false
        , title: ""
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
        , actionButtonDisabled: false
        , size: Small
        , variant: ""
        , children: props.children
        , internalBorders: false
        , title: "Error"
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
      else lumiModalContainer
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
                                      , if not null props.title
                                          then sectionHeader_ props.title
                                          else empty
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
                                          , disabled = props.actionButtonDisabled
                                          , onPress = mkEffectFn1 \_ -> actionFn
                                          , style = R.css { marginLeft: "1.2rem" }
                                          }
                                    ]
                                }
                            ]
                        }
                  }
        }

    lumiModalContainer = element (R.unsafeCreateDOMComponent "lumi-modal-container")
    lumiModalOverlay = element (R.unsafeCreateDOMComponent "lumi-modal-overlay")
    lumiModal = element (R.unsafeCreateDOMComponent "lumi-modal")
    lumiModalClose = element (R.unsafeCreateDOMComponent "lumi-modal-close")
    lumiModalHeader = element (R.unsafeCreateDOMComponent "lumi-modal-header")
    lumiModalContent = element (R.unsafeCreateDOMComponent "lumi-modal-content")
    lumiModalFooter = element (R.unsafeCreateDOMComponent "lumi-modal-footer")

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
          , zIndex: "10000000"
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
              , padding: "6.4rem 0"
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
                  , padding: "2.4rem 0"
                  , "&[data-size=\"small\"]":
                      { width: "40rem"
                      }
                  , "&[data-size=\"medium\"]":
                      { width: "50.4rem"
                      }
                  , "&[data-size=\"large\"]":
                      { width: "60rem"
                      }
                  , "&[data-size=\"extra-large\"]":
                      { width: "84.8rem"
                      }
                  , maxWidth: "calc(100vw - (2.4rem * 2))"
                  , background: "rgba(255, 255, 255, 1)"
                  , borderRadius: "0.4rem"

                  , "& lumi-modal-close":
                      { boxSizing: "border-box"
                      , position: "absolute"
                      , top: "2.2rem"
                      , right: "2.4rem"
                      , display: "flex"
                      , flexFlow: "column"
                      , fontSize: "1.4rem"
                      , color: cssStringHSLA colors.black2
                      , cursor: "pointer"
                      , "@media (max-width: 860px)":
                          { "& lumi-font-icon":
                              { fontSize: "1.6rem"
                              , lineHeight: "2.4rem"
                              }
                          }
                      }
                  , "& lumi-modal-header":
                      { boxSizing: "border-box"
                      , flex: "0 0 auto"
                      , display: "flex"
                      , flexFlow: "column"
                      , paddingLeft: "2.4rem"
                      , paddingRight: "4.8rem" -- avoid colliding with the close "X"
                      , "@media (max-width: 860px)":
                          { paddingLeft: "1.6rem"
                          }
                      , padding: "0 4.8rem 0 2.4rem"
                      , "& > lumi-section-header":
                          { paddingTop: "0"
                          , marginTop: "0"
                          }
                      , lineHeight: "1"
                      }
                  , "& lumi-modal-content":
                      { boxSizing: "border-box"
                      , margin: "2.4rem 0"
                      , flex: "1 0 auto"
                      , display: "flex"
                      , flexFlow: "column"
                      , paddingLeft: "2.4rem"
                      , paddingRight: "2.4rem"
                      , "@media (max-width: 860px)":
                          { paddingLeft: "1.6rem"
                          , paddingRight: "1.6rem"
                          }
                      , "&[data-internal-borders=\"true\"]":
                          { borderTop: [ "0.1rem", "solid", cssStringHSLA colors.black4 ]
                          , borderBottom: [ "0.1rem", "solid", cssStringHSLA colors.black4 ]
                          , paddingTop: "0.8rem"
                          , paddingBottom: "0.8rem"
                          }
                      }
                  , "& lumi-modal-footer":
                      { boxSizing: "border-box"
                      , flex: "0 0 auto"
                      , display: "flex"
                      , flexFlow: "row"
                      , justifyContent: "flex-end"
                      , paddingLeft: "2.4rem"
                      , paddingRight: "2.4rem"
                      , "@media (max-width: 860px)":
                          { paddingLeft: "1.6rem"
                          , paddingRight: "1.6rem"
                          , "& button.lumi":
                              { fontSize: "1.6rem"
                              , lineHeight: "2.4rem"
                              , padding: "1rem 2.1rem 1rem"
                              }
                          }
                      }
                  }

                -- non-dialog modals fill the screen on mobile devices
              , "&:not([data-variant=\"dialog\"])":
                  { "& lumi-modal":
                      { "@media (max-width: 860px)":
                          { minHeight: "100vh"
                          , width: "100vw"
                          , maxWidth: "100vw"
                          , "&[data-size=\"small\"], &[data-size=\"medium\"], &[data-size=\"large\"]":
                              { width: "100vw"
                              }
                          }
                      }
                  }

              , "&[data-variant=\"dialog\"] lumi-modal lumi-modal-content":
                  { margin: "0 0 1.6rem"
                  }

                -- constrain total height while making the intenal content area flexible and scrollable
              , "&[data-variant=\"internal-scrolling\"]":
                  { maxHeight: "100%"
                  , "& lumi-modal lumi-modal-content":
                      { flex: "1 1 auto"
                      , overflowY: "auto"
                      }
                  }
              }
          }
      }
  }
  where
    alpha a = toHSLA >>> \{ h, s, l } -> hsla h s l a
