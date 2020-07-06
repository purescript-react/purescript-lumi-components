module Lumi.Components.Toast
  ( ToastBubbleProps
  , ToastProps
  , toast
  , toastManager
  , enqueueToast
  , toastBubble
  , styles
  ) where

import Prelude

import Color (cssStringHSLA)
import Data.Array (drop, head, snoc)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (warn)
import Effect.Ref (Ref, modify, new, read, write)
import Effect.Unsafe (unsafePerformEffect)
import JSS (JSS, jss)
import Lumi.Components.Color (colors)
import Lumi.Components.ZIndex (ziToast)
import React.Basic.Classic (Component, JSX, createComponent, element, empty, make)
import React.Basic.DOM (createPortal)
import React.Basic.DOM as R
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

type ToastBubbleProps =
  { message :: String
  }

toastBubble :: ToastBubbleProps -> JSX
toastBubble = \props -> toastBubbleElement { children: props.message }
  where
    toastBubbleElement = element $ unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-toast-bubble"

-- | Component creation is an unsafe module-level
-- | effect, so we need to create this Ref unsafely
-- | as well.
toastQueue :: Ref (Array { message :: String, duration :: Milliseconds })
toastQueue = unsafePerformEffect do new []

enqueueToast :: String -> Effect Unit
enqueueToast message = void do
  modify (_ `snoc` { message, duration: toastDuration }) toastQueue

toastDuration :: Milliseconds
toastDuration = Milliseconds 4000.0

type ToastProps =
  { message :: Maybe String
  }

toastComponent :: Component ToastProps
toastComponent = createComponent "Toast"

toast :: ToastProps -> JSX
toast = make toastComponent
  { initialState: unit
  , shouldUpdate
  , didMount: tryEnqueueToast
  , didUpdate: \self _ -> tryEnqueueToast self
  , render: const empty
  }
  where
    shouldUpdate self { nextProps } =
      self.props /= nextProps

    tryEnqueueToast self = do
      traverse_ enqueueToast self.props.message

data ToastStatus
  = Empty
  | Visible
  | Expired

transitionDuration :: Milliseconds
transitionDuration = Milliseconds 150.0 -- must match CSS value

toastManagerComponent :: Component Unit
toastManagerComponent = createComponent "ToastManager"

toastManager :: JSX
toastManager = unit # make toastManagerComponent
  { initialState
  , didMount
  , render
  }
  where
    initialState =
      { message: Nothing
      , status: Empty
      , toastRoot: Nothing
      }

    didMount self = do
      toastRoot <- getElementById "toast-root" <<< toNonElementParentNode =<< document =<< window
      case toastRoot of
        Nothing ->
          warn "Warning: `#toast-root` element not found, no messages will be displayed"

        Just tr ->
          initializeQueue self tr

    initializeQueue self toastRoot = do
      self.setState \state -> state { toastRoot = Just toastRoot }
      readQueue self

    readQueue self = launchAff_ do
      q <- liftEffect $ read toastQueue
      liftEffect $ write (drop 1 q) toastQueue
      delay transitionDuration
      case head q of
        Nothing -> do
          delay $ Milliseconds 500.0
          liftEffect $ readQueue self

        Just m -> do
          liftEffect $ newMessage self m
          delay transitionDuration
          liftEffect $ self.setState setVisible
          delay m.duration
          liftEffect $ self.setState setExpired
          delay transitionDuration
          liftEffect $ self.setState reset
          delay transitionDuration
          liftEffect $ readQueue self

    newMessage self m = do
      unless (Just m == self.state.message) do
        self.setState \state -> state { message = Just m, status = Empty }

    setVisible = _ { status = Visible }

    setExpired = _ { status = Expired }

    reset = _ { message = Nothing, status = Empty }

    render { state } =
      case { toastRoot: _, message: _ } <$> state.toastRoot <*> state.message of
        Nothing ->
          empty

        Just { toastRoot, message: { message } } ->
          let
            jsx = toastAnchorElement
              { children:
                  toastWrapperElement
                    { children:
                        [ toastBubble { message }
                        ]
                    , class: case state.status of
                        Empty -> "empty"
                        Visible -> "visible"
                        Expired -> "expired"
                    }
              }
            in
              createPortal jsx toastRoot

    toastAnchorElement = element $ unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-toast-anchor"
    toastWrapperElement = element $ unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-toast-wrapper"

styles :: JSS
styles = jss
  { "@global":
      { "lumi-toast-bubble":
          { boxSizing: "border-box"
          , display: "flex"
          , padding: "16px 24px"
          , color: cssStringHSLA colors.white
          , backgroundColor: cssStringHSLA colors.black
          , borderRadius: "3px"
          , fontSize: "15px"
          , lineHeight: "24px"
          }

      , "lumi-toast-anchor":
          { boxSizing: "border-box"

          , position: "fixed"

          , display: "flex"
          , justifyContent: "center"
          , bottom: "32px"
          , padding: "0 32px"
          , width: "100%"

          , "& > lumi-toast-wrapper":
              { boxSizing: "border-box"
              , display: "flex"
              , zIndex: ziToast
              , transitionProperty: "opacity, transform"
              , transitionTimingFunction: "ease-in-out"
              , transitionDuration

              , "&.empty":
                  { opacity: "0"
                  , transform: "translateY(30px)"
                  }
              , "&.visible":
                  { opacity: "1"
                  , transform: "translateY(0)"
                  }
              , "&.expired":
                  { opacity: "0"
                  , transform: "translateY(0)"
                  }
              }
        }


      }
  }
