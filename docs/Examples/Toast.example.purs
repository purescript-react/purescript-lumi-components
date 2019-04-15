module Lumi.Components.Examples.Toast where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Random (random)
import Lumi.Components.Button (button, primary)
import Lumi.Components.ButtonGroup (buttonGroup)
import Lumi.Components.Column (column_)
import Lumi.Components.Input (input, text_)
import Lumi.Components.Text (p, text)
import Lumi.Components.Toast (toast, toastBubble, toastManager)
import Lumi.Components.Example (example)
import React.Basic (Component, JSX, createComponent, keyed, make)
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, handler_)

component :: Component Unit
component = createComponent "ToastExample"

docs :: JSX
docs = unit # make component { initialState, render }
  where
    initialState =
      { message: Nothing
      , pendingMessage: "This is an example message"
      , key: ""
      }

    render self =
      column_
        [ example $
            toastBubble { message: self.state.pendingMessage }

        , text p
            { children =
                [ R.text "Click here to trigger a toast notification: "
                , buttonGroup
                    { children:
                        [ input text_
                            { value = self.state.pendingMessage
                            , onChange = handler targetValue \targetValue -> do
                                self.setState _ { pendingMessage = fromMaybe "" targetValue }
                            }
                        , button primary
                            { onPress = handler_ do
                                key <- show <$> random
                                self.setState _ { key = key, message = Just self.state.pendingMessage }
                            , title = "Notify"
                            }
                        ]
                    , joined: true
                    , style: R.css {}
                    }
                , toastManager
                , keyed self.state.key $
                    toast { message: self.state.message }
                ]
            }
        ]
