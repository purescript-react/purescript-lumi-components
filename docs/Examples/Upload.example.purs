module Lumi.Components.Examples.Upload where

import Prelude

import Control.Coroutine.Aff (close, emit, produceAff)
import Data.Array (head)
import Data.Foldable (foldMap)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Random (randomRange)
import Effect.Uncurried (runEffectFn2)
import Lumi.Components.Button (button, primary) as Button
import Lumi.Components.Column (column, column_)
import Lumi.Components.Input as Input
import Lumi.Components.LabeledField (labeledField, RequiredField(..))
import Lumi.Components.Text (h2_)
import Lumi.Components.Upload (FileId(..), FileName(..), UploadVariant(..), defaults, upload)
import Lumi.Components.Example (example)
import React.Basic (Component, JSX, createComponent, make)
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetChecked)
import React.Basic.Events (handler, handler_)
import Unsafe.Coerce (unsafeCoerce)
import Web.File.File as File

component :: Component Unit
component = createComponent "UploadExample"

data Action
  = NoOp
  | SetReadonly Boolean
  | ImageEx (Array FileId)
  | FileEx (Array FileId)
  | AvatarEx (Maybe FileId)
  | StartUpload
  | InitializeUploadBuffer (AVar Unit)

docs :: JSX
docs = unit # make component { initialState, didMount, render }
  where
    initialState =
      { images: []
      , files: []
      , avatar: Nothing
      , readonly: false
      , startUpload: Nothing
      }

    didMount self = launchAff_ do send self <<< InitializeUploadBuffer <$> AVar.empty

    send self = case _ of
      NoOp ->
        pure unit

      SetReadonly readonly ->
        self.setState _ { readonly = readonly }

      ImageEx value ->
        self.setStateThen
          (_ { images = value })
          $ debug "value:" value

      FileEx value ->
        self.setStateThen
          (_ { files = value })
          $ debug "value:" value

      AvatarEx value ->
        self.setStateThen
          (_ { avatar = value })
          $ debug "value:" value

      StartUpload -> do
        launchAff_ do
          maybe
            (InitializeUploadBuffer <$> AVar.new unit)
            (map (const NoOp) <<< AVar.tryPut unit)
            self.state.startUpload

      InitializeUploadBuffer avar ->
        self.setState _ { startUpload = Just avar }

    render self =
      column_
        [ column
            { style: R.css { maxWidth: 500, padding: "20px 0" }
            , children:
                [ labeledField
                    { label: R.text "Readonly"
                    , value: Input.input Input.switch
                        { checked = if self.state.readonly then Input.On else Input.Off
                        , onChange = handler targetChecked $ send self <<< SetReadonly <<< fromMaybe false
                        }
                    , validationError: Nothing
                    , required: Neither
                    , forceTopLabel: false
                    , style: R.css {}
                    }
                ]
            }

        , h2_ "Images"
        , example $
            column
              { style: R.css { alignSelf: "stretch" }
              , children:
                  [ upload defaults
                      { value = self.state.images
                      , variant = Images
                      , onChange = send self <<< ImageEx
                      , readonly = self.state.readonly
                      , backend =
                          { fetch: \id@(FileId name) ->
                              pure { id, name: FileName name, previewUri: Nothing }
                          , upload: \file -> produceAff \emitter ->
                              uploadWithRandomPauses emitter file
                          }
                      }
                  ]
              }

        , h2_ "Files"
        , example $
            column
              { style: R.css { alignSelf: "stretch" }
              , children:
                  [ upload defaults
                      { value = self.state.files
                      , variant = Files
                      , onChange = send self <<< FileEx
                      , readonly = self.state.readonly
                      , backend =
                          { fetch: \id@(FileId name) ->
                              pure { id, name: FileName name, previewUri: Nothing }
                          , upload: \file -> produceAff \emitter -> do
                              foldMap AVar.read self.state.startUpload
                              void $ foldMap AVar.tryTake self.state.startUpload
                              uploadWithRandomPauses emitter file
                          }
                      }
                  , Button.button Button.primary
                      { title = "Upload"
                      , onPress = handler_ $ send self StartUpload
                      }
                  ]
              }

        , h2_ "Avatar"
        , example $
            column
              { style: R.css { alignSelf: "stretch" }
              , children:
                  [ upload defaults
                      { value = maybe [] pure self.state.avatar
                      , variant = Avatar
                      , onChange = send self <<< AvatarEx <<< head
                      , readonly = self.state.readonly
                      , backend =
                          { fetch: \id@(FileId name) -> do
                              pure { id, name: FileName name, previewUri: Nothing }
                          , upload: \file -> produceAff \emitter ->
                              uploadWithRandomPauses emitter file
                          }
                      }
                  ]
              }
        , h2_ "Logo"
        , example $
            column
              { style: R.css { alignSelf: "stretch" }
              , children:
                  [ upload defaults
                      { value = maybe [] pure self.state.avatar
                      , variant = Logo
                      , onChange = send self <<< AvatarEx <<< head
                      , readonly = self.state.readonly
                      , backend =
                          { fetch: \id@(FileId name) -> do
                              pure { id, name: FileName name, previewUri: Nothing }
                          , upload: \file -> produceAff \emitter ->
                              uploadWithRandomPauses emitter file
                          }
                      }
                  ]
              }
        ]

    randomPause = do
      interval <- liftEffect $ randomRange 100.0 700.0
      delay $ Milliseconds interval

    uploadWithRandomPauses emitter file = do
      let
        totalBytes = Int.round $ File.size file
        progress = { totalBytes, uploadedBytes: 0 }
      randomPause
      emit emitter progress
      randomPause
      emit emitter progress { uploadedBytes = totalBytes / 8 }
      randomPause
      emit emitter progress { uploadedBytes = totalBytes / 2 }
      randomPause
      emit emitter progress { uploadedBytes = totalBytes }

      close emitter $ pure $ FileId $ File.name file

debug :: forall a. String -> a -> Effect Unit
debug = runEffectFn2 (unsafeCoerce log)
