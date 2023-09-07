module Lumi.Components.Upload
  ( FileInfo
  , PendingFileInfo
  , Progress
  , FileId(..)
  , FileName(..)
  , URI(..)
  , UploadProps
  , UploadBackend
  , UploadVariant(..)
  , upload
  , defaults
  , uploadFile
  , styles
  ) where

import Prelude

import Color (cssStringHSLA)
import Control.Alt ((<|>))
import Control.Coroutine (Producer, await, runProcess, ($$))
import Control.Coroutine.Aff (close, emit, produce)
import Control.Monad.Except (lift, runExcept, throwError)
import Control.Monad.Rec.Class (forever)
import Control.Monad.ST as ST
import Control.Parallel (parTraverse)
import Data.Array as Array
import Data.Array.ST as STArray
import Data.Either (Either(..), either)
import Data.Foldable (foldMap, for_, traverse_)
import Data.Int (toNumber)
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.MediaType (MediaType(..))
import Data.Newtype (class Newtype, un)
import Data.Nullable (Nullable, toNullable)
import Data.String as String
import Data.Traversable (for, sequence_)
import Effect (Effect)
import Effect.Aff (Aff, Error, launchAff_, runAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (errorShow)
import Effect.Exception (try)
import Effect.Uncurried (EffectFn1, EffectFn5, mkEffectFn1, runEffectFn1, runEffectFn5)
import Effect.Unsafe (unsafePerformEffect)
import Foreign (readNumber, unsafeToForeign)
import Foreign.Index (readProp)
import JSS (JSS, jss)
import Lumi.Components.Button (button, linkStyle)
import Lumi.Components.Color (colors)
import Lumi.Components.FetchCache as FetchCache
import Lumi.Components.Icon (IconType(..), icon_)
import Lumi.Components.Progress (progressBar, progressCircle, progressDefaults)
import Lumi.Components.Svg (userSvg)
import Lumi.Components.Text (body_, nbsp)
import React.Basic.Classic (Component, JSX, createComponent, element, elementKeyed, empty, fragment, make, readProps)
import React.Basic.DOM as R
import React.Basic.DOM.Components.GlobalEvents (defaultOptions, globalEvents)
import React.Basic.DOM.Events (capture, capture_, nativeEvent, preventDefault, stopPropagation, target)
import React.Basic.Events (EventHandler, handler, handler_)
import Web.Event.Event (Event, EventType(..))
import Web.Event.Event as E
import Web.Event.Internal.Types (EventTarget)
import Web.File.File (File)
import Web.File.File as File
import Web.File.FileList (FileList)
import Web.File.FileList as FileList
import Web.HTML (window)
import Web.HTML.Event.DataTransfer as DataTransfer
import Web.HTML.Event.DragEvent as DragEvent
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.HTML.Window (document)
import Web.HTML.Window as Window

type FileInfo =
  { id :: FileId
  , name :: FileName
  , previewUri :: Maybe URI
  , readonly :: Boolean
  }

type PendingFileInfo =
  { name :: FileName
  , previewUri :: Maybe URI
  , progress :: Maybe Progress
  }

type Progress =
  { totalBytes :: Int
  , uploadedBytes :: Int
  }

newtype FileId = FileId String
derive instance eqFileId :: Eq FileId
derive instance ordFileId :: Ord FileId
derive instance ntFileId :: Newtype FileId _

newtype FileName = FileName String
derive instance eqFileName :: Eq FileName
derive instance ordFileName :: Ord FileName
derive instance ntFileName :: Newtype FileName _

newtype URI = URI String
derive instance eqURI :: Eq URI
derive instance ordURI :: Ord URI
derive instance ntURI :: Newtype URI _

type UploadProps =
  { allowMultiple :: Boolean
  , backend :: UploadBackend
  , disabled :: Boolean
  , name :: String
  , onBlur :: Nullable EventHandler
  , onChange :: Array FileId -> Effect Unit
  , onClick :: FileId -> Maybe (Effect Unit)
  , onFocus :: Nullable EventHandler
  , readonly :: Boolean
  , required :: Boolean
  , testId :: Nullable String
  , value :: Array FileId
  , variant :: UploadVariant
  }

type UploadBackend =
  { fetch :: FileId -> Aff FileInfo
    -- | Allows for running an async effect to determine whether the given file
    -- | should be removed or not. This may be opening a modal window that asks
    -- | for user confirmation or fetching some remote data.
  , remove :: FileId -> Aff Boolean
  , upload :: File -> Producer Progress Aff (Either Error FileId)
  }

data UploadVariant
  = Images
  | Files
  | Avatar
  | Logo

defaults :: UploadProps
defaults =
  { allowMultiple: true
  , backend:
      { fetch: \id@(FileId name) -> pure
          { id
          , name: FileName name
          , previewUri: Nothing
          , readonly: false
          }
      , remove: \_ -> pure true
      , upload: \file -> pure (Right (FileId (File.name file)))
      }
  , disabled: false
  , name: ""
  , onBlur: toNullable $ Just $ handler_ $ pure unit
  , onChange: \_ -> pure unit
  , onClick: \_ -> Nothing
  , onFocus: toNullable $ Just $ handler_ $ pure unit
  , readonly: false
  , required: false
  , testId: toNullable Nothing
  , value: []
  , variant: Files
  }

getPreviewUri :: File -> Effect (Maybe URI)
getPreviewUri file =
  -- Technically this effect is leaving images in memory, but we probably aren't
  -- loading enough files for it to matter. In the future this could be cleaned up on
  -- unmount and deselection/removal.
  -- https://developer.mozilla.org/en-US/docs/Web/API/URL/createObjectURL#Memory_management
  case String.indexOf (String.Pattern "image/") =<< un MediaType <$> File.type_ file of
    Just 0 -> either (const Nothing) Just <$> (try $ URI <$> runEffectFn1 createObjectURL file)
    _      -> pure Nothing

data Action
  = AddNewFiles (Array File)
  | NewPendingFile PendingFileInfo
  | UpdatePendingFile FileName Progress
  | CancelPendingFile PendingFileInfo
  | AddCompleteFiles (Array FileInfo)
  | RemoveCompleteFile FileId

component :: Component UploadProps
component = createComponent "Upload"

upload :: UploadProps -> JSX
upload = make component { initialState, render }
  where
    initialState =
      { pendingFiles: []
      , filePreviewCache: Map.empty
      }

    runUpload self files = do
      let
        allowedFiles =
          let
            mimeFilter =
              map String.Pattern case self.props.variant of
                Images -> Just "image/"
                Files  -> Nothing
                Avatar -> Just "image/"
                Logo -> Just "image/"
          in
            case mimeFilter of
              Nothing ->
                files

              Just mimeFilter' ->
                files # Array.filter \f -> maybe false (eq 0) do
                  type_ <- File.type_ f
                  index <- String.indexOf mimeFilter' $ un MediaType type_
                  pure index

      -- initialize new files in the pending files list to preserve order in UI
      allowedFiles' <-
        for allowedFiles \file -> do
          previewUri <- getPreviewUri file
          let name = FileName $ File.name file
          send self $ NewPendingFile
            { name
            , previewUri
            , progress: Nothing
            }
          pure { file, name, previewUri, readonly: true }
      let
        handlePending name = forever do
          progress <- await
          lift $ liftEffect do
            send self $ UpdatePendingFile name progress

        handleCompleteFiles = send self <<< AddCompleteFiles
      runAff_ (either errorShow handleCompleteFiles) do
        flip parTraverse allowedFiles' \{ file, name, previewUri, readonly } -> do
          let var = self.props.backend.upload file
          completeFile <- runProcess (var $$ handlePending name)
          case completeFile of
            Left error -> throwError error
            Right id -> pure { id, name, previewUri, readonly }

    send self action = do
      unless self.props.readonly do
        case action of
          AddNewFiles files -> do
            runUpload self files

          NewPendingFile pendingFileInfo ->
            self.setState \state ->
              if self.props.allowMultiple then state { pendingFiles = Array.snoc state.pendingFiles pendingFileInfo }
                else state { pendingFiles = [ pendingFileInfo ] }

          UpdatePendingFile name progress -> do
            let
              go existing =
                case Array.findIndex (\f -> f.name == name) existing of
                  Nothing ->
                    existing
                  Just ix ->
                    fromMaybe existing
                    $ Array.modifyAt ix (_{ progress = Just progress }) existing
            self.setState \state -> state { pendingFiles = go state.pendingFiles }

          CancelPendingFile pendingFileInfo -> do
            -- | The pending file data structure probably
            -- | needs to change to support this
            pure unit

          AddCompleteFiles files -> do
            let
              fileNames = map _.name files
            self.setStateThen
              (\state -> state
                { pendingFiles = Array.filter (\f -> f.name `not Array.elem` fileNames) state.pendingFiles
                , filePreviewCache =
                    Array.foldl (\m f -> maybe m (\uri -> Map.insert f.id uri m) f.previewUri) state.filePreviewCache files
                })
              do
                props <- readProps self
                props.onChange $ mergeFileIds (shouldAllowMultiple props) (map _.id files) props.value

          RemoveCompleteFile fileId -> do
            launchAff_ do
              shouldRemove <- self.props.backend.remove fileId
              when shouldRemove $ liftEffect do
                props <- readProps self
                props.onChange $ Array.delete fileId props.value

    render self@{ props, state, instance_ } =
      FetchCache.multi
        { getData: \fileIds -> do
            files <- parTraverse (props.backend.fetch <<< FileId) fileIds
            pure $ Array.zipWith { id: _, value: _ } fileIds files
        , ids: Just $ map (un FileId) (props.value :: Array FileId)
        , render: \values ->
            let
              completeFiles = maybe [] (Array.catMaybes <<< map _.value) values
            in
              dragdrop
                { onDrop: runUpload self
                , render: \dragdropState ->
                    renderUI
                      { disabled: props.disabled
                      , allowMultiple: shouldAllowMultiple props
                      , readonly: props.readonly
                      , required: props.required
                      , isDragging: dragdropState.isDragging
                      , completeFiles
                      , pendingFiles: state.pendingFiles
                      , filePreviewCache: state.filePreviewCache
                      , onDrop: dragdropState.onDrop
                      , testId: props.testId
                      , backend: props.backend
                      , variant: props.variant
                      }
                      self
                }
        }

    shouldAllowMultiple { allowMultiple, variant } = allowMultiple && case variant of
      Avatar -> false
      Logo -> false
      _      -> true

    renderUI stuff self =
      lumiUpload
        { "data-disabled": stuff.disabled
        , "data-multiple": stuff.allowMultiple
        , "data-readonly": stuff.readonly
        , "data-required": stuff.required
        , "data-is-dragging": stuff.isDragging
        , children:
            [ case stuff.variant of
                Images -> renderImageList stuff self
                Files  -> renderFileList stuff self
                Avatar -> renderAvatarImage stuff self stuff.variant
                Logo -> renderAvatarImage stuff self stuff.variant
            ]
        }

    renderDropArea stuff self =
      if stuff.readonly || (not stuff.allowMultiple && not Array.null stuff.completeFiles)
        then empty
        else lumiUploadLabel
          { key: "lumi-upload-label"
          , className: "lumi-upload"
          , onDrop: stuff.onDrop
          , children:
              [ lumiUploadPlaceholder
                  { children:
                      [ body_ $ "Drag and drop or" <> nbsp
                      , button linkStyle { title = "pick a file" }
                      ]
                  }
              , renderHiddenInput stuff self
              ]
          }

    renderHiddenInput stuff self =
      lumiUploadInput
        { className: "lumi lumi-upload"
        , "type": "file"
        , "data-testid": stuff.testId
        , disabled: stuff.disabled
        , multiple: stuff.allowMultiple
        , readOnly: stuff.readonly
        , required: stuff.required
        , accept:
            case stuff.variant of
              Images -> "image/*"
              Files  -> ""
              Avatar -> "image/*"
              Logo -> "image/*"
        , onChange: handler target \e -> do
            mFiles <- getEventFiles e
            for_ mFiles (send self <<< AddNewFiles)
        , value: ""
        }

    renderImageList stuff self =
      fragment
        [ renderDropArea stuff self
        , if Array.null stuff.completeFiles && Array.null stuff.pendingFiles
            then empty
            else lumiUploadImageList
              { children:
                  ( stuff.completeFiles <#> \value ->
                      lumiUploadImage
                        { key: un FileName value.name
                        , onClick: cancelAnd $ sequence_ (self.props.onClick value.id)
                        , "data-clickable": isJust (self.props.onClick value.id)
                        , style:
                            case value.previewUri <|> Map.lookup value.id stuff.filePreviewCache of
                              Just (URI uri) ->
                                R.css
                                  { backgroundImage: "url(\"" <> uri <> "\")"
                                  , backgroundPosition: "center"
                                  , backgroundRepeat: "no-repeat"
                                  , backgroundSize: "cover"
                                  }
                              _ ->
                                R.css
                                  { backgroundImage: "url(\"data:image/svg+xml;charset=UTF-8,%3csvg viewBox='0 0 112 99' width='100%' xmlns='http://www.w3.org/2000/svg'%3e%3cg stroke-width='2' stroke='%23CBCBC9' fill='none' fill-rule='evenodd' opacity='.7'%3e%3cpath d='M111 95L80.9997013 43 56 81.1326499 31.0002987 60.3333333 1 95'/%3e%3cpath d='M1 98h110V1H1z'/%3e%3cpath d='M49 33c0 6.6314752-5.3685248 12-12 12-6.62359 0-12-5.3685248-12-12 0-6.62359 5.37641-12 12-12 6.6314752 0 12 5.37641 12 12z'/%3e%3c/g%3e%3c/svg%3e \")"
                                  , backgroundPosition: "center"
                                  , backgroundRepeat: "no-repeat"
                                  , backgroundSize: "36px 36px"
                                  }
                        , children:
                            if stuff.readonly || value.readonly
                              then [ empty ]
                              else
                                [ lumiUploadImageHoverOverlay {}
                                , lumiUploadImageRemove
                                    { onClick: capture_ $ send self $ RemoveCompleteFile value.id
                                    , children: icon_ Bin
                                    }
                                ]
                        }
                  ) <>
                  ( let completeFileNames = map _.name stuff.completeFiles
                    in Array.filter (\f -> not Array.elem f.name completeFileNames) stuff.pendingFiles <#> \value ->
                      lumiUploadImage
                        { key: un FileName value.name
                        , "data-clickable": false
                        , onClick: cancel
                        , style: R.css {}
                        , children:
                            [ value.progress # foldMap \progress ->
                                progressCircle progressDefaults
                                  { total = progress.totalBytes
                                  , completed = progress.uploadedBytes
                                  }
                            ]
                        }
                  )
              }
        ]

    renderFileList stuff self =
      fragment
        [ renderDropArea stuff self
        , if Array.null stuff.completeFiles && Array.null stuff.pendingFiles
            then empty
            else lumiUploadFileList
              { children:
                  ( stuff.completeFiles <#> \value ->
                      lumiUploadFile
                        { key: un FileName value.name
                        , children:
                            [ lumiUploadFileImage
                                { style:
                                    case value.previewUri <|> Map.lookup value.id stuff.filePreviewCache of
                                      Just (URI uri) ->
                                        R.css
                                          { backgroundImage: "url(\"" <> uri <> "\")"
                                          , backgroundPosition: "center"
                                          , backgroundRepeat: "no-repeat"
                                          , backgroundSize: "cover"
                                          }
                                      _ ->
                                        R.css
                                          { backgroundImage: "url(\"data:image/svg+xml;charset=UTF-8,%3csvg viewBox='0 0 112 99' width='100%' xmlns='http://www.w3.org/2000/svg'%3e%3cg stroke-width='2' stroke='%23CBCBC9' fill='none' fill-rule='evenodd' opacity='.7'%3e%3cpath d='M111 95L80.9997013 43 56 81.1326499 31.0002987 60.3333333 1 95'/%3e%3cpath d='M1 98h110V1H1z'/%3e%3cpath d='M49 33c0 6.6314752-5.3685248 12-12 12-6.62359 0-12-5.3685248-12-12 0-6.62359 5.37641-12 12-12 6.6314752 0 12 5.37641 12 12z'/%3e%3c/g%3e%3c/svg%3e \")"
                                          , backgroundPosition: "center"
                                          , backgroundRepeat: "no-repeat"
                                          , backgroundSize: "36px 36px"
                                          }
                                }
                            , lumiUploadFileInfo
                                { children:
                                    [ case self.props.onClick value.id of
                                        Nothing ->
                                          body_ $ un FileName value.name
                                        Just onClick ->
                                          button linkStyle
                                            { title = un FileName value.name
                                            , onPress = cancelAnd onClick
                                            }
                                    ]
                                }
                            , if stuff.readonly || value.readonly
                                then empty
                                else
                                  lumiUploadFileRemove
                                    { onClick: capture_ $ send self $ RemoveCompleteFile value.id
                                    , children: icon_ Bin
                                    , style: R.css {}
                                    }
                            ]
                        }
                  )
                  <>
                  ( let completeFileNames = map _.name stuff.completeFiles
                    in Array.filter (\f -> not Array.elem f.name completeFileNames) stuff.pendingFiles <#> \value ->
                      lumiUploadFile
                        { key: un FileName value.name
                        , children:
                            [ lumiUploadFileImage
                                { style:
                                    case value.previewUri of
                                      Just (URI uri) ->
                                        R.css
                                          { backgroundImage: "url(\"" <> uri <> "\")"
                                          , backgroundPosition: "center"
                                          , backgroundRepeat: "no-repeat"
                                          , backgroundSize: "cover"
                                          }
                                      _ ->
                                        R.css
                                          { backgroundImage: "url(\"data:image/svg+xml;charset=UTF-8,%3csvg viewBox='0 0 112 99' width='100%' xmlns='http://www.w3.org/2000/svg'%3e%3cg stroke-width='2' stroke='%23CBCBC9' fill='none' fill-rule='evenodd' opacity='.7'%3e%3cpath d='M111 95L80.9997013 43 56 81.1326499 31.0002987 60.3333333 1 95'/%3e%3cpath d='M1 98h110V1H1z'/%3e%3cpath d='M49 33c0 6.6314752-5.3685248 12-12 12-6.62359 0-12-5.3685248-12-12 0-6.62359 5.37641-12 12-12 6.6314752 0 12 5.37641 12 12z'/%3e%3c/g%3e%3c/svg%3e \")"
                                          , backgroundPosition: "center"
                                          , backgroundRepeat: "no-repeat"
                                          , backgroundSize: "20px 20px"
                                          }
                                }
                            , lumiUploadFileInfo
                                { children: join
                                    [ [ body_ $ un FileName value.name ]
                                    , value.progress # foldMap \progress ->
                                        [ body_ $ show (Int.round (100.0 * toNumber progress.uploadedBytes / toNumber progress.totalBytes)) <> "% of " <> simpleBytesToString progress.totalBytes
                                        , progressBar progressDefaults
                                            { total = progress.totalBytes
                                            , completed = progress.uploadedBytes
                                            }
                                        ]
                                    ]
                                }
                            , lumiUploadFileRemove
                                { onClick: capture_ $ send self $ CancelPendingFile value
                                , children: icon_ Bin
                                , style: R.css { visibility: "hidden" }
                                }
                            ]
                        }
                  )
              }
        ]

    simpleBytesToString bytes
      | bytes >= 1073741824 = show (Int.round (toNumber bytes / 1073741824.0)) <> "gb"
      | bytes >= 1048576 = show (Int.round (toNumber bytes / 1048576.0)) <> "mb"
      | bytes >= 1024 = show (Int.round (toNumber bytes / 1024.0)) <> "kb"
      | otherwise = show (Int.round (toNumber bytes)) <> "b"

    renderAvatarImage stuff self variant =
      lumiUploadAvatar
        { onDrop:
            if stuff.readonly
              then handler_ do pure unit
              else stuff.onDrop
        , children:
            [ case Array.head stuff.pendingFiles, Array.head stuff.completeFiles of
                Just pendingFileInfo, _ ->
                  lumiUploadAvatarImage
                    { style: R.css {}
                    , "data-clickable": false
                    , onClick: cancel
                    , children:
                        [ pendingFileInfo.progress # foldMap \progress ->
                            progressCircle progressDefaults
                              { total = progress.totalBytes
                              , completed = progress.uploadedBytes
                              }
                        ]
                    }

                _, Just fileInfo ->
                  case fileInfo.previewUri <|> Map.lookup fileInfo.id stuff.filePreviewCache of
                    Just (URI uri) ->
                      lumiUploadAvatarImage
                        { style: R.css
                            { backgroundImage: "url(\"" <> uri <> "\")"
                            , backgroundPosition: "center"
                            , backgroundRepeat: "no-repeat"
                            , backgroundSize: "cover"
                            }
                        , "data-clickable": isJust (self.props.onClick fileInfo.id)
                        , onClick: cancelAnd $ sequence_ (self.props.onClick fileInfo.id)
                        , children: []
                        }
                    _ ->
                      defaultAvatarImage

                _, _ ->
                  defaultAvatarImage
            , if stuff.readonly
                then empty
                else
                  let
                    isEmpty = Array.null stuff.pendingFiles && Array.null stuff.completeFiles
                  in
                    lumiUploadAvatarActions
                      { children:
                          [ R.label
                              { className: "lumi-upload"
                              , children:
                                  [ button linkStyle
                                      { title = if isEmpty then "Upload photo" else "Change photo"
                                      }
                                  , renderHiddenInput stuff self
                                  ]
                              }
                          , case Array.head stuff.completeFiles of
                              Nothing ->
                                empty

                              Just fileInfo | stuff.readonly || fileInfo.readonly ->
                                empty

                              Just fileInfo ->
                                button linkStyle
                                  { onPress = capture_ $ send self $ RemoveCompleteFile fileInfo.id
                                  , title = "Remove"
                                  }
                          ]
                      }
            ]
        }
      where
        defaultAvatarImage =
          case variant of
            Logo ->
              lumiUploadAvatarImage
                { style: R.css { backgroundColor: cssStringHSLA colors.black4 }
                , "data-clickable": false
                , onClick: cancel
                , children: []
                }
            _ ->
              lumiUploadAvatarImage
                { style: R.css {}
                , "data-clickable": false
                , onClick: cancel
                , children: [ userSvg ]
                }

    lumiUpload = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-upload")
    lumiUploadLabel = elementKeyed (unsafePerformEffect $ R.unsafeCreateDOMComponent "label")
    lumiUploadDragMask = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-upload-drag-mask")
    lumiUploadPlaceholder = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-upload-placeholder")
    lumiUploadInput = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "input")

    lumiUploadImageList = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-upload-image-list")
    lumiUploadImage = elementKeyed (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-upload-image")
    lumiUploadImageHoverOverlay = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-upload-image-hover-overlay")
    lumiUploadImageRemove = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-upload-image-remove-icon")
    lumiUploadImageProgress = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-upload-image-progress")

    lumiUploadFileList = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-upload-file-list")
    lumiUploadFile = elementKeyed (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-upload-file")
    lumiUploadFileImage = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-upload-file-image")
    lumiUploadFileInfo = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-upload-file-info")
    lumiUploadFileRemove = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-upload-file-remove")

    lumiUploadAvatar = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-upload-avatar")
    lumiUploadAvatarImage = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-upload-avatar-image")
    lumiUploadAvatarActions = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-upload-avatar-actions")

mergeFileIds :: Boolean -> Array FileId -> Array FileId -> Array FileId
mergeFileIds allowMultiple newValues existingValues =
  newValues
    # Array.filter (\id -> not Array.elem id existingValues)
    # append existingValues
    # if allowMultiple then identity else maybe [] pure <<< Array.last

getEventFiles :: EventTarget -> Effect (Maybe (Array File))
getEventFiles fileInput = do
  let mInput = HTMLInputElement.fromEventTarget fileInput
  files <- case mInput of
            Nothing    -> pure Nothing
            Just input -> do
              fileList <- HTMLInputElement.files input
              pure $ map fileListToArray fileList
  pure files

cancel :: EventHandler
cancel = handler (stopPropagation >>> preventDefault) \_ -> pure unit

cancelAnd :: Effect Unit -> EventHandler
cancelAnd eff = handler (stopPropagation >>> preventDefault) \_ -> eff

unsafeGlobalEventTarget :: EventTarget
unsafeGlobalEventTarget = unsafePerformEffect do
  mBody <- HTMLDocument.body =<< document =<< window
  case mBody of
    Just body -> pure $ HTMLElement.toEventTarget body
    Nothing   -> map Window.toEventTarget window

nativeClientX :: Event -> Maybe Number
nativeClientX = either (const Nothing) Just <<< runExcept <<< (readNumber <=< readProp "clientX") <<< unsafeToForeign

nativeClientY :: Event -> Maybe Number
nativeClientY = either (const Nothing) Just <<< runExcept <<< (readNumber <=< readProp "clientY") <<< unsafeToForeign

fileListToArray :: FileList -> Array File
fileListToArray fileList = ST.run do
  arr <- STArray.new
  ST.for 0 (FileList.length fileList) \i -> do
    for_ (FileList.item i fileList) \file -> do
      void $ STArray.push file arr
  STArray.freeze arr

foreign import createObjectURL :: EffectFn1 File String

type DragdropProps =
  { onDrop :: Array File -> Effect Unit
  , render :: DragdropState -> JSX
  }

type DragdropState =
  { isDragging :: Boolean
  , onDrop :: EventHandler
  }

data DragdropAction
  = IsDragging Boolean

dragdropComponent :: Component DragdropProps
dragdropComponent = createComponent "UploadDragdrop"

dragdrop :: DragdropProps -> JSX
dragdrop = make dragdropComponent { initialState, render }
  where
    initialState =
      { isDragging: false
      }

    send self = case _ of
      IsDragging isDragging -> do
        unless (self.state.isDragging == isDragging) do
          self.setState _ { isDragging = isDragging }

    render self@{ props, state } =
      let
        dragdropState =
          { isDragging: state.isDragging
          , onDrop: capture nativeEvent \e -> do
              let files = DataTransfer.files =<< (DragEvent.dataTransfer <$> DragEvent.fromEvent e)
              traverse_ (props.onDrop <<< fileListToArray) files
          }
      in
        globalEvents unsafeGlobalEventTarget
          [ { eventType: EventType "drop"
            , options: defaultOptions
            , handler: \e -> do
                E.preventDefault e -- prevent accidental drops from redirecting the page
                send self $ IsDragging false
            }
          , { eventType: EventType "dragover"
            , options: defaultOptions
            , handler: \e -> do
                E.preventDefault e -- prevent accidental drops from redirecting the page
                send self $ IsDragging true
            }
          , { eventType: EventType "dragenter"
            , options: defaultOptions
            , handler: \e -> do
                E.preventDefault e -- prevent accidental drops from redirecting the page
            }
          , { eventType: EventType "dragleave"
            , options: defaultOptions
            , handler: \e -> do
                E.preventDefault e
                send self $ IsDragging false
            }
          ]
          $ props.render dragdropState

foreign import xhrUpload ::
  EffectFn5
    String
    File
    (EffectFn1 { totalBytes :: Int, uploadedBytes :: Int } Unit)
    (EffectFn1 Error Unit)
    (EffectFn1 String Unit)
    Unit

uploadFile
  :: URI
  -> (String -> Either Error FileId)
  -> File
  -> Producer Progress Aff (Either Error FileId)
uploadFile uri parseFileId file = produce \emitter ->
  runEffectFn5 xhrUpload
    (un URI uri)
    file
    (mkEffectFn1 (emit emitter))
    (mkEffectFn1 $ close emitter <<< Left)
    (mkEffectFn1 $ close emitter <<< parseFileId)

styles :: JSS
styles = jss
  { "@global":
      { "lumi-upload":
          { boxSizing: "border-box"
          , display: "flex"
          , flexFlow: "column"

          , "&[data-is-dragging=\"true\"] > label.lumi-upload > lumi-upload-placeholder":
              { backgroundColor: cssStringHSLA colors.primary4
              }

          , "& > label.lumi-upload":
              { boxSizing: "border-box"
              , display: "flex"
              , flexFlow: "column"
              , alignItems: "stretch"
              , padding: "8px"
              , cursor: "pointer"
              , borderRadius: "3px"
              , border: [ "dashed", "1px", cssStringHSLA colors.black2 ]
              , height: "80px"

              , "& > lumi-upload-placeholder":
                  { boxSizing: "border-box"
                  , flex: "1"
                  , display: "flex"
                  , flexFlow: "row"
                  , alignItems: "center"
                  , justifyContent: "center"
                  , padding: "8px"
                  , borderRadius: "3px"
                  , color: cssStringHSLA colors.black1
                  }

              , "& > input": { display: "none" }
              }

          , "& > label.lumi-upload + lumi-upload-image-list, & > label.lumi-upload + lumi-upload-file-list":
                { marginTop: "8px" -- (gap)
                }

          , "& > lumi-upload-image-list":
              { boxSizing: "border-box"
              , flex: "1"
              , display: "flex"
              , flexFlow: "row wrap"
              , marginTop: "-8px" -- (gap)
              , marginLeft: "-8px" -- (gap)
              , marginRight: "-8px" -- (gap)
              , marginBottom: "-8px" -- (gap)

              , "& > lumi-upload-image":
                  { boxSizing: "border-box"
                  , margin: "8px" -- (gap)
                  , display: "flex"
                  , alignItems: "center"
                  , justifyContent: "center"
                  , border: [ "1px", "solid", cssStringHSLA colors.black4 ]
                  , backgroundColor: cssStringHSLA colors.black6
                  , cursor: "initial"
                  , position: "relative"
                  , width: "128px"
                  , height: "128px"

                  , "&[data-clickable=\"true\"]":
                      { cursor: "pointer"
                      }

                  , "&:not(:hover)":
                      { "& > lumi-upload-image-hover-overlay, & > lumi-upload-image-remove-icon":
                          { opacity: "0"
                          }
                      }

                  , "& > lumi-upload-image-hover-overlay":
                      { boxSizing: "border-box"
                      , position: "absolute"
                      , top: "-1px"
                      , right: "-1px"
                      , bottom: "-1px"
                      , left: "-1px"
                      , display: "flex"
                      , flexFlow: "row"
                      , justifyContent: "flex-end"
                      , alignItems: "flex-start"
                      , height: "128px"
                      , width: "128px"
                      , padding: "11px"
                      , backgroundColor: cssStringHSLA colors.black
                      , opacity: "0.75"
                      , transition: "opacity 200ms ease-in-out"
                      }

                  , "& > lumi-upload-image-remove-icon":
                      { boxSizing: "border-box"
                      , position: "absolute"
                      , top: "12px"
                      , right: "12px"
                      , display: "flex"
                      , justifyContent: "center"
                      , alignItems: "center"
                      , cursor: "pointer"
                      , height: "20px"
                      , width: "20px"
                      , fontSize: "20px"
                      , lineHeight: "20px"
                      , color: cssStringHSLA colors.black3
                      , transition: "color, opacity 200ms ease-in-out"

                      , "&:hover":
                          { color: cssStringHSLA colors.white
                          }
                      }
                  }
              }

          , "& > lumi-upload-file-list":
              { boxSizing: "border-box"
              , display: "flex"
              , flexFlow: "column"

              , "& > lumi-upload-file":
                  { boxSizing: "border-box"
                  , display: "flex"
                  , flexFlow: "row"
                  , alignItems: "center"
                  , height: "65px"
                  , borderBottom: [ "1px", "solid", cssStringHSLA colors.black4 ]

                  , "& > lumi-upload-file-image":
                      { boxSizing: "border-box"
                      , height: "40px"
                      , width: "40px"
                      , border: [ "1px", "solid", cssStringHSLA colors.black4 ]
                      , backgroundColor: cssStringHSLA colors.black6
                      }

                  , "& > lumi-upload-file-info":
                      { boxSizing: "border-box"
                      , flex: "1"
                      , display: "flex"
                      , flexFlow: "row wrap"
                      , alignItems: "center"
                      , padding: "0 30px 0 12px"
                      , overflow: "hidden"

                      , "& > :nth-child(1)": { flex: "1" }
                      , "& > :nth-child(2)":
                          { color: cssStringHSLA colors.black1
                          , padding: "0 30px"
                          }
                      , "& > :nth-child(3)": { flex: "0 1 120px" }
                      }

                  , "& > lumi-upload-file-remove":
                      { boxSizing: "border-box"
                      , display: "flex"
                      , justifyContent: "center"
                      , alignItems: "center"
                      , cursor: "pointer"
                      , height: "20px"
                      , width: "20px"
                      , fontSize: "20px"
                      , lineHeight: "20px"
                      , color: cssStringHSLA colors.primary
                      }
                  }
              }

          , "& > lumi-upload-avatar":
              { boxSizing: "border-box"
              , display: "flex"
              , flexFlow: "row"
              , alignItems: "center"
              , height: "80px"

              , "& > lumi-upload-avatar-image":
                  { boxSizing: "border-box"
                  , display: "flex"
                  , alignItems: "center"
                  , justifyContent: "center"
                  , height: "80px"
                  , width: "80px"
                  , border: [ "1px", "solid", cssStringHSLA colors.black4 ]
                  , borderRadius: "40px"
                  , overflow: "hidden"
                  , backgroundColor: cssStringHSLA colors.white
                  , "&[data-clickable=\"true\"]":
                      { cursor: "pointer"
                      }
                  }
              , "& > lumi-upload-avatar-actions":
                  { boxSizing: "border-box"
                  , marginLeft: "16px"
                  , display: "flex"
                  , flexFlow: "column"
                  , justifyContent: "center"

                  , "& > :not(:first-child)":
                      { marginTop: "8px"
                      }

                  , "& > label.lumi-upload > input":
                      { display: "none"
                      }
                  }
              }
          }
      }
  }
