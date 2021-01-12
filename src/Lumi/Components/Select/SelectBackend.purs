module Lumi.Components.Select.Backend
  ( SelectOption
  , SelectOptions(..)
  , SelectBackendProps
  , selectBackend
  , defaultOptionSort
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (either)
import Data.Foldable (for_, traverse_)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.Nullable (toMaybe)
import Data.String as String
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, message, runAff_)
import React.Basic.Classic (Component, JSX, createComponent, make, readProps, readState)
import React.Basic.DOM.Components.GlobalEvents (windowEvents)
import React.Basic.DOM.Components.Ref (ref)
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)
import Web.DOM (Node)
import Web.DOM.Node as Node
import Web.Event.Event (Event, EventType(..))
import Web.Event.Event as E
import Web.HTML (window)
import Web.HTML.HTMLDocument (activeElement)
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window (document)
import Web.UIEvent.KeyboardEvent as KE

type SelectOption = { value :: String , label :: String }

type SelectOptionInternal a = { value :: String , label :: String, external :: a }

data SelectOptions option
  = Ready (Array option)
  | Failed String
  | Loading

derive instance fSelectOptions :: Functor SelectOptions

type SelectBackendProps option =
  { allowMultiple :: Boolean
  , disabled :: Boolean
  , isOpen :: Maybe Boolean
  , loadOnMount :: Boolean
  , loadOptions :: String -> Aff (Array option)
  , toSelectOption :: option -> SelectOption
  , optionSort :: Maybe (String -> option -> option -> Ordering)
  , onChange :: Array option -> Effect Unit
  , render ::
      { addSelectedOption :: option -> Effect Unit
      , closeSelect :: Effect Unit
      , focusedIndex :: Maybe Int
      , isOpen :: Boolean
      , isActive :: Boolean
      , keydownEventHandler :: Event -> Effect Unit
      , openSelect :: Effect Unit
      , options :: SelectOptions option
      , removeAllSelectedOptions :: Effect Unit
      , removeSelectedOption :: option -> Effect Unit
      , searchTerm :: String
      , setSearchTerm :: String -> Effect Unit
      }
      -> JSX
  , value :: Array option
  }

type SelectBackendState option =
  { focusedIndex :: Maybe Int
  , isMouseDown :: Boolean
  , isOpen :: Boolean
  , isActive :: Boolean
  , optionCache :: Map.Map String (SelectOptions (SelectOptionInternal option))
  , searchTerm :: String
  }

type SetState option = (SelectBackendState option -> SelectBackendState option) -> Effect Unit

component :: forall option. Component (SelectBackendProps option)
component = createComponent "SelectBackend"

data Action option
  = IsMouseDown Boolean
  | OpenSelect
  | CloseSelect
  | Blur
  | FocusIndexUp
  | FocusIndexDown
  | AddSelectedOption option
  | RemoveSelectedOption option
  | RemoveAllSelectedOptions
  | SetSearchTerm String
  | LoadOptions String
  | SetOptions String (SelectOptions (SelectOptionInternal option))

selectBackend :: forall option. SelectBackendProps option -> JSX
selectBackend = make component
  { initialState
  , didMount
  , didUpdate
  , render
  }
  where
    initialState :: SelectBackendState option
    initialState =
      { focusedIndex: Nothing
      , isMouseDown: false
      , isOpen: false
      , isActive: false
      , optionCache: Map.empty
      , searchTerm: ""
      }

    didMount self = do
      when self.props.loadOnMount $ send self $ LoadOptions self.state.searchTerm
      syncIsOpenState self

    didUpdate self _ = do
      syncIsOpenState self

    syncIsOpenState self = do
      let isOpen = fromMaybe self.state.isOpen self.props.isOpen
      when (isOpen /= self.state.isOpen) do
        send self if isOpen then CloseSelect else OpenSelect

    send self action = do
      props <- readProps self
      state <- readState self
      unless props.disabled do
        case action of
          IsMouseDown isMouseDown -> do
            unless (isMouseDown == state.isMouseDown) do
              self.setState _ { isMouseDown = isMouseDown }

          OpenSelect -> do
            unless state.isOpen do
              self.setState \s -> s
                { isOpen = true
                , isActive = true
                , focusedIndex = s.focusedIndex <|>
                    if (getOptions props s # maybe 0 optionsLength) > 0 then
                      Just 0
                    else
                      Nothing
                }
              send self $ LoadOptions state.searchTerm

          CloseSelect -> do
            when (not String.null state.searchTerm) do
              -- This call to `props.onChange` looks like a noop,
              -- but it communicates to parent components that a
              -- choice has not been made. This is like typing in
              -- a normal text field and then clearing the value
              -- and this allows parent components and forms to
              -- mark the field as "modified" and display
              -- validation messages.
              props.onChange props.value
            unless (not state.isOpen) do
              self.setState _
                { isOpen = false
                , searchTerm = ""
                }

          Blur -> do
            when (state.isActive) do
              self.setState _ { isActive = false }

          FocusIndexUp -> do
            self.setState \s -> s
              { focusedIndex = do
                  options <- getOptions props s
                  let iMax = optionsLength options - 1
                  pure
                    case fromMaybe 0 s.focusedIndex of
                      i | i <= 0 -> iMax -- wrap to end
                      i -> min iMax (i - 1)
              }

          FocusIndexDown -> do
            self.setState \s -> s
              { focusedIndex = do
                  options <- getOptions props s
                  let iMax = optionsLength options - 1
                  pure
                    case fromMaybe (-1) s.focusedIndex of
                      i | i >= iMax -> 0 -- wrap to start
                      i -> max 0 (i + 1)
              }

          AddSelectedOption option -> do
            let
              valueToAdd :: String
              valueToAdd = (props.toSelectOption option).value

              isOptionSelected :: Boolean
              isOptionSelected =
                Array.any (\v -> (props.toSelectOption v).value == valueToAdd) props.value
            when (not isOptionSelected) do
              props.onChange
                if props.allowMultiple
                  then props.value <> [ option ]
                  else [ option ]
            self.setState _
              { isOpen = false
              , searchTerm = ""
              }

          RemoveSelectedOption option ->
            let
              valueToRemove :: String
              valueToRemove = (props.toSelectOption option).value

              valueMinusSelectedOption :: Array option
              valueMinusSelectedOption =
                Array.filter (\v -> (props.toSelectOption v).value /= valueToRemove) props.value
            in do
              when (Array.length props.value /= Array.length valueMinusSelectedOption) do
                props.onChange valueMinusSelectedOption

          RemoveAllSelectedOptions -> do
            props.onChange []
            self.setState _
              { isOpen = false
              , searchTerm = ""
              }

          SetSearchTerm searchTerm -> do
            unless props.disabled do
              self.setState _ { searchTerm = searchTerm }
              send self $ LoadOptions searchTerm

          LoadOptions searchTerm -> do
            when (isNothing $ getOptions props { searchTerm, optionCache: state.optionCache }) do
              let
                tidyUp =
                    map
                      (\external -> let { label, value } = props.toSelectOption external
                                    in { label, value, external })
                      <<< Array.nubBy
                            ((fromMaybe
                              (defaultOptionSort (_.label <<< props.toSelectOption)) props.optionSort)
                              searchTerm)
                      <<< Array.take 10000

              runAff_
                (send self <<< SetOptions searchTerm <<< either (Failed <<< message) Ready)
                (tidyUp <$> props.loadOptions searchTerm)

          SetOptions searchTerm options ->
            self.setState \s -> s
              { optionCache = Map.insert searchTerm options s.optionCache
              , focusedIndex =
                  case options of
                    Ready a | not Array.null a -> Just 0
                    _ -> Nothing
              }

    render self =
      let
        childProps =
          { addSelectedOption: send self <<< AddSelectedOption
          , focusedIndex: self.state.focusedIndex
          , openSelect: send self OpenSelect
          , closeSelect: send self CloseSelect
          , isOpen: self.state.isOpen
          , isActive: self.state.isActive
          , keydownEventHandler
          , options: map _.external $ fromMaybe Loading $ getOptions self.props self.state
          , removeAllSelectedOptions: send self RemoveAllSelectedOptions
          , removeSelectedOption: send self <<< RemoveSelectedOption
          , searchTerm: self.state.searchTerm
          , setSearchTerm: send self <<< SetSearchTerm
          }
        child = self.props.render childProps
      in
        ref \mRootRef ->
          windowEvents
            [ { eventType: EventType "mousedown"
              , options: { capture: false, once: false, passive: true }
              , handler: \_ -> send self $ IsMouseDown true
              }
            , { eventType: EventType "mouseup"
              , options: { capture: false, once: false, passive: true }
              , handler: \e -> do
                  for_ (Node.fromEventTarget =<< E.target e) \target -> do
                    send self $ IsMouseDown false
                    for_ mRootRef \rootRef -> do
                      when (self.state.isOpen || self.state.isActive) do
                        eventTargetIsChild <- isOrContainsNode rootRef target
                        mActiveNodeIsChild <- isOrContainsActiveNode rootRef
                        for_ mActiveNodeIsChild \activeNodeIsChild -> do
                          when (not eventTargetIsChild && not activeNodeIsChild) do
                            send self CloseSelect
                            send self Blur
              }
            , { eventType: EventType "touchstart"
              , options: { capture: false, once: false, passive: true }
              , handler: \e -> do
                  for_ (Node.fromEventTarget =<< E.target e) \target -> do
                    send self $ IsMouseDown false
                    for_ mRootRef \rootRef -> do
                      when self.state.isOpen do
                        eventTargetIsChild <- isOrContainsNode rootRef target
                        when (not eventTargetIsChild) do
                            send self CloseSelect
                            send self Blur
              }
            , { eventType: EventType "keydown"
              , options: { capture: false, once: false, passive: false }
              , handler: \e -> do
                  when self.state.isActive do
                    keydownEventHandler e
              }
            ]
            child
      where
        keydownEventHandler :: Event -> Effect Unit
        keydownEventHandler e = do
          props <- readProps self
          state <- readState self
          when (not props.disabled) do
            for_ (eventKey e) case _ of
              "ArrowUp" -> do
                E.preventDefault e
                E.stopPropagation e
                send self OpenSelect
                send self FocusIndexUp
              "ArrowDown" -> do
                E.preventDefault e
                E.stopPropagation e
                send self OpenSelect
                send self FocusIndexDown
              "p" -> do
                when (ctrlKey e) do
                  E.preventDefault e
                  E.stopPropagation e
                  send self OpenSelect
                  send self FocusIndexUp
              "n" -> do
                when (ctrlKey e) do
                  E.preventDefault e
                  E.stopPropagation e
                  send self OpenSelect
                  send self FocusIndexDown
              "Tab" -> do
                if state.isOpen then do
                  E.preventDefault e
                  E.stopPropagation e
                  if shiftKey e then do
                    send self OpenSelect
                    send self FocusIndexUp
                  else do
                    send self OpenSelect
                    send self FocusIndexDown
                else do
                  send self Blur
              "Enter" -> do
                when state.isOpen do
                  E.preventDefault e
                  E.stopPropagation e
                  traverse_ (send self <<< AddSelectedOption) do
                    options <- getOptions props state
                    focusedIndex <- state.focusedIndex
                    option <- getOption focusedIndex options
                    pure option.external
              "Escape" -> do
                when state.isOpen do
                  E.preventDefault e
                  E.stopPropagation e
                  send self CloseSelect
              "Backspace" -> do
                let
                  inputIsEmpty = String.null state.searchTerm
                  hasSelectedValues = not Array.null props.value
                when (inputIsEmpty && hasSelectedValues) do
                  E.preventDefault e
                  E.stopPropagation e
                  traverse_ (send self <<< RemoveSelectedOption) do
                    Array.last props.value
              _ -> pure unit

optionsLength :: forall a. SelectOptions a -> Int
optionsLength = case _ of
  Ready xs -> Array.length xs
  _        -> 0

getOptions ::
  forall a p r.
  { value :: Array a
  , toSelectOption :: a -> SelectOption
  | p
  } ->
  { searchTerm :: String
  , optionCache :: Map.Map String (SelectOptions (SelectOptionInternal a))
  | r
  } ->
  Maybe (SelectOptions (SelectOptionInternal a))
getOptions { toSelectOption, value } { searchTerm, optionCache } =
  Map.lookup searchTerm optionCache <#> case _ of
    Ready os -> Ready $ isNotSelected os
    so -> so
  where
  selected = map (_.value <<< toSelectOption) value

  isNotSelected = Array.filter \v -> Array.notElem v.value selected

getOption :: forall a. Int -> SelectOptions a -> Maybe a
getOption i = case _ of
  Ready xs -> Array.index xs i
  _        -> Nothing

isOrContainsNode :: Node -> Node -> Effect Boolean
isOrContainsNode parent child = do
  if unsafeRefEq parent child
    then pure true
    else Node.contains parent child

isOrContainsActiveNode :: Node -> Effect (Maybe Boolean)
isOrContainsActiveNode parent = do
  active <- map HTMLElement.toNode <$> (activeElement =<< document =<< window)
  traverse (isOrContainsNode parent) active

eventKey :: Event -> Maybe String
eventKey = toMaybe <<< _.key <<< unsafeCoerce

shiftKey :: Event -> Boolean
shiftKey = KE.shiftKey <<< (unsafeCoerce :: Event -> KE.KeyboardEvent)

ctrlKey :: Event -> Boolean
ctrlKey = KE.ctrlKey <<< (unsafeCoerce :: Event -> KE.KeyboardEvent)

data ComparisonWeight a = Exact a | StartsWith a | Contains a | Other a

derive instance eqComparisonWeight :: Eq a => Eq (ComparisonWeight a)
derive instance ordComparisonWeight :: Ord a => Ord (ComparisonWeight a)

defaultOptionSort :: forall a. (a -> String) -> String -> a -> a -> Ordering
defaultOptionSort optionLabel searchTerm = comparing f
  where
    f s =
      let
        lowerSearchTerm = String.toLower searchTerm
        label = String.toLower (optionLabel s)
      in
        if label == lowerSearchTerm
          then Exact label
          else
            case String.indexOf (String.Pattern lowerSearchTerm) label of
              Just i | i == 0    -> StartsWith label
                     | otherwise -> Contains (show i <> label)
              _                  -> Other label
