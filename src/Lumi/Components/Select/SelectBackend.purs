module Lumi.Components.Select.Backend
  ( SelectOption
  , SelectOptions(..)
  , SelectBackendProps
  , selectBackend
  , defaultOptionSort
  ) where

import Prelude

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
import React.Basic (Component, JSX, createComponent, make)
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

    send self@{ props, state } action =
      unless props.disabled do
        case action of
          IsMouseDown isMouseDown -> do
            unless (isMouseDown == state.isMouseDown) do
              self.setState _ { isMouseDown = isMouseDown }

          OpenSelect -> do
            unless state.isOpen do
              self.setState _ { isOpen = true }
              send self $ LoadOptions self.state.searchTerm

          CloseSelect -> do
            unless (not state.isOpen) do
              self.setState _ { isOpen = false, focusedIndex = Nothing }

          FocusIndexUp -> do
            self.setState _
              { focusedIndex = Just $
                  max 0 (maybe 0 (_ - 1) state.focusedIndex)
              }

          FocusIndexDown -> do
            self.setState _
              { focusedIndex = Map.lookup state.searchTerm state.optionCache <#> \options ->
                  min (optionsLength options - 1) (maybe 0 (_ + 1) state.focusedIndex)
              }

          AddSelectedOption option -> do
            let
              valueToRemove :: String
              valueToRemove = (props.toSelectOption option).value

              isOptionSelected :: Boolean
              isOptionSelected =
                Array.any (\v -> (props.toSelectOption v).value == valueToRemove) props.value
            when (not isOptionSelected) do
              self.props.onChange
                if self.props.allowMultiple
                  then self.props.value <> [ option ]
                  else [ option ]
            send self $ SetSearchTerm ""
            send self CloseSelect

          RemoveSelectedOption option ->
            let
              valueToRemove :: String
              valueToRemove = (props.toSelectOption option).value

              valueMinusSelectedOption :: Array option
              valueMinusSelectedOption =
                Array.filter (\v -> (props.toSelectOption v).value /= valueToRemove) props.value
            in do
              when (Array.length self.props.value /= Array.length valueMinusSelectedOption) do
                self.props.onChange valueMinusSelectedOption

          RemoveAllSelectedOptions -> do
            self.props.onChange []
            send self $ SetSearchTerm ""
            send self CloseSelect

          SetSearchTerm searchTerm -> do
            unless props.disabled do
              self.setState _ { searchTerm = searchTerm }
              send self $ LoadOptions searchTerm

          LoadOptions searchTerm -> do
            when (isNothing $ Map.lookup searchTerm self.state.optionCache) do
              let
                tidyUp =
                    map
                      (\external -> let { label, value } = self.props.toSelectOption external
                                    in { label, value, external })
                      <<< Array.nubBy
                            ((fromMaybe
                              (defaultOptionSort (_.label <<< self.props.toSelectOption)) self.props.optionSort)
                              searchTerm)
                      <<< Array.take 10000

              runAff_
                (send self <<< SetOptions searchTerm <<< either (Failed <<< message) Ready)
                (tidyUp <$> self.props.loadOptions searchTerm)

          SetOptions searchTerm options ->
            self.setState _ { optionCache = Map.insert searchTerm options state.optionCache }

    render self =
      let
        childProps =
          { addSelectedOption: send self <<< AddSelectedOption
          , focusedIndex: self.state.focusedIndex
          , openSelect: send self OpenSelect
          , closeSelect: send self CloseSelect
          , isOpen: self.state.isOpen
          , keydownEventHandler
          , options: map _.external $ fromMaybe Loading $
              Map.lookup self.state.searchTerm self.state.optionCache
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
                      when self.state.isOpen do
                        eventTargetIsChild <- isOrContainsNode rootRef target
                        mActiveNodeIsChild <- isOrContainsActiveNode rootRef
                        for_ mActiveNodeIsChild \activeNodeIsChild -> do
                          when (not eventTargetIsChild && not activeNodeIsChild) do
                            childProps.closeSelect
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
                          childProps.closeSelect
              }
            , { eventType: EventType "keydown"
              , options: { capture: false, once: false, passive: false }
              , handler: \e -> do
                  if not self.state.isOpen
                    then pure unit
                    else keydownEventHandler e
              }
            ]
            child
      where
        keydownEventHandler :: Event -> Effect Unit
        keydownEventHandler e = do
          when (not self.props.disabled) do
            let mKey = eventKey e
            for_ mKey case _ of
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
              "Enter" -> do
                E.preventDefault e
                E.stopPropagation e
                traverse_ (send self <<< AddSelectedOption) $ map _.external $ join $
                  getOption <$> self.state.focusedIndex <*> Map.lookup self.state.searchTerm self.state.optionCache
              "Escape" -> do
                E.preventDefault e
                E.stopPropagation e
                send self $ SetSearchTerm ""
                send self CloseSelect
              "Backspace" -> when (String.null self.state.searchTerm && not Array.null self.props.value) do
                E.preventDefault e
                E.stopPropagation e
                traverse_ (send self <<< RemoveSelectedOption) $ Array.last self.props.value
              _ -> pure unit

optionsLength :: forall a. SelectOptions a -> Int
optionsLength = case _ of
  Ready xs -> Array.length xs
  _        -> 0

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
