module Lumi.Components.FixedPrecisionInput
  ( ValuedEventHander
  , FixedPrecisionInputProps
  , defaults
  , fixedPrecisionInput
  ) where

import Prelude

import Data.Either (Either(Left, Right), either)
import Data.Fixed (class KnownPrecision, Fixed, PProxy(PProxy), reflectPrecision, fromString, toString)
import Data.Foldable (for_)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (toMaybe, toNullable)
import Data.Number as Math
import Data.String as String
import Data.String.Regex (Regex, regex, test)
import Data.String.Regex.Flags (noFlags)
import Effect.Uncurried (EffectFn2, mkEffectFn2, runEffectFn2)
import Lumi.Components.Size (Size(..))
import React.Basic.Classic (Component, JSX, ReactComponent, createComponent, element, makeStateless)
import React.Basic.DOM (CSS, css, mergeStyles)
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (SyntheticEvent, syntheticEvent)
import React.Basic.Events as Events

type ValuedEventHander precision =
  EffectFn2 (Either String (Fixed precision)) SyntheticEvent Unit

type FixedPrecisionInputProps precision =
  { "type" :: String
  , disabled :: Boolean
  , max :: Number
  , min :: Number
  , name :: String
  , onBlur :: ValuedEventHander precision
  , onChange :: ValuedEventHander precision
  , onFocus :: ValuedEventHander precision
  , pattern :: String
  , patternFailedMessage :: String
  , placeholder :: String
  , readOnly :: Boolean
  , required :: Boolean
  , size :: Size
  , strict :: Boolean
  , style :: CSS
  , testId :: Maybe String
  , validate :: Fixed precision -> Maybe String
  , value :: Either String (Fixed precision)
  }

defaults :: forall precision. FixedPrecisionInputProps precision
defaults =
  { "type": "tel"
  , disabled: false
  , max: Int.toNumber top
  , min: Int.toNumber bottom
  , name: ""
  , onBlur: mkEffectFn2 \_ _ -> pure unit
  , onChange: mkEffectFn2 \_ _ -> pure unit
  , onFocus: mkEffectFn2 \_ _ -> pure unit
  , pattern: "-?[0-9,]*\\.?[0-9,]*"
  , patternFailedMessage: "The value does not appear to be a number."
  , placeholder: ""
  , readOnly: false
  , required: false
  , size: Medium
  , strict: true
  , style: css {}
  , testId: Nothing
  , validate: const Nothing
  , value: Left ""
  }

component :: forall precision. Component (FixedPrecisionInputProps precision)
component = createComponent "FixedPrecisionInput"

fixedPrecisionInput
  :: forall precision
   . KnownPrecision precision
  => FixedPrecisionInputProps precision
  -> JSX
fixedPrecisionInput = makeStateless component render
  where
    render props =
      element input_
        { "data-size": show props.size
        , "data-testid": toNullable props.testId
        , "data-precision": show (reflectPrecision (PProxy :: PProxy precision))
        , "type": props.type
        , autoComplete: "off"
        , className: "lumi"
        , defaultValue: either identity toString props.value
        , disabled: props.disabled
        , max: props.max
        , min: props.min
        , name: props.name
        , onBlur: valuedEventHandlerFor [ props.onChange, props.onBlur ]
        , onFocus: valuedEventHandlerFor [ props.onFocus ]
        , onKeyPress:
            Events.handler syntheticEvent \e ->
            case props.strict, patternTester of
              true, Right patternTester' -> runEffectFn2 cancelWhenNotMatch patternTester' e
              _   , _                    -> pure unit
        , placeholder: props.placeholder
        , readOnly: props.readOnly
        , required: props.required
        , style: mergeStyles
            [ css { "WebkitAppearance": "none"
                  , "MozAppearance": "none"
                  , margin: 0
                  , width: 160
                  }
            , props.style
            ]
        , validate: toMaybe
                      >>> parseValue
                      >>> _.validationError
                      >>> fromMaybe ""
        }
      where
        patternTester = regex ("^" <> props.pattern <> "$") noFlags

        valuedEventHandlerFor callbacks =
          Events.handler (Events.merge { targetValue, syntheticEvent }) \e -> do
            let { value } = parseValue e.targetValue
            for_ callbacks \callback ->
              runEffectFn2 callback value e.syntheticEvent

        parseValue maybeValueString =
          let
            valueString = fromMaybe "" maybeValueString
            asUnvalidatedFixed = do
              patternTester' <- patternTester
              maybe (Left valueString) Right
                if not test patternTester' valueString
                then Nothing
                else fromString (stripCommas valueString)
            validationError =
              case asUnvalidatedFixed of
                Left _        -> Just props.patternFailedMessage
                Right asFixed -> props.validate asFixed
          in
            { validationError
            , value:
                case validationError of
                  Just error -> Left valueString
                  Nothing    -> asUnvalidatedFixed
            }

    stripCommas = String.replace (String.Pattern ",") (String.Replacement "")

countDigits :: Int -> Int
countDigits n = n # Int.toNumber
                  # Math.abs
                  # Math.log
                  # (_ * Math.log10e)
                  # Int.floor

foreign import input_ :: forall props. ReactComponent {| props }

foreign import cancelWhenNotMatch :: EffectFn2 Regex SyntheticEvent Unit
