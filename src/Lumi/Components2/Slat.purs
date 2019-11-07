module Lumi.Components2.Slat where

import Prelude

import Color (Color)
import Data.Array as Array
import Data.Char (fromCharCode)
import Data.Foldable (fold)
import Data.Maybe (Maybe(..))
import Data.Nullable as Nullable
import Data.String.CodeUnits (fromCharArray)
import Effect (Effect)
import Foreign.Object (fromHomogeneous)
import Lumi.Components (LumiComponent, LumiComponentProps, lumiComponent)
import Lumi.Components.Size (Size(..))
import Lumi.Styles.Border (Border(..))
import Lumi.Styles.Slat as Styles.Slat
import Lumi.Styles.Theme (LumiTheme)
import Prim.Row (class Union)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)
import React.Basic.Emotion as E
import React.Basic.Hooks (JSX, ReactComponent, ReactContext, useContext)
import React.Basic.Hooks as React

type SlatProps
  = ( content :: Array JSX, border :: Border )

mkSlat :: ReactContext LumiTheme -> Effect (LumiComponent SlatProps SlatProps)
mkSlat t = do
  lumiComponent "Slat" { content: [], className: "", border: BorderRound } \props -> React.do
    theme <- useContext t
    let
      slatStyle = Styles.Slat.slat theme props.border
    pure $
      E.element slatStyle
        R.div'
          { children: props.content
          , className: props.className
          }
