module Lumi.Components2.Image
  ( image
  , thumbnail
  , small
  , medium
  , large
  , extraLarge
  , resize
  , resizeSquare
  , round
  , Image
  , Thumbnail
  ) where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid as Monoid
import Data.String as String
import Data.Tuple.Nested ((/\))
import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components (LumiComponent, PropsModifier, lumiComponent, ($$$))
import Lumi.Components.Loader (loader)
import Lumi.Components.Svg (placeholderSvg)
import Lumi.Components2.Box as Box
import Lumi.Styles (Style, StyleModifier, style, style_, toCSS)
import Lumi.Styles.Box (FlexAlign(..))
import Lumi.Styles.Box as Styles.Box
import Lumi.Styles.Slat hiding (_interactive,slat) as Styles.Slat
import Lumi.Styles.Theme (LumiTheme(..), useTheme)
import React.Basic.DOM as R
import React.Basic.Emotion as E
import React.Basic.Events (handler_)
import React.Basic.Hooks (JSX)
import React.Basic.Hooks as Hooks

data Image = Image

type ImageProps
  = ( component :: Image
    , src :: String
    , placeholder :: Maybe JSX
    )

-- | An image has no size restrictions
-- | will flex fill it's container
image :: LumiComponent ImageProps
image =
  unsafePerformEffect do
    lumiComponent "Image" defaults \props -> Hooks.do
      theme <- useTheme
      loaded /\ setLoaded <- Hooks.useState' false
      pure
        $ E.element R.div'
            { children:
                [ if String.null props.src
                    then fromMaybe placeholderSvg props.placeholder
                    else
                      Box.column
                      $ Styles.Box._flex
                      $ Styles.Box._align Center
                      $ Styles.Box._justify Center
                      $$$
                        [ Monoid.guard (not loaded) $ placeholderSvg
                            -- @TODO confirming with design
                            -- $ loader { style: R.css { width: "20px", height: "20px", borderWidth: "2px" }, testId: Nullable.toNullable Nothing }
                        , E.element R.img'
                            { src: props.src
                            , className: ""
                            , css: E.css { maxWidth: E.percent 100.0 }
                            , onLoad: handler_ $ setLoaded true
                            }
                        ]
                ]
            , className: props.className
            , css: defaultImageStyle theme <> props.css theme
            }
    where
      defaults =
        { component: Image
        , src: ""
        , placeholder: Nothing
        }

      defaultImageStyle :: LumiTheme -> Style
      defaultImageStyle theme@(LumiTheme { colors }) =
          E.css
            { boxSizing: E.str "border-box"
            , overflow: E.str "hidden"
            , display: E.str "flex"
            , border: E.str "1px solid"
            , borderColor: E.color colors.black4
            }

data Thumbnail = Thumbnail

type ThumbnailProps
  = ( component :: Thumbnail
    , src :: String
    , placeholder :: Maybe JSX
    )

-- | A thumbnail can support size restrictions
thumbnail :: LumiComponent ThumbnailProps
thumbnail =
  unsafePerformEffect do
    lumiComponent "Thumbnail" defaults \props -> Hooks.do
      theme <- useTheme
      pure
        $ image
        $ _ { src = props.src
            , placeholder = props.placeholder
            -- @TODO get this to compile
            -- we should be setting the default square size on thumbnails
            -- , css = theme # toCSS defaultSize <> props.css
            , css = props.css
            }

      where
        defaults =
          { component: Thumbnail
          , src: ""
          , placeholder: Nothing
          }

-- | The `c` type parameter lets us constrain the type of component to which
-- | an image modifier may be applied
type ImageModifier c = forall r. PropsModifier (component :: c | r)

-- | restricted to only Image
resize :: { width :: Int, height :: Int } -> ImageModifier Image
resize props =
  style \(LumiTheme theme) ->
    E.css
      { width: E.int props.width
      , height: E.int props.height
      }

-- | restricted to only Thumbnail
defaultSize :: StyleModifier
defaultSize =
  style_ $ E.css
      { width: E.int 30
      , height: E.int 30
      }

round :: ImageModifier Thumbnail
round =
  style_
    $ E.css
      { borderRadius: E.percent 50.0
      }

resizeSquare :: Int -> ImageModifier Thumbnail
resizeSquare size =
  style \(LumiTheme theme) ->
    E.css
      { width: E.int size
      , height: E.int size
      }

small :: ImageModifier Thumbnail
small =
  style_
    $ E.css
      { width: E.int 24
      , height: E.int 24
      }

medium :: ImageModifier Thumbnail
medium = defaultSize

large :: ImageModifier Thumbnail
large =
  style_
    $ E.css
      { width: E.int 36
      , height: E.int 36
      }

extraLarge :: ImageModifier Thumbnail
extraLarge =
  style_
    $ E.css
      { width: E.int 140
      , height: E.int 140
      }


