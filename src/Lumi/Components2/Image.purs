module Lumi.Components2.Image
  ( image
  , thumbnail
  , small
  , medium
  , large
  , extraLarge
  , smallAvatar
  , mediumAvatar
  , largeAvatar
  , resize
  , resizeSquare
  , round
  , cover
  , contain
  , scaleDown
  , ImageProps
  , ThumbnailProps
  , Image(..)
  , Thumbnail(..)
  ) where

import Prelude

import Data.Foldable (minimum)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid as Monoid
import Data.Nullable as Nullable
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Effect.Timer (clearTimeout, setTimeout)
import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components (LumiComponent, PropsModifier, lumiComponent, propsModifier, ($$$))
import Lumi.Components.Loader (loader)
import Lumi.Components.Svg (placeholderSvg)
import Lumi.Components2.Box as Box
import Lumi.Styles (Style, StyleModifier, color, style, style_, toCSS)
import Lumi.Styles.Box (FlexAlign(..))
import Lumi.Styles.Box as Styles.Box
import Lumi.Styles.Slat hiding (_interactive,slat) as Styles.Slat
import Lumi.Styles.Theme (LumiTheme(..), useTheme)
import React.Basic.DOM as R
import React.Basic.Emotion as E
import React.Basic.Events (handler_)
import React.Basic.Hooks (JSX)
import React.Basic.Hooks as Hooks
import Web.DOM.Element (getBoundingClientRect) as HTMLElement
import Web.HTML.HTMLElement as HTMLElement

data Image = Image

type ImageProps
  = ( component :: Image
    , content :: String
    , placeholder :: Maybe JSX
    , imgStyle :: Style
    )

-- | An image has no size restrictions
-- | will respect the image's original aspect ratio
image :: LumiComponent ImageProps
image =
  unsafePerformEffect do
    lumiComponent "Image" defaults \props -> Hooks.do
      theme <- useTheme
      loaded /\ setLoaded <- Hooks.useState' false
      error /\ setError <- Hooks.useState' false

      containerRef <- Hooks.useRef Nullable.null
      dimensions /\ setDimensions <- Hooks.useState { width: 20.0, height: 20.0 }
      Hooks.useLayoutEffect unit do
        container <- Hooks.readRefMaybe containerRef
        case container of
          Nothing -> mempty
          Just container' -> do
            rect <- traverse HTMLElement.getBoundingClientRect (HTMLElement.toElement <$> HTMLElement.fromNode container')
            let
              newDimentions =
                { height: maybe 0.0 (\r -> r.height + 2.0) rect
                , width: maybe 0.0 (\r -> r.width + 2.0) rect
                }
            if newDimentions /= dimensions then do
              timeout <-
                setTimeout 10 do
                  setDimensions \_ -> newDimentions
              pure (clearTimeout timeout)
            else
              mempty

      pure
        $ E.element R.div'
            { ref: containerRef
            , children:
                [ if String.null props.content
                    then fromMaybe placeholder props.placeholder
                    else
                      Box.column
                      $ Styles.Box._flex
                      $ Styles.Box._align Center
                      $ Styles.Box._justify Center
                      $$$
                        [ Monoid.guard (not loaded && not error)
                            $ let d = fromMaybe 20.0 ([ dimensions.height, dimensions.width ] # minimum)
                              in loader
                                { style: R.css
                                    { width: d * 0.25
                                    , height: d * 0.25
                                    , borderWidth: "2px"
                                    }
                                , testId: Nullable.toNullable Nothing
                                }
                        , E.element R.img'
                            { src: props.content
                            , className: ""
                            , css: E.css
                                { objectFit: E.str "cover"
                                , display: E.str $ if loaded || error then "block" else "none"
                                }
                                <> if error
                                    then E.css
                                      { width: E.str "auto"
                                      , height: E.str "auto"
                                      }
                                    else E.css
                                      { height: E.percent 100.0
                                      , width: E.percent 100.0
                                      }
                                <> props.imgStyle
                            , onLoad: handler_ $ setLoaded true
                            , onError: handler_ $ setError true
                            }
                        ]
                ]
            , className: props.className
            , css: defaultImageStyle theme <> props.css theme
            }
    where
      defaults =
        { component: Image
        , content: ""
        , placeholder: Nothing
        , imgStyle: E.css {}
        }

      defaultImageStyle :: LumiTheme -> Style
      defaultImageStyle theme@(LumiTheme { colors }) =
        E.css
          { boxSizing: E.borderBox
          , overflow: E.hidden
          , display: E.flex
          , flexFlow: E.column
          , alignItems: E.center
          , justifyContent: E.center
          , border: E.str "1px solid"
          , borderColor: E.color colors.black4
          }

data Thumbnail = Thumbnail

type ThumbnailProps
  = ( component :: Thumbnail
    , content :: String
    , placeholder :: Maybe JSX
    , imgStyle :: Style
    )

-- | A thumbnail can support size restrictions
-- | will always have a square aspect ratio
thumbnail :: LumiComponent ThumbnailProps
thumbnail =
  unsafePerformEffect do
    lumiComponent "Thumbnail" defaults \props -> Hooks.do
      pure
        $ image
        $ _ { content = props.content
            , placeholder = props.placeholder
            , css = toCSS defaultSize <> props.css
            , imgStyle = props.imgStyle
            }

      where
        defaults =
          { component: Thumbnail
          , content: ""
          , placeholder: Nothing
          , imgStyle: E.css {}
          }

-- | The `c` type parameter lets us constrain the type of component to which
-- | an image modifier may be applied
type ImageModifier c = forall r. PropsModifier (component :: c | r)

-- | restricted to only Image
resize :: { width :: Int, height :: Int } -> ImageModifier Image
resize props =
  style \(LumiTheme theme) ->
    E.css
      { width: E.px props.width
      , height: E.px props.height
      }

-- | restricted to only Thumbnail
round :: ImageModifier Thumbnail
round = style_ mkRound

cover :: PropsModifier ThumbnailProps
cover =
  propsModifier  \props -> props
    { imgStyle = props.imgStyle <> E.css { objectFit: E.str "cover" }
    }

contain :: PropsModifier ThumbnailProps
contain =
  propsModifier  \props -> props
    { imgStyle = props.imgStyle <> E.css { objectFit: E.str "contain" }
    }

scaleDown :: PropsModifier ThumbnailProps
scaleDown =
  propsModifier  \props -> props
    { imgStyle = props.imgStyle <> E.css { objectFit: E.str "scale-down" }
    }

resizeSquare :: Int -> ImageModifier Thumbnail
resizeSquare size =
  style \(LumiTheme theme) ->
    E.css
      { width: E.px size
      , height: E.px size
      }

small :: ImageModifier Thumbnail
small =
  style_
    $ E.css
      { width: E.px 40
      , height: E.px 40
      }

medium :: ImageModifier Thumbnail
medium = defaultSize

defaultSize :: StyleModifier
defaultSize =
  style_ $ E.css
      { width: E.px 56
      , height: E.px 56
      }

large :: ImageModifier Thumbnail
large =
  style_
    $ E.css
      { width: E.px 72
      , height: E.px 72
      }

extraLarge :: ImageModifier Thumbnail
extraLarge =
  style_
    $ E.css
      { width: E.px 140
      , height: E.px 140
      }

-- | we support two sizing scales for thumbnails
-- | avatars use a smaller scale, whereas all other images use the above size scale
smallAvatar :: ImageModifier Thumbnail
smallAvatar =
  style_
    $ E.merge
      [ E.css
          { width: E.px 24
          , height: E.px 24
          }
      , mkRound
      ]

mediumAvatar :: ImageModifier Thumbnail
mediumAvatar =
  style_
    $ E.merge
      [ E.css
          { width: E.px 30
          , height: E.px 30
          }
      , mkRound
      ]

largeAvatar :: ImageModifier Thumbnail
largeAvatar =
  style_
    $ E.merge
      [ E.css
          { width: E.px 36
          , height: E.px 36
          }
      , mkRound
      ]

mkRound :: Style
mkRound = E.css { borderRadius: E.percent 50.0 }

placeholder :: JSX
placeholder =
  Box.column
  $ Styles.Box._align Center
  $ Styles.Box._justify Center
  $ Styles.Box._flex
  $ style do
    \(LumiTheme { colors }) -> (E.css { backgroundColor: color colors.black6, width: E.percent 100.0 })
  $$$
    [ Box.box
      $ style_ (E.css { width: E.percent 50.0 })
      $$$ [ placeholderSvg ]
    ]
