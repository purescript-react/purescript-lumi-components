module Lumi.Components2.Text where

import Prelude

import Data.Lens (Setter')
import Effect.Unsafe (unsafePerformEffect)
import Lumi.Components (LumiComponent, LumiProps, PropsModifier, lumiComponent, propsModifier)
import Lumi.Styles as S
import Lumi.Styles.Theme (LumiTheme(..), useTheme)
import React.Basic (JSX, ReactComponent)
import React.Basic.DOM as R
import React.Basic.Emotion as E
import React.Basic.Hooks as Hooks

-- Text

data Text = Text

type TextProps =
  ( component :: Text
  , element :: TextElement
  , content :: String
  )

type TextElement = ReactComponent { children :: Array JSX, className :: String }

text :: LumiComponent TextProps
text =
  unsafePerformEffect $ lumiComponent "Text" defaults \props -> Hooks.do
    theme <- useTheme
    pure $
      E.element props.element
        { children: [ R.text props.content ]
        , className: props.className
        , css: defaultTextStyle <> props.css theme
        }
  where
    defaults :: Record TextProps
    defaults =
      { component: Text
      , element: R.span'
      , content: ""
      }

    defaultTextStyle =
      S.css
        { fontSize: S.str "14px"
        , lineHeight: S.str "17px"
        , margin: S.str "0"
        , padding: S.str "0"
        }

type TextModifier = forall r. PropsModifier (component :: Text, element :: TextElement | r)

_body :: TextModifier
_body = propsModifier _{ element = R.span' :: TextElement }

_strong :: TextModifier
_strong = propsModifier _{ element = R.strong' :: TextElement }

_emphasis :: TextModifier
_emphasis = propsModifier _{ element = R.em' :: TextElement }

_subtext :: TextModifier
_subtext =
  propsModifier _{ element = R.small' :: TextElement }
  <<< S.style_
        ( S.css
            { fontSize: S.str "12px"
            , lineHeight: S.str "14px"
            , marginBottom: S.str "6px"
            }
        )

_muted :: TextModifier
_muted =
  S.style \(LumiTheme { colors }) ->
    S.css { color: S.color colors.black1 }

-- Paragraph

type ParagraphProps =
  ( component :: Text
  , element :: TextElement
  , content :: Array JSX
  )

paragraph_ :: LumiComponent TextProps
paragraph_ = paragraph <<< renderInnerText
  where
    renderInnerText :: Setter' (LumiProps ParagraphProps) (LumiProps TextProps)
    renderInnerText f props =
      let
        props' = f $ props { content = "" }
      in
        props'
          { content =
              [ text _
                  { element = props'.element
                  , content = props'.content
                  , css = \_ -> S.css { fontSize: S.str "inherit", lineHeight: S.str "inherit" }
                  }
              ]
          }

paragraph :: LumiComponent ParagraphProps
paragraph =
  unsafePerformEffect $ lumiComponent "Paragraph" defaults \props -> Hooks.do
    theme <- useTheme
    pure $
      E.element R.p'
        { children: props.content
        , className: props.className
        , css: defaultParagraphStyle <> props.css theme
        }
  where
    defaults :: Record ParagraphProps
    defaults =
      { component: Text
      , element: R.span'
      , content: []
      }

    defaultParagraphStyle =
      S.css
        { fontSize: S.str "14px"
        , lineHeight: S.str "17px"
        , margin: S.str "0"
        , marginBottom: S.str "7px"
        , padding: S.str "0"
        }

-- Headers

type HeaderProps = (content :: String)

subsectionHeader :: LumiComponent HeaderProps
subsectionHeader = mkHeaderComponent R.h4'

sectionHeader :: LumiComponent HeaderProps
sectionHeader =
  mkHeaderComponent R.h3'
  <<< S.style_
        ( S.css
            { fontSize: S.str "20px"
            , lineHeight: S.str "24px"
            , marginBottom: S.str "10px"
            }
        )

title :: LumiComponent HeaderProps
title =
  mkHeaderComponent R.h2'
  <<< S.style_
        ( S.css
            { fontSize: S.str "24px"
            , lineHeight: S.str "29px"
            , marginBottom: S.str "12px"
            }
        )

mainHeader :: LumiComponent HeaderProps
mainHeader =
  mkHeaderComponent R.h1'
  <<< S.style_
        ( S.css
            { fontSize: S.str "30px"
            , lineHeight: S.str "36px"
            , marginBottom: S.str "15px"
            }
        )

mkHeaderComponent
  :: TextElement
  -> LumiComponent HeaderProps
mkHeaderComponent el =
  unsafePerformEffect $ lumiComponent "Header" { content: "" } \props -> Hooks.do
    theme <- useTheme
    pure $
      E.element el
        { children: [ R.text props.content ]
        , className: props.className
        , css: defaultHeaderStyle <> props.css theme
        }
  where
    defaultHeaderStyle =
      S.css
        { fontSize: S.str "17px"
        , fontWeight: S.str "400"
        , lineHeight: S.str "20px"
        , padding: S.str "0"
        , margin: S.str "0"
        , marginBottom: S.str "9px"
        }
