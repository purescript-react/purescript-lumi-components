module Lumi.Components.Text where

import Prelude

import Color (cssStringHSLA)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.String (Pattern(Pattern), indexOf)
import JSS (JSS, jss)
import Lumi.Components.Color (ColorName, colors)
import React.Basic.Classic (Component, JSX, ReactComponent, createComponent, element, makeStateless)
import React.Basic.DOM (CSS, css)
import React.Basic.DOM as R
import Unsafe.Coerce (unsafeCoerce)

type TextProps
  = { children :: Array JSX
    , className :: Nullable String
    , color :: Nullable ColorName
    , style :: CSS
    , tag :: String
    , testId :: Nullable String
    , title :: Nullable String
    }

component :: Component TextProps
component = createComponent "Text"

text :: TextProps -> JSX
text = makeStateless component render
  where
  render { children, className, color, style, tag, testId, title: title' } = case indexOf (Pattern "lumi-") tag of
    Just 0 ->
      element (unsafeTextTagToComponent tag)
        { children
        , "class": className
        , "data-color": color
        , "data-testid": testId
        , style
        , title: title'
        }
    _ ->
      element (unsafeTextTagToComponent tag)
        { children
        , className: "lumi" <> foldMap (" " <> _) (toMaybe className)
        , "data-color": color
        , "data-testid": testId
        , style
        , title: title'
        }

  unsafeTextTagToComponent :: forall props. String -> ReactComponent props
  unsafeTextTagToComponent = unsafeCoerce

-- Lumi-styled defaults for built in elements with padding
h1 :: TextProps
h1 = body { tag = "h1" }

h1_ :: String -> JSX
h1_ str = text h1 { children = [ R.text str ] }

h2 :: TextProps
h2 = body { tag = "h2" }

h2_ :: String -> JSX
h2_ str = text h2 { children = [ R.text str ] }

h3 :: TextProps
h3 = body { tag = "h3" }

h3_ :: String -> JSX
h3_ str = text h3 { children = [ R.text str ] }

h4 :: TextProps
h4 = body { tag = "h4" }

h4_ :: String -> JSX
h4_ str = text h4 { children = [ R.text str ] }

h5 :: TextProps
h5 = body { tag = "h5" }

h5_ :: String -> JSX
h5_ str = text h5 { children = [ R.text str ] }

h6 :: TextProps
h6 = body { tag = "h6" }

h6_ :: String -> JSX
h6_ str = text h6 { children = [ R.text str ] }

p :: TextProps
p = body { tag = "p" }

p_ :: String -> JSX
p_ str = text p { children = [ R.text str ] }

span :: TextProps
span = body { tag = "span" }

span_ :: String -> JSX
span_ str = text span { children = [ R.text str ] }

-- Lumi-specific elements with no padding
mainHeader :: TextProps
mainHeader = body { tag = "lumi-main-header" }

mainHeader_ :: String -> JSX
mainHeader_ str = text mainHeader { children = [ R.text str ] }

title :: TextProps
title = body { tag = "lumi-title" }

title_ :: String -> JSX
title_ str = text title { children = [ R.text str ] }

sectionHeader :: TextProps
sectionHeader = body { tag = "lumi-section-header" }

sectionHeader_ :: String -> JSX
sectionHeader_ str = text sectionHeader { children = [ R.text str ] }

subsectionHeader :: TextProps
subsectionHeader = body { tag = "lumi-subsection-header" }

subsectionHeader_ :: String -> JSX
subsectionHeader_ str = text subsectionHeader { children = [ R.text str ] }

body :: TextProps
body =
  { children: []
  , className: toNullable Nothing
  , color: toNullable Nothing
  , style: css {}
  , tag: "lumi-body"
  , testId: toNullable Nothing
  , title: toNullable Nothing
  }

body_ :: String -> JSX
body_ str = text body { children = [ R.text str ] }

paragraph :: TextProps
paragraph = body { tag = "lumi-paragraph" }

paragraph_ :: String -> JSX
paragraph_ str = text paragraph { children = [ R.text str ] }

subtext :: TextProps
subtext = body { tag = "lumi-subtext" }

subtext_ :: String -> JSX
subtext_ str = text subtext { children = [ R.text str ] }

nbsp :: String
nbsp = " "

styles :: JSS
styles =
  jss
    { "@global":
      { "lumi-main-header": lumiMainHeader
      , "lumi-title": lumiTitle
      , "lumi-section-header": lumiSectionHeader
      , "lumi-subsection-header": lumiSubsectionHeader
      , "body.lumi, lumi-body": lumiBody
      , "lumi-paragraph": lumiParagraph
      , "lumi-subtext": lumiSubtext
      , "h1, h2, h3, h4, h5, h6":
        { "&.lumi": { paddingBottom: "10px" }
        }
      , "h1.lumi": lumiMainHeader
      , "h2.lumi": lumiTitle
      , "h3.lumi":
        { extend: lumiSectionHeader
        , marginTop: "initial"
        }
      , "h4.lumi": lumiSubsectionHeader
      , "h5.lumi": lumiSubsectionHeader
      , "h6.lumi": lumiSubsectionHeader
      , "p.lumi":
        { extend: lumiBody
        , wordWrap: "break-word"
        , padding: "5px 0"
        }
      , "caption": lumiSubtext
      , "section.lumi": { padding: "20px" }
      , "strong.lumi": { fontWeight: "600" }
      , "lumi-main-header, lumi-title, lumi-section-header, lumi-subsection-header, lumi-body, lumi-subtext, body.lumi, h1.lumi, h2.lumi, h3.lumi, h4.lumi, h5.lumi, h6.lumi, caption.lumi, section.lumi, strong.lumi":
        { "&[data-color=\"black\"]": { color: cssStringHSLA colors.black }
        , "&[data-color=\"black-1\"]": { color: cssStringHSLA colors.black1 }
        , "&[data-color=\"black-2\"]": { color: cssStringHSLA colors.black2 }
        , "&[data-color=\"black-3\"]": { color: cssStringHSLA colors.black3 }
        , "&[data-color=\"black-4\"]": { color: cssStringHSLA colors.black4 }
        , "&[data-color=\"black-5\"]": { color: cssStringHSLA colors.black5 }
        , "&[data-color=\"black-6\"]": { color: cssStringHSLA colors.black6 }
        , "&[data-color=\"black-7\"]": { color: cssStringHSLA colors.black7 }
        , "&[data-color=\"black-8\"]": { color: cssStringHSLA colors.black8 }
        , "&[data-color=\"primary\"]": { color: cssStringHSLA colors.primary }
        , "&[data-color=\"primary-1\"]": { color: cssStringHSLA colors.primary1 }
        , "&[data-color=\"primary-2\"]": { color: cssStringHSLA colors.primary2 }
        , "&[data-color=\"primary-3\"]": { color: cssStringHSLA colors.primary3 }
        , "&[data-color=\"primary-4\"]": { color: cssStringHSLA colors.primary4 }
        , "&[data-color=\"accent-1\"]": { color: cssStringHSLA colors.accent1 }
        , "&[data-color=\"accent-1-1\"]": { color: cssStringHSLA colors.accent11 }
        , "&[data-color=\"accent-1-2\"]": { color: cssStringHSLA colors.accent12 }
        , "&[data-color=\"accent-1-3\"]": { color: cssStringHSLA colors.accent13 }
        , "&[data-color=\"accent-2\"]": { color: cssStringHSLA colors.accent2 }
        , "&[data-color=\"accent-2-1\"]": { color: cssStringHSLA colors.accent21 }
        , "&[data-color=\"accent-2-2\"]": { color: cssStringHSLA colors.accent22 }
        , "&[data-color=\"accent-2-3\"]": { color: cssStringHSLA colors.accent23 }
        , "&[data-color=\"accent-3\"]": { color: cssStringHSLA colors.accent3 }
        , "&[data-color=\"accent-3-1\"]": { color: cssStringHSLA colors.accent31 }
        , "&[data-color=\"accent-3-2\"]": { color: cssStringHSLA colors.accent32 }
        , "&[data-color=\"accent-3-3\"]": { color: cssStringHSLA colors.accent33 }
        , "&[data-color=\"white\"]": { color: cssStringHSLA colors.white }
        , "&[data-color=\"secondary\"]": { color: cssStringHSLA colors.black1 }
        , "&[data-color=\"finished\"]": { color: cssStringHSLA colors.primary }
        , "&[data-color=\"active\"]": { color: cssStringHSLA colors.accent1 }
        , "&[data-color=\"warning\"]": { color: cssStringHSLA colors.accent2 }
        , "&[data-color=\"error\"]": { color: cssStringHSLA colors.accent3 }
        }
      }
    }
  where
  lumiMainHeader =
    { fontSize: "25px"
    , lineHeight: "40px"
    , fontWeight: "600"
    }

  lumiTitle =
    { fontSize: "20px"
    , lineHeight: "32px"
    , fontWeight: "400"
    }

  lumiSectionHeader =
    { fontSize: "17px"
    , lineHeight: "24px"
    , fontWeight: "600"
    , display: "inline-block"
    }

  lumiSubsectionHeader =
    { fontSize: lumiSubsectionHeaderFontSize
    , lineHeight: "24px"
    , fontWeight: "600"
    }

  lumiBody =
    { fontSize: "14px"
    , lineHeight: "20px"
    , fontWeight: "400"
    }

  lumiParagraph =
    { fontSize: "14px"
    , lineHeight: "24px"
    }

  lumiSubtext =
    { fontSize: lumiSubtextFontSize
    , lineHeight: "16px"
    , fontWeight: "400"
    }

lumiSubsectionHeaderFontSize :: String
lumiSubsectionHeaderFontSize = "15px"

lumiSubtextFontSize :: String
lumiSubtextFontSize = "13px"