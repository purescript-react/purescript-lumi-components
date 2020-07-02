module Lumi.Components.Breadcrumb
  ( breadcrumb
  , compactBreadcrumb
  , styles
  ) where

import Prelude

import Color (cssStringHSLA)
import Data.Array (null, (:))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Nullable (Nullable, toMaybe)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Effect.Unsafe (unsafePerformEffect)
import JSS (JSS, jss)
import Lumi.Components.Color (colors)
import Lumi.Components.Link as Link
import React.Basic.Classic (JSX, createComponent, element, makeStateless)
import React.Basic.DOM as R
import Web.HTML.History (URL)

type BreadcrumbProps =
  { onNavigate :: Nullable (EffectFn1 URL Unit)
  , items ::
      NonEmptyArray
        { label :: String
        , href :: URL
        }
  }

breadcrumb :: BreadcrumbProps -> JSX
breadcrumb { onNavigate, items } =
  genericBreadcrumb { onNavigate, items, compact: false }

compactBreadcrumb :: BreadcrumbProps -> JSX
compactBreadcrumb { onNavigate, items } =
  genericBreadcrumb { onNavigate, items, compact: true }

genericBreadcrumb
  :: { onNavigate :: Nullable (EffectFn1 URL Unit)
     , items ::
         NonEmptyArray
           { label :: String
           , href :: URL
           }
     , compact :: Boolean
     }
  -> JSX
genericBreadcrumb = makeStateless (createComponent "Breadcrumb") render
  where
    lumiBreadcrumb = element (unsafePerformEffect $ R.unsafeCreateDOMComponent "lumi-breadcrumb")
    render props@{ compact } =
      lumiBreadcrumb
        { class: guard compact "compact"
        , children:
            intercalate separator $
              if compact then
                renderCompactVariant props
              else
                renderNormalVariant props
        }

    renderNormalVariant { items, onNavigate } =
      let
        { init, last } = NEA.unsnoc items
      in
        map (pure <<< toLink onNavigate) init <> [ [ R.span_ [ R.text last.label ] ] ]

    renderCompactVariant { items, onNavigate } =
      let
        { head, tail } = NEA.uncons items
      in
        if null tail then
          [ backLink onNavigate head ] : map (pure <<< toLink onNavigate) tail
        else
          NEA.toArray $ map (pure <<< toLink onNavigate) items

    backLink onNavigate { label, href } =
      Link.link Link.defaults
        { className = Just "lumi-breadcrumb-back"
        , href = href
        , text = R.text label
        , navigate = do
            n <- toMaybe onNavigate
            pure (runEffectFn1 n href)
        }

    toLink onNavigate { label, href } =
      Link.link Link.defaults
        { href = href
        , text = R.text label
        , navigate = do
            n <- toMaybe onNavigate
            pure (runEffectFn1 n href)
        }

    separator =
      [ R.span
          { className: "lumi-breadcrumb-separator"
          , children: [ R.text "/" ]
          }
      ]

styles :: JSS
styles = jss
  { "@global":
      { "lumi-breadcrumb":
          { fontSize: "20px"
          , lineHeight: "32px"
          , fontWeight: "400"
          , color: cssStringHSLA colors.black1

          , "& > :last-child": { color: cssStringHSLA colors.black }

          , "&.compact":
              { fontSize: "14px"
              , lineHeight: "20px"
              , "& :last-child": { color: cssStringHSLA colors.black1 }
              }

          , "& > a.lumi, & > a.lumi:visited":
              { color: cssStringHSLA colors.black1
              , "&:hover":
                  { color: cssStringHSLA colors.black
                  , textDecoration: "none"
                  }
              }

          , "& > a.lumi:hover": { textDecoration: "none" }

          , "& > .lumi-breadcrumb-separator": { margin: "0 10px" }

          , "& > a.lumi-breadcrumb-back":
              { "&:hover::before":
                  { borderColor: cssStringHSLA colors.black
                  }
              , "&::before":
                  { display: "inline-block"

                  , content: "\"\""
                  , verticalAlign: "baseline"

                  , borderTop: [ "1px", "solid", cssStringHSLA colors.black1 ]
                  , borderRight: "none"
                  , borderBottom: "none"
                  , borderLeft: [ "1px", "solid", cssStringHSLA colors.black1 ]
                  , transform: "rotate(-45deg)"

                  , width: "8px"
                  , height: "8px"
                  , margin: "0 7px 0 2px"
                  }
              }
          }
      }
  }
