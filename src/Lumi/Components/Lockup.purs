module Lumi.Components.Lockup where

import Prelude

import Color (cssStringHSLA)
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Nullable (notNull)
import JSS (JSS, jss)
import Lumi.Components.Color (colorNames, colors)
import Lumi.Components.Images (avatar_, productThumb_)
import Lumi.Components.Size (Size(..))
import Lumi.Components.Spacing (Space(..), hspace)
import Lumi.Components.Text as T
import React.Basic.Classic (JSX, createComponent, element, fragment, makeStateless)
import React.Basic.DOM as R

lockup
  :: { title :: JSX
     , subtitle :: Maybe JSX
     , image :: Maybe JSX
     }
  -> JSX
lockup = makeStateless (createComponent "Lockup") render
  where
    lumiLockup = element (R.unsafeCreateDOMComponent "lumi-lockup")
    lumiLockupImage = element (R.unsafeCreateDOMComponent "lumi-lockup-image")
    lumiLockupDescription = element (R.unsafeCreateDOMComponent "lumi-lockup-description")
    lumiLockupDescriptionTitle = element (R.unsafeCreateDOMComponent "lumi-lockup-description-title")
    lumiLockupDescriptionBody = element (R.unsafeCreateDOMComponent "lumi-lockup-description-body")

    render props =
      lumiLockup
        { children:
            [ props.image # foldMap \image ->
                fragment
                  [ lumiLockupImage { children: [ image ] }
                  , hspace S12
                  ]
            , lumiLockupDescription
                { children:
                    [ lumiLockupDescriptionTitle
                        { children:
                            T.text T.body
                              { children = [ props.title ]
                              }
                        }
                    , props.subtitle # foldMap \subtitle ->
                        lumiLockupDescriptionBody
                          { children:
                              T.text T.subtext
                                { color = notNull colorNames.black1
                                , children = [ subtitle ]
                                }
                          }
                    ]
                }
            ]
        }

pageLockup
  :: { title :: String
     , subtitle :: Maybe String
     , image :: Maybe JSX
     }
  -> JSX
pageLockup { title, subtitle, image } =
  lockup
    { title: T.mainHeader_ title
    , subtitle: T.body_ <$> subtitle
    , image
    }

productLockup
  :: { name :: String
     , description :: Maybe String
     , image :: JSX
     }
  -> JSX
productLockup { name, description, image } =
  lockup
    { title: R.text name
    , subtitle: R.text <$> description
    , image: Just $ productThumb_ { size: Small, image }
    }

textLockup
  :: { title :: String
     , subtitle :: Maybe String
     }
  -> JSX
textLockup { title, subtitle } =
  lockup
    { title: R.text title
    , subtitle: R.text <$> subtitle
    , image: Nothing
    }

userLockup
  :: { name :: String
     , description :: Maybe String
     , image :: JSX
     }
  -> JSX
userLockup { name, description, image } =
  lockup
    { title: R.text name
    , subtitle: R.text <$> description
    , image: Just $ avatar_ { size: Large, image }
    }

styles :: JSS
styles = jss
  { "@global":
      { "lumi-lockup":
          { display: "flex"
          , flexFlow: "row nowrap"
          , alignItems: "center"
          , minWidth: "0"

          , "& > lumi-lockup-description":
              { display: "flex"
              , flexFlow: "column"
              , minWidth: "0"
              , lineHeight: "initial"

              , "& > lumi-lockup-description-body > lumi-subtext":
                  { textDecoration: "none"
                  , display: "inline-block"
                  }

              , "& > lumi-lockup-description-title, & > lumi-lockup-description-body":
                  { whiteSpace: "nowrap"
                  , overflow: "hidden"
                  , textOverflow: "ellipsis"
                  }
              }

          , "& > lumi-lockup-image":
              { flex: "none"
              }

          , "&[data-variant=\"product\"], &[data-variant=\"user\"]":
              { "& > lumi-lockup-description > lumi-lockup-description-title":
                  { display: "flex"
                  , flexFlow: "column"
                  , "& > lumi-subtext":
                      { color: cssStringHSLA colors.black1
                      }
                  }
              }

          , "&[data-variant=\"page\"] > lumi-lockup-description > lumi-lockup-description-body":
              { color: cssStringHSLA colors.black1
              }
          }
      }
  }
