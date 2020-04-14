module Lumi.Components.Layouts where

import Color (cssStringHSLA)
import JSS (JSS, jss)
import Lumi.Components.Color (colors)

styles :: JSS
styles = jss
  { "@global":
      { "lumi-layout":
          { boxSizing: "border-box"
          , flex: "1"
          , display: "flex"
          , flexFlow: "column"
          , alignSelf: "stretch"

          , "& > lumi-layout-view-head":
              { boxSizing: "border-box"
              , flex: "none"
              , display: "flex"
              , flexDirection: "row"
              , padding: "12px 24px"
              , backgroundColor: cssStringHSLA colors.black6
              , borderBottom: [ "1px", "solid", cssStringHSLA colors.black4 ]
              }

          , "& > lumi-layout-view-body":
              { boxSizing: "border-box"
              , overflowY: "auto"
              , display: "flex"
              , flexFlow: "column"
              }
          }

      , "lumi-layout-centered":
          { display: "block"
          , width: "850px"
          , maxWidth: "90%"
          , margin: "5vh auto"
          }

      , "lumi-layout-centered-full-width":
          { display: "block"
          , width: "90%"
          , maxWidth: "90%"
          , margin: "5vh auto"
          }

      , "lumi-layout > lumi-layout-view-body, lumi-layout-centered, .lumi-layout-content":
          { "& lumi-section-header":
              { paddingTop: "32px"
              , paddingBottom: "12px"
              }
          }

      , "lumi-layout-view-body, lumi-layout-centered":
          { flex: "1"
          , "& lumi-section-header":
              { paddingTop: "32px"
              , paddingBottom: "12px"
              }
          }
      }
  }
