module Lumi.Components.Examples.List where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Lumi.Components.Color (colorNames)
import Lumi.Components.Column (columnSelfStretch, column_)
import Lumi.Components.Images (avatar, avatar_)
import Lumi.Components.Link (link, defaults)
import Lumi.Components.List (borderlessList, compactList, defaultList, keyValueList, list, structuredColumnList)
import Lumi.Components.Lockup (lockup)
import Lumi.Components.Row (row_)
import Lumi.Components.Size (Size(..))
import Lumi.Components.Text (text, subtext, h2_, body_)
import Lumi.Components.Text as T
import Lumi.Components.Example (example)
import React.Basic (JSX, fragment)
import React.Basic.DOM as R
import Web.HTML.History (URL(..))

docs :: JSX
docs =
  column_
    [ h2_ "Basic List"
    , example
        $ columnSelfStretch
            [ list defaultList
                { rows = simpleListData
                }
            ]

    , h2_ "Basic List w/ no Borders"
    , example
        $ columnSelfStretch
            [ borderlessList defaultList
                { rows = simpleListData
                }
            ]

    , h2_ "Compact Basic List"
    , example
        $ columnSelfStretch
            [ list compactList
                { rows = simpleListData
                }
            ]

    , h2_ "Right-aligned (last column) Basic List"
    , example
        $ columnSelfStretch
            [ list defaultList
                { rightAligned = true
                , rows = simpleListData
                }
            ]

    , h2_ "Structured Column List"
    , example
        $ columnSelfStretch
            [ structuredColumnList
                { rightAligned: false
                , rows: listData
                , columns:
                    [ { renderCell: \rowData ->
                          lockup
                            { title: R.text rowData.name
                            , subtitle: Just $ R.text rowData.companyName
                            , image: Just $ avatar_ { size: Large, image: R.img { src: rowData.src } }
                            }
                      }
                    , { renderCell: \rowData ->
                          body_ rowData.createdDate
                      }
                    , { renderCell: \rowData ->
                          body_ rowData.id
                      }
                    ]
                }
            ]

    , h2_ "Right-aligned (last column) Structured Column List"
    , example
        $ columnSelfStretch
            [ structuredColumnList
                { rightAligned: true
                , rows: listData
                , columns:
                    [ { renderCell: \rowData ->
                          lockup
                            { image: Just $ avatar_ { size: Large, image: R.img { src: rowData.src } }
                            , title: R.text rowData.name
                            , subtitle: Just $ R.text rowData.companyName
                            }
                      }
                    , { renderCell: \rowData ->
                          body_ rowData.createdDate
                      }
                    , { renderCell: \rowData ->
                          fragment
                            [ link defaults
                                { href = URL "/"
                                , text = body_ "Edit"
                                , style = R.css { marginRight: "16px" }
                                }
                            , link defaults
                                { href = URL "/"
                                , text = body_ "Deactivate"
                                }
                            ]
                      }
                    ]
                }
            ]

    , h2_ "Key Value Lists"
    , example
        $ keyValueList
            { rightAligned: true
            , rows: r
            , borders: true
            }
    , example
        $ keyValueList
            { rightAligned: false
            , rows: r
            , borders: true
            }
    , example
        $ keyValueList
            { rightAligned: true
            , rows: r
            , borders: false
            }
    , example
        $ keyValueList
            { rightAligned: false
            , rows: r
            , borders: false
            }
    ]
  where
    simpleListData =
      [
        [ body_ "User"
        , row_ -- TODO: update with userLockup once lockups are corrected/updated
          [ avatar
              { image:
                  R.img
                    { src: "https://s3.amazonaws.com/lumi-flapjack-staging/mockups/9e7f08b801e6bb3a428ef72e93c49fe5.jpg"
                    }
              , size: Large
              , style: R.css { marginRight: "8px" }
              }
          , column_
              [ body_ "Flexo R."
              , text subtext
                  { style = R.css { }
                  , color = toNullable (Just colorNames.black1)
                  , children = [ R.text "Lumi" ]
                  }
              ]
          ]
        ]
      , [ body_ "ID"
        , link defaults
            { href = URL "/"
            , text = body_ "1234"
            , style = R.css { }
            }
        ]
      , [ body_ "Created"
        , body_ "2018-09-02"
        ]
      ]

    listData =
      [ { src: "https://s3.amazonaws.com/lumi-flapjack-staging/mockups/9e7f08b801e6bb3a428ef72e93c49fe5.jpg"
        , name: "Flexo R."
        , companyName: "Lumi"
        , createdDate: "2018-09-02"
        , id: "123"
        }
      , { src: "https://s3.amazonaws.com/lumi-flapjack-staging/avatars/vfhpnqairr/thumbnail_1517878176349.png"
        , name: "Bob Sagat"
        , companyName: "Full House"
        , createdDate: "2018-09-02"
        , id: "456"
        }
      , { src: "https://s3.amazonaws.com/lumi-flapjack-staging/avatars/vfhpnqairr/thumbnail_1517878176349.png"
        , name: "David Bowie"
        , companyName: "Musician"
        , createdDate: "2018-09-02"
        , id: "789"
        }
      ]

    r =
      [ { label: "Name"
        , value: T.body_ "Flexo"
        }
      , { label: "Email"
        , value: T.body_ "flexo@lumi.com"
        }
      , { label: ""
        , value: lockup
            { title: R.text "Flexo R."
            , subtitle: Just $ R.text "Lumi"
            , image: Just $ avatar_
                { size: Large
                , image:
                    R.img
                      { src: "https://s3.amazonaws.com/lumi-flapjack-staging/mockups/9e7f08b801e6bb3a428ef72e93c49fe5.jpg"
                      }
                }
            }
        }
      ]
