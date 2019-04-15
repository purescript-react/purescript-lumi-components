module Lumi.Components.Examples.ScrollManager where

import Lumi.Components.Column (column_)
import Lumi.Components.Text (p_)
import Lumi.Components.Example (exampleCode)
import React.Basic (JSX)

docs :: JSX
docs =
  column_
    [ p_ "Used to normalize forward/back button scroll behavior across browsers."
    , p_ "`ScrollManager` should be used at the root of the application, just below the `react-router` `Router` component, as it only supports Window scrolling. For example:"
    , exampleCode """ReactDOM.render(
  <HashRouter>
    <ScrollManager>
      <App />
    </ScrollManager>
  </HashRouter>,
  document.getElementById("root")
);
"""
    ]
