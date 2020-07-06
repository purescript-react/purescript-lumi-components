module AppSetup where

import React.Basic.Classic (JSX)
import React.Basic.ReactDND (dndProvider)
import React.Basic.ReactDND.Backends.HTML5Backend (html5Backend)

dragDropContext :: JSX -> JSX
dragDropContext = dndProvider html5Backend
