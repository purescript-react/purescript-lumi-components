module AppSetup where

import React.Basic (JSX)
import React.Basic.ReactDND (createDragDropContext)
import React.Basic.ReactDND.Backends.HTML5Backend (html5Backend)

dragDropContext :: JSX -> JSX
dragDropContext = createDragDropContext html5Backend
