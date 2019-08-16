module JSS
  ( JSSConfig
  , JSSInstance
  , JSSStyleSheet
  , JSS
  , JSSValue
  , jss
  , createInstance
  , preset
  , createStyleSheet
  , globalAttachStyleSheet
  , toStringStyleSheet
  , important
  , jssValue
  ) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Unsafe.Coerce (unsafeCoerce)

foreign import data JSSConfig :: Type

foreign import data JSSInstance :: Type

foreign import data JSSStyleSheet :: Type

foreign import data JSS :: Type

foreign import data JSSValue :: Type

jss :: forall styles. {| styles } -> JSS
jss = unsafeCoerce

createInstance :: Effect JSSConfig -> Effect JSSInstance
createInstance = runEffectFn1 createInstance_

foreign import createInstance_ :: EffectFn1 (Effect JSSConfig) JSSInstance

foreign import preset :: Effect JSSConfig

createStyleSheet :: JSSInstance -> JSS -> Effect JSSStyleSheet
createStyleSheet = runEffectFn2 createStyleSheet_

foreign import createStyleSheet_ :: EffectFn2 JSSInstance JSS JSSStyleSheet

globalAttachStyleSheet :: JSSStyleSheet -> Effect Unit
globalAttachStyleSheet = runEffectFn1 globalAttachStyleSheet_

foreign import globalAttachStyleSheet_ :: EffectFn1 JSSStyleSheet Unit

foreign import toStringStyleSheet :: JSSStyleSheet -> String

-- | Allows the mixing of types in the values passed to JSS.
-- | For example, to create `!important` properties JSS
-- | sometimes requires heterogeneous arrays:
-- |
-- | ```purs
-- | [ [ "10px" ], "!important" ]
-- | ```
jssValue :: forall a. a -> JSSValue
jssValue = unsafeCoerce

important :: forall a. a -> JSSValue
important a = jssValue [ jssValue [ a ], jssValue "!important" ]
