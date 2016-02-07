module Graphics.WebGL.Unsafe where

import Prelude (Unit)
import Control.Monad.Eff (Eff ())

foreign import unsafeCoerce :: forall a b. a -> b
