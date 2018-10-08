module Graphics.WebGL where

import Prelude (Unit, bind, not, when, ($), (&&), (<$>))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.Reader.Trans (runReaderT)
import Data.Either (Either ())
import Effect (Effect)

import Graphics.WebGL.Raw.Types as Raw

import Graphics.WebGL.Methods (getError, isContextLost)
import Graphics.WebGL.Shader (compileShadersIntoProgram, getAttrBindings, getUniformBindings)
import Graphics.WebGL.Types (ErrorCode(..), WebGL, WebGLContext, WebGLError(..), WebGLProgram, fromWebglEnum)

runWebgl :: forall eff a. WebGL a -> Raw.WebGLContext -> Effect (Either WebGLError a)
runWebgl f ctx = runExceptT $ runReaderT f ctx

runWebglWithShaders :: forall eff attrs uniforms a. (WebGLProgram -> Record attrs -> Record uniforms -> WebGL a) -> WebGLContext -> String -> String -> Effect (Either WebGLError a)
runWebglWithShaders f ctx vertSrc fragSrc = runWebgl (do
    prog <- compileShadersIntoProgram vertSrc fragSrc
    attr <- getAttrBindings prog
    unif <- getUniformBindings prog
    f prog attr unif) ctx

debug :: WebGL Unit
debug = do
    hasCtx <- not <$> isContextLost
    err <- fromWebglEnum <$> getError
    when (hasCtx && hasErr err) (throwError $ ErrorCode err)
  where
    hasErr NoError = false
    hasErr _       = true
