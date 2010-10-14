{-# LANGUAGE ForeignFunctionInterface #-}
module ZObject(ZObject(ZObject), zLoadObjectFile) where

import Control.Applicative
import Graphics.Rendering.OpenGL
import Foreign hiding (rotate)
import Foreign.C

glmNONE     = 0 
glmFLAT     = 1
glmSMOOTH   = 2
glmTEXTURE  = 4
glmCOLOR    = 8
glmMATERIAL = 16

newtype ZObject = ZObject DisplayList

foreign import ccall unsafe "glmReadOBJ" glmReadOBJ :: CString -> IO (Ptr a)
foreign import ccall unsafe "glmList" glmList :: Ptr a -> GLuint -> IO (GLuint)
foreign import ccall unsafe "glmDraw" glmDraw :: Ptr a -> GLuint -> IO ()
foreign import ccall unsafe "glmUnitize" glmUnitize :: Ptr a -> IO GLfloat

zLoadObjectFile :: String -> IO ZObject
zLoadObjectFile objfile = withCString objfile $ \str -> do
                            ptr <- glmReadOBJ str
                            glmUnitize ptr
                            lst <- glmList ptr 22
                            return $ ZObject (DisplayList lst)
