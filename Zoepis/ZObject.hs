{-# LANGUAGE ForeignFunctionInterface #-}
module Zoepis.ZObject(ZObject(ZObject), zLoadObjectFile) where

import Control.Monad
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

foreign import ccall safe "glmReadOBJ" glmReadOBJ :: CString -> IO (Ptr a)
foreign import ccall safe "glmList" glmList :: Ptr a -> GLuint -> IO GLuint
foreign import ccall safe "glmDraw" glmDraw :: Ptr a -> GLuint -> IO ()
foreign import ccall safe "glmUnitize" glmUnitize :: Ptr a -> IO GLfloat

zLoadObjectFile :: String -> Bool -> IO ZObject
zLoadObjectFile objfile unit = withCString objfile $ \str -> do
                            ptr <- glmReadOBJ str
                            when unit $ glmUnitize ptr >> return ()
                            lst <- glmList ptr 22
                            return $ ZObject (DisplayList lst)
