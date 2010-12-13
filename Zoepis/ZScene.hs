{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Zoepis.ZScene where

import Zoepis.ZParticles
import Zoepis.ZGraphics
import Zoepis.ZObject
import Zoepis.ZVector hiding (scale)

import Data.Map
import Data.List (sortBy)
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.GL.CoordTrans
import Unsafe.Coerce
import Control.Monad
import Data.Maybe 

data ZSceneRoot = ZSceneRoot {
      zSceneSkybox :: Maybe ZSkybox
    , zSceneCamera :: (Point3D GLdouble, Point3D GLdouble, Vector3D GLdouble)
    , zSceneObjects :: [ZSceneObject]
    }

data ZSkybox = ZSkybox Int

data ZSceneObject = ZModel !ZScale !ZRotate !ZTranslate !Int
                  | ZParticles !Float !Int ![ZParticle]
                  | ZLiteral (IO ()) --mostly for debugging
                  | ZEmptyScene

data ZScale = ZScale { sX :: Float , sY :: Float , sZ ::Float }
data ZRotate = ZRotate { rQuat :: Quaternion Float }
data ZTranslate = ZTranslate { tTrans :: Vector3D Float }

arrange zsr = zsr { zSceneObjects = arrangeOs (zSceneObjects zsr) }
arrangeOs = sortBy (\x y -> case (x,y) of
                              (ZParticles _ _ _, ZParticles _ _ _) -> EQ
                              (ZParticles _ _ _, _) -> GT
                              (_, ZParticles _ _ _) -> LT
                              _ -> EQ)

zNoScale = ZScale 1 1 1
zScale = ZScale

zRot = ZRotate

zNoTrans = ZTranslate $ vector3D (0,0,0)
zTrans   = ZTranslate

instance ZRenderGL ZSceneRoot where
    zRenderGL res (ZSceneRoot sb (pt,at,up) os) = do
      lookAt (fromVec3D pt) (fromVec3D at) (fromVec3D up)
      when (isJust sb) $ preservingMatrix $ do
        let (Just (ZSkybox objid)) = sb
        depthMask $= Disabled
        textureFunction $= Decal
        textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
        textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
        translate (fromVec3D pt)
        let (ZObject dlist) = gDisplayLists res ! objid
        callList dlist
        depthMask $= Enabled
      mapM_ (zRenderGL res) (arrangeOs os)
      

instance ZRenderGL ZSceneObject where
  zRenderGL _ ZEmptyScene  = return ()
  zRenderGL _ (ZLiteral a) = a
  zRenderGL res (ZModel s r t objid) = preservingMatrix $ do
    translate .  toGLVec $ tTrans t
    rotate (toGL .(*(180/pi)). angleOfRot $ rQuat r) (toGLVec . axisOfRot $ rQuat r)
    scale (toGL $ sX s ) (toGL $ sY s) (toGL $ sZ s)
    callList dlist
      where
        (ZObject dlist)    = gDisplayLists res ! objid
        toGL :: Real a => a -> GLfloat
        toGL               = unsafeCoerce
        toGLVec            = fromVec3D . vecMap toGL
  zRenderGL res (ZParticles sz tex ps) = zRenderParticles res sz tex ps