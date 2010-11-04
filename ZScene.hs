{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module ZScene where

import ZParticle
import ZGraphics
import ZObject
import ZVector hiding (scale)

import Data.Map
import Data.List (sortBy)
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.GL.CoordTrans

-- f is floating type, v is vector
data ZSceneRoot = ZSceneRoot {
      zSceneSkybox :: Maybe ZSkybox
    , zSceneCamera :: (Point3D GLdouble, Point3D GLdouble, Vector3D GLdouble)
    , zSceneObjects :: [ZSceneObject]
    }

data ZSkybox = ZSkybox ZObject

data ZSceneObject = ZModel ZScale ZRotate ZTranslate Int
                  | ZParticles ZParticleEngine
                  | ZLiteral (IO ()) --mostly for debugging
                  | ZEmptyScene

data ZScale = ZScale { sX :: Float , sY :: Float , sZ ::Float }
data ZRotate = ZRotate { rQuat :: Quaternion Float }
data ZTranslate = ZTranslate { tTrans :: Vector3D Float }

arrange zsr = zsr { zSceneObjects = arrangeOs (zSceneObjects zsr) }
arrangeOs = sortBy (\x y -> case (x,y) of
                              (ZParticles _, ZParticles _) -> EQ
                              (ZParticles _, _) -> GT
                              (_, ZParticles _) -> LT
                              _ -> EQ)

zNoScale = ZScale 1 1 1
zScale = ZScale

zRot = ZRotate

zNoTrans = ZTranslate $ vector3D (0,0,0)
zTrans   = ZTranslate

instance ZRenderGL ZSceneRoot where
    zRenderGL res (ZSceneRoot _ (pt,at,up) os) =
        lookAt (fromVec3D pt) (fromVec3D at) (fromVec3D up)
                   >> mapM_ (zRenderGL res) (arrangeOs os)

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
        toGL               = realToFrac -- Hey, it works.
        toGLVec            = fromVec3D . vecMap toGL
  zRenderGL res (ZParticles pe) = renderParticles res pe