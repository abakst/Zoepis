{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module ZScene where

import ZGraphics
import ZObject
import ZVector hiding (scale)

import Data.Map
import Graphics.Rendering.OpenGL
import Graphics.Rendering.OpenGL.GL.CoordTrans

-- f is floating type, v is vector
data ZSceneRoot = ZSceneRoot {
      zSceneSkybox :: Maybe ZSkybox
    , zSceneCamera :: (Point3D GLdouble, Point3D GLdouble, Vector3D GLdouble)
    , zSceneTree   :: ZSceneTree
    }

data ZSkybox = ZSkybox ZObject

data ZSceneTree = ZObjectNode Int
                | ZXFormNode ZTransform ZSceneTree
                | ZGroupNode [ZSceneTree]
                | ZLiteral (IO ()) --mostly for debugging
                | ZEmptyScene      --mostly for debugging
                      
data ZTransform = ZScale GLfloat GLfloat GLfloat
                | ZRotate (Vector3D GLfloat) GLfloat
                | ZTranslate (Vector3D GLfloat)
                      
class IsoVecTo a where    
    fromVec3D :: (Vector3D f) -> (a f)
    
instance IsoVecTo Vector3 where
    fromVec3D v = Vector3 (vecX v) (vecY v) (vecZ v)
    
instance IsoVecTo Vertex3 where
    fromVec3D v = Vertex3 (vecX v) (vecY v) (vecZ v)    
  
instance ZRenderGL ZSceneRoot where
  zRenderGL res (ZSceneRoot _ (pt,at,up) st) =
      lookAt (fromVec3D pt) (fromVec3D at) (fromVec3D up) >> zRenderGL res st
  
instance ZRenderGL ZSceneTree where
  zRenderGL _ ZEmptyScene  = return ()
  zRenderGL _ (ZLiteral a) = a
  zRenderGL res (ZObjectNode objid) = callList dlist
      where
        (ZObject dlist) = gDisplayLists res ! objid
  zRenderGL res (ZGroupNode g) = mapM_ (zRenderGL res) g
  zRenderGL res (ZXFormNode xform t) =
      preservingMatrix $ applyTransform xform >> zRenderGL res t
          where
            applyTransform (ZScale x y z) = scale x y z
            applyTransform (ZRotate v angle) = rotate angle (fromVec3D v)
            applyTransform (ZTranslate v) = translate $ (fromVec3D v)
