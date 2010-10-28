module ZCamera where

import ZScene
import ZVector

zMoveCamera :: Double -> Double -> ZSceneRoot -> ZSceneRoot
zMoveCamera rightLeft forBack root = root { zSceneCamera = newCamera }
    where (c,f,u) = zSceneCamera root
          forward = unit $ f - c
          right = forward `cross` u
          dDir  = realToFrac forBack `scale` forward
                  + realToFrac rightLeft `scale` right
          newCamera = (c + dDir, f + dDir, u)
          
zRotateCamera :: Double -> Double -> ZSceneRoot -> ZSceneRoot
zRotateCamera theta phi root = root { zSceneCamera = newCamera }
    where (c,f,u) = zSceneCamera root
          dTheta = realToFrac theta
          dPhi   = realToFrac phi
          dir    = unit $ f - c
          rotTheta = rotation dTheta u
          dirTheta = rotateVector rotTheta dir
          rightVec = dir `cross` u
          rotPhi   = rotation dPhi rightVec
          newCamera = (c, c + rotateVector rotPhi dirTheta, u)
          