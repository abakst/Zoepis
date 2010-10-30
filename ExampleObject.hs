module ExampleObject(DT, ExampleObject(..), newShip, shipR) where

import ZScene
import ZParticle
import ZGraphics
import ZVector
import ZRandom

type DT = Integer

data ExampleObject = Object {
      objUpdate :: DT -> ExampleObject
    , objScene :: ZSceneTree
    }                   
                     
data PhysicalState = PS {
      psPos :: Vector3D Double
    , psVel :: Vector3D Double
    , psAcc :: Vector3D Double
    , psUp  :: Vector3D Double
    }

shipR = do zLoadObject 0 "resources/patrol.obj" True                       

newShip x0 v0 a0 seed = ship (PS x0 v0 a0 yAxis) rng0 pe0
    where
      rng0 = zRandGen seed 
      pe0 = zNewParticleEngine 1 750 0 myParticle myStep
      myParticle = zNewParticle 0.05 0.01 0.1 (return (0.3, 0.6, 0.1))
      myStep     = zStepParticle myParticle 0
                   
ship ps rng pe = Object {
                   objUpdate = updateShip ps rng pe
                 , objScene  = renderShip ps pe
                 }

                                 
updateShip :: PhysicalState -> 
              ZRandGen ->
              ZParticleEngine ->
              DT ->
              ExampleObject
updateShip ps rng pe dt = ship ps' rng' pe'
    where
      pe' = updateParticles pe 
      p' = psPos ps + (psVel ps)
      v' = psVel ps + (scale 0.003 $ psAcc ps)
      v'' = if zMagSq v' > 0.1
            then scale (sqrt (0.1/zMagSq v')) v'
            else v'
      a' = if (zMagSq $ psAcc ps) < 0.1
             then origin
             else psAcc ps
      right = zCross (unit $ psVel ps) (unit $ psUp ps)
      (a'', rng') = zRunRand (do p <- zGetRandomR (0, 1)
                                 (if p < (0.5::Double)
                                  then do s <- zGetRandomR (-0.2, 0.6)
                                          return $ scale s right + a'
                                  else return a')) rng
      ps' = PS p' v'' a'' (psUp ps)
      

renderShip :: PhysicalState ->
              ZParticleEngine ->
              ZSceneTree
renderShip (PS pos vel acc up) pe =
    if zMagSq acc > 0.1
    then
        ZXFormNode (ZTranslate $ vecMap realToFrac pos)
        $ ZXFormNode (ZRotate rollV roll')
          $ ZXFormNode (ZRotate rotV rotDeg') 
            $ ZGroupNode [rFuselage, rEngine pe]
    else
        ZXFormNode (ZTranslate $ vecMap realToFrac pos)
          $ ZXFormNode (ZRotate rotV rotDeg') 
            $ ZGroupNode [rFuselage, rEngine pe]
    where
      rollDir = (unit (unit vel `zCross` unit acc)) `zDot` up
      rollb   = zMagSq (unit vel `zCross` unit acc) > 0.001
      rollS   = (*  min (0.001*zMagSq acc / zMagSq vel) 0.5)
      roll = realToFrac.rollS.toDeg.acos $ (unit vel `zDot` unit acc)
      roll'= if rollDir <= 0
             then roll
             else 360 - roll
      rollV = vecMap realToFrac (unit vel)
      rotV = vecMap realToFrac up
      right = unit vel `zCross` up
      rotDeg = realToFrac . toDeg . acos $ (unit vel `zDot` zAxis)
      rotDeg' = if vecX vel >= 0
                then rotDeg
                else 360 - rotDeg
      rFuselage = ZObjectNode 0
      rEngine pe = ZGroupNode
                   [ ZXFormNode (ZTranslate $ (scale 0.25 yAxis)
                                                + (scale 0.22 xAxis))
                   $ ZXFormNode (ZRotate xAxis 270)
                         $ ZParticles pe
                   , ZXFormNode (ZTranslate $ (scale 0.25 yAxis)
                                                - (scale 0.22 xAxis))
                                    $ ZXFormNode (ZRotate xAxis 270)
                                          $ ZParticles pe
                   ]                             
