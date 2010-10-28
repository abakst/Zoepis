module ExampleObject where

import ZScene
import ZParticle
import ZGraphics
import ZVector

import Control.Monad.Random
import Random

type DT = Integer

data ExampleObject = Object {
      objUpdate :: DT -> ExampleObject
    , objScene :: ZSceneTree
    }                   

shipR = do zLoadObject 0 "resources/patrol.obj" True                       

newShip speed pos0 dir0 = ship pos0 dir0 $ zNewParticleEngine 1 750 0 myParticle myStep
    where ship pos dir pe = Object {
                              objUpdate = \dt -> updateShip dt pos dir pe
                            , objScene  = rShip pos dir pe
                            }
          myParticle = zNewParticle 0.05 0.01 0.2
          myStep     = zStepParticle myParticle 0
          updateShip _ pos dir pe =
              ship (scale speed dir + pos) dir (updateParticles pe)
          rShip pos dir pe = ZXFormNode (ZTranslate pos)
                             $ ZGroupNode [rFuselage, rEngine pe]
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
                             
