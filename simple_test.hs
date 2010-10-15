module Main where

import ZScene
import ZObject
import ZGraphics
import ZVector
import ZGame
import ZEventMessage
import ZChannel

import Control.Concurrent
--import Control.Applicative
--import System.Posix.Unistd
import Control.Monad.State
import Data.IORef
import Data.Bits
import Graphics.Rendering.OpenGL hiding (scale)
import Graphics.UI.GLUT hiding (scale)

-- far too lazy to type --
x = xAxis
y = yAxis
z = zAxis
cameraStart = (x+4*z+2*y, origin, yAxis)

resources = zLoadObject 0 "resources/patrol.obj"
simpleScene = ZSceneRoot Nothing cameraStart ships

ships = ZGroupNode [
         ZXFormNode (ZTranslate $ xAxis+yAxis)
                        (ZObjectNode 0)
        , ZXFormNode (ZTranslate $ origin - xAxis - yAxis)
                         (ZObjectNode 0)
        , ZXFormNode (ZTranslate $ origin - 3*xAxis - 3*yAxis)
                         (ZObjectNode 0)
        , ZLiteral drawAxes
        ]

newtype KeyVector = KV Integer

data GameState = GameState {
      stGraphics    :: ZGraphicsGL ZSceneRoot
    , stLastMouse   :: (Int, Int)
    , stPressedKeys :: KeyVector
    }
                 
main = do graphics <- zInitialize "Simple Test" 800 600 simpleScene resources
          let gs = GameState { stGraphics = graphics
                             , stLastMouse = (0,0)
                             , stPressedKeys = KV 0
                             }
          x <- runStateT (gameLoop handleEvents) gs
          return $ fst x
              where gameLoop f = f >> gameLoop f

aKey = 0                                 
sKey = 1
dKey = 2
wKey = 3

setKey :: KeyVector -> Int -> KeyVector
setKey (KV k) i = KV $ k .|. bit i

clearKey :: KeyVector -> Int -> KeyVector
clearKey (KV k) i = KV $ k .&. complement (bit i)

keySet :: KeyVector -> Int -> Bool
keySet (KV k) i = testBit k i

mouseDown :: ZHandlerFunc (Button, (Int, Int)) GameState
mouseDown (_, (x,y)) = do modify (\s -> s { stLastMouse = (x,y) })
                                   
mouseMoved (x,y) = do
  gr   <- gets stGraphics
  (oldx, oldy) <- gets stLastMouse
  modify (\s -> s { stLastMouse = (x,y) })
  let theta = 0.001*fromIntegral (x-oldx)
  let phi   = 0.001*fromIntegral (y-oldy)
  let chan = gSceneChannel gr
  lift $ do (ZSceneRoot x cam@(c,f,u) t) <- zPeekChan chan
            zSwapChan chan  $ ZSceneRoot x (c, mv theta phi cam, u) t
            zUpdateGraphics
      where mv theta phi (c,f,u) = let dir = unit $ f - c
                                       rot1 = rotation (-theta) u
                                       dir' = rotateVector rot1 dir
                                       right = dir `cross` u
                                       rot2 = rotation phi right
                                   in  c + rotateVector rot2 dir'

keyPress c = do
  gr <- gets stGraphics
  kv <- gets stPressedKeys
  let kv' = case c of 
              'a' -> kv `setKey` aKey
              's' -> kv `setKey` sKey
              'd' -> kv `setKey` dKey
              'w' -> kv `setKey` wKey
              _ -> kv
  let chan = gSceneChannel gr
  lift $ do (ZSceneRoot x cam@(c,f,u) t) <- zPeekChan chan
            let forward = unit $ f - c
            let right = forward `cross` u
            let d = unit $ (keyvec kv' aKey (-right)
                            + keyvec kv' sKey (-forward)
                            + keyvec kv' dKey right
                            + keyvec kv' wKey forward)
            zSwapChan chan $ ZSceneRoot x (c+ scale 0.1 d,f+ scale 0.1 d,u) t
            zUpdateGraphics
  modify (\s -> s { stPressedKeys = kv' })
          where keyvec kv key vec = if kv `keySet` key
                                    then vec else zeroV
keyRelease c = do kv <- gets stPressedKeys
                  let kv' = case c of 
                              'a' -> kv `clearKey` aKey
                              's' -> kv `clearKey` sKey
                              'd' -> kv `clearKey` dKey
                              'w' -> kv `clearKey` wKey
                              _ -> kv
                  modify (\s -> s { stPressedKeys = kv' })

handleEvents :: ZGameLoop GameState
handleEvents = do graphics <- gets stGraphics
                  let channel = gEventChannel graphics
                  zWithEventChannel channel $ (ZEventHandler {
                                          zKeyPress = keyPress
                                        , zKeyRelease = keyRelease
                                        , zMouseDown = mouseDown
                                        , zMouseUp = zIgnoreEvent
                                        , zMouseMove = mouseMoved
                                        })
