module Main where

import ZScene
import ZObject
import ZGraphics
import ZVector
import ZGame
import ZChannel
import ZEventMessage

import Control.Monad.State
import Data.Bits
import Control.Concurrent
import System.Exit
import System.CPUTime

tPrec = 100000000

nTicks t = getCPUTime >>= (return . (-t). (`div` tPrec))

cameraStart = (xAxis+4*zAxis+2*yAxis, origin, yAxis)
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
      stGraphics    :: (ZGraphicsGL ZSceneRoot)
    , stLastMouse   :: !(Int, Int)
    , stMouseV      :: !(Int, Int)
    , stPressedKeys :: !KeyVector
    , stLastTick    :: !Integer
    }

main = do graphics <- zInitialize "Test" 800 600 simpleScene resources
          time <- getCPUTime
          let gs = GameState { stGraphics = graphics
                             , stLastMouse = (0,0)
                             , stPressedKeys = KV 0
                             , stMouseV = (0,0)
                             , stLastTick = time `div` tPrec
                             }
          gameloop gs myGame
              where gameloop st lp = do st' <- zGameLoopStep st lp
                                        gameloop st' lp

myGame :: ZGameLoop GameState
myGame = do handleEvents
            moveCamera
            whenDt 1000 $ (gets stLastTick >>= (lift . print))
            lift zUpdateGraphics

whenDt :: Integer -> ZGameLoop GameState -> ZGameLoop GameState
whenDt dt a = do t0 <- gets stLastTick
                 t1 <- lift $ getCPUTime >>= (return.(`div` tPrec))
                 when (t1 - t0 > dt)
                          (modify (\s -> s{ stLastTick = t1 }) >> a)
    
zSpawnEventThread handler = forkIO eventLoop
    where eventLoop = handler >> eventLoop

aKey = 0                                 
sKey = 1
dKey = 2
wKey = 3

setKey :: KeyVector -> Int -> KeyVector
setKey (KV k) i = KV $ k .|. bit i

clearKey :: KeyVector -> Int -> KeyVector
clearKey (KV k) i = KV $ k .&. complement (bit i)

keySet :: KeyVector -> Int -> Bool
keySet (KV k) = testBit k

mouseDown :: ZHandlerFunc (Button, (Int, Int)) GameState
mouseDown (_, (x,y)) = modify (\s -> s { stLastMouse = (x,y) })
                                   
mouseMoved (x,y) = do
  gr   <- gets stGraphics
  (oldx, oldy) <- gets stLastMouse
  let theta = 0.01 * fromIntegral (oldx-x)
  let phi   = 0.01 * fromIntegral (oldy-y)
  modify (\s -> s { stLastMouse = (x,y) })
  rotateCamera theta phi
  lift zUpdateGraphics

keyPress c = if c == 'q'
             then do gr <- gets stGraphics
                     lift exitSuccess
             else do kv <- gets stPressedKeys
                     let kv' = case c of 
                                 'a' -> kv `setKey` aKey
                                 's' -> kv `setKey` sKey
                                 'd' -> kv `setKey` dKey
                                 'w' -> kv `setKey` wKey
                                 _ -> kv
                     modify (\s -> s { stPressedKeys = kv' })

keyRelease c = do kv <- gets stPressedKeys
                  let kv' = case c of 
                              'a' -> kv `clearKey` aKey
                              's' -> kv `clearKey` sKey
                              'd' -> kv `clearKey` dKey
                              'w' -> kv `clearKey` wKey
                              _ -> kv
                  modify (\s -> s { stPressedKeys = kv' })
moveCamera :: StateT GameState IO ()                  
moveCamera = do
  kv <- gets stPressedKeys
  gr <- gets stGraphics
  let chan = gSceneChannel gr
  lift $ do (ZSceneRoot x cam@(c,f,u) t) <- zPeekChan chan
            let forward = unit $ f - c
            let right = forward `cross` u
            let d = unit (keyvec kv aKey (-right)
                          + keyvec kv sKey (-forward)
                          + keyvec kv dKey right
                          + keyvec kv wKey forward)
            zSwapChan chan $ ZSceneRoot x (c+scale 0.1 d,f+ scale 0.1 d,u) t
            zUpdateGraphics
      where keyvec kv key vec = if kv `keySet` key
                                then vec else zeroV
                                                
rotateCamera theta phi = do
  gr <- gets stGraphics
  let chan = gSceneChannel gr
  lift $ do (ZSceneRoot x cam@(c,f,u) t) <- zPeekChan chan
            zSwapChan chan $ ZSceneRoot x (c, mv theta phi cam, u) t
      where mv theta phi (c,f,u) = let dir   = unit $ f - c
                                       rot1  = rotation theta u
                                       dir'  = rotateVector rot1 dir
                                       right = dir `cross` u
                                       rot2  = rotation phi right
                                   in c + rotateVector rot2 dir'  
                                      
ticEvent _ = moveCamera
    
handleEvents :: ZGameLoop GameState
handleEvents = do graphics <- gets stGraphics
                  let channel = gEventChannel graphics
                  zWithEventChannel channel ZEventHandler {
                                          zKeyPress = keyPress
                                        , zKeyRelease = keyRelease
                                        , zMouseDown = mouseDown
                                        , zMouseUp = zIgnoreEvent
                                        , zMouseMove = mouseMoved
                                        , zTic = zIgnoreEvent
                                        }
