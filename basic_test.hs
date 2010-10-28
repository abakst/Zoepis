module Main where

import ZScene
import ZObject
import ZGraphics
import ZVector
import ZGame
import ZChannel
import ZEventMessage
import ZParticle
import ZCamera
import ZKeys

import ExampleObject    

import Control.Monad.State
import Control.Concurrent
import System.Exit
import System.CPUTime

cameraStart = (xAxis+4*zAxis+2*yAxis, origin, yAxis)

resources  = shipR
startScene = ZSceneRoot Nothing cameraStart ZEmptyScene
objects    = [ newShip 0.1 origin zAxis
             , newShip 0.1 (origin+yAxis+xAxis) zAxis
             ]
             
data GameState = GameState {
      stGraphics    :: ZGraphicsGL ZSceneRoot
    , stLastMouse   :: !(Int, Int)
    , stMouseV      :: !(Int, Int)
    , stPressedKeys :: !KeyVector
    , stLastTick    :: !Integer
    , stObjects     :: [ExampleObject]
    , stSceneChannel :: ZChannel ZSceneRoot
    , stGameOver     :: ZChannel Bool
    }

main = do time     <- zGetTime
          gameOver <- zNewEmptyChan
          (gr,startGr) <- zInitialize "Test" 800 600  resources startScene gameOver
          let gs = GameState { stGraphics = gr
                             , stLastMouse = (0,0)
                             , stPressedKeys = KV 0
                             , stMouseV = (0,0)
                             , stLastTick = time
                             , stSceneChannel = gSceneChannel gr
                             , stObjects = objects
                             , stGameOver = gameOver
                             }
          forkOS $ gameloop gs myGame
          startGr
              where gameloop st lp = do st' <- zGameLoopStep st lp
                                        gameloop st' lp

myGame :: ZGameLoop GameState ()
myGame = do handleEvents
            moveCamera
            everyNTicks 300 $ do
              chan <- gets stSceneChannel
              objs <- gets stObjects
              modify (\s -> s{stObjects = map (flip objUpdate 0) objs})
              lift $ do root <- zPeekChan chan
                        let scene = map objScene objs
                        zSwapChan chan $ root {
                                        zSceneTree = ZGroupNode scene
                                      }
            lift $ zUpdateGraphics
            
everyNTicks :: Integer -> ZGameLoop GameState a ->
               ZGameLoop GameState ()
everyNTicks n a = do
  t0 <- gets stLastTick
  t1 <- lift $ zGetTime
  when (t1 - t0 > n) (a >> modify (\s -> s{stLastTick = t1} ))
  
handleEvents :: ZGameLoop GameState ()
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

endGame = gets stGameOver >>= (lift . flip zPutChan True)                    

keyPress c = if c == 'q'
             then endGame
             else do
               kv <- gets stPressedKeys
               let kv' = case c of 
                           'a' -> (kv `zSetKey` zAKey) `zClearKey` zDKey
                           's' -> (kv `zSetKey` zSKey) `zClearKey` zWKey
                           'd' -> (kv `zSetKey` zDKey) `zClearKey` zAKey
                           'w' -> (kv `zSetKey` zWKey) `zClearKey` zSKey
                           _ -> kv
               modify (\s -> s { stPressedKeys = kv' })

keyRelease c = do kv <- gets stPressedKeys
                  let kv' = case c of 
                              'a' -> kv `zClearKey` zAKey
                              's' -> kv `zClearKey` zSKey
                              'd' -> kv `zClearKey` zDKey
                              'w' -> kv `zClearKey` zWKey
                              _ -> kv
                  modify (\s -> s { stPressedKeys = kv' })
                         
moveCamera :: ZGameLoop GameState ()                  
moveCamera = do
  kv@(KV pressed) <- gets stPressedKeys
  chan <- gets stSceneChannel
  lift $ do root <- zPeekChan chan
            let scale = 0.1
            let right = if zKeySet kv zDKey
                        then scale else if zKeySet kv zAKey
                                        then -scale else 0
            let forward = if zKeySet kv zWKey
                          then scale else if zKeySet kv zSKey
                                          then -scale else 0
            zSwapChan chan $ zMoveCamera right forward root
            return ()
                                                
rotateCamera theta phi = do
  chan <- gets stSceneChannel
  lift $ do root <- zPeekChan chan
            zSwapChan chan $ zRotateCamera theta phi root
