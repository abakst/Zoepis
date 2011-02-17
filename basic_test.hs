{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Zoepis
  
import ExampleObject    
import Unsafe.Coerce
import Control.Monad.State
import Control.Concurrent
import System.Exit
import System.CPUTime

cameraStart = (origin, zAxis, yAxis)
resources  = shipR >> zLoadObject 1 "resources/skybox/cube.obj" True
startScene = ZSceneRoot (Just (ZSkybox 1)) cameraStart [ZEmptyScene]
--startScene = ZSceneRoot Nothing cameraStart [ZEmptyScene]
left = vector3D (-2.0, 0.0, -1.0)
right = vector3D (2.0, 0.0, -1.0)
formation = [newShip origin noRotation 0.3 1
            ,newShip left noRotation 0.3 1
            ,newShip right noRotation 0.3 1
            ]
             
data GameState = GameState {
      stLastMouse    :: !(Int, Int)
    , stMouseV       :: !(Int, Int)
    , stPressedKeys  :: !KeyVector
    , stLastTick     :: !Integer
    , stObjects      :: [ExampleObject]
    , stSceneChannel :: ZChannel ZSceneRoot
    , stEventChannel :: ZChannel [ZEvent]
    , stGameOver     :: ZChannel Bool
    , stScene        :: ZSceneRoot
    , stSubWindowMsg :: ZChannel [ZWindowMessage]
    }
                 
instance KeysEnabled GameState where
  keKV = stPressedKeys
  keSetKeys g kv = g { stPressedKeys = kv }
  
instance Timed GameState where  
  tGetTicks = stLastTick
  tUpdateTicks g t = g { stLastTick = t }
  
instance EventSub GameState where  
  esEventChannel = stEventChannel

main = do
  time         <- zGetTime
  gameOver     <- zNewEmptyChan
  event        <- zNewChan []
  scene        <- zNewChan startScene
  subWindowC   <- zNewChan []
  mainWindowC  <- zNewChan []
  let subWindow = ZWindowGL {
                    gWindowName = "Sub"
                  , gWidth = 500
                  , gHeight = 400
                  , gWindowOffset = (0,0)
                  , gSceneChannel = scene
                  , gEventChannel = Nothing
                  , gSubWindows = []
                  , gResloader = resources
                  , gWindowChannel = subWindowC
                  }
  let window = ZWindowGL {
                 gWindowName = "Main"
               , gWidth = 1024
               , gHeight = 768
               , gWindowOffset = (0,0)
               , gSceneChannel = scene
               , gEventChannel = Just event
               , gSubWindows = [subWindow]
               , gResloader = resources
               , gWindowChannel = mainWindowC
               }
  startGr <- zInitialize "Test" window gameOver
  let gs = GameState { stLastMouse = (0,0)
                     , stPressedKeys = KV 0
                     , stMouseV = (0,0)
                     , stLastTick = time
                     , stSceneChannel = scene
                     , stEventChannel = event
                     , stObjects = formation
                     , stGameOver = gameOver
                     , stScene = startScene
                     , stSubWindowMsg = subWindowC
                     }
  startGr $ zGameLoop gs myGame

myGame :: ZGameLoop GameState ()
myGame = do
  zHandleEvents $ zEmptyHandler { zKeyPress = keyPress
                                , zKeyRelease = keyRelease
                                , zMouseDown = mouseDown
                                , zMouseMove = mouseMoved
                                }
  moveCamera
  zEveryNTicks 100 $ do
           chan <- gets stSceneChannel
           objs <- gets stObjects
           root <- gets stScene
           let root' = root { zSceneObjects = concatMap objScene objs }
           modify (\s -> s{ stObjects = map (flip objUpdate 0.1) objs
                          , stScene = root'
                          })
           lift $ zSwapChan chan root'
            
mouseDown :: ZHandlerFunc (Button, (Int, Int)) GameState
mouseDown (_, (x,y)) = modify (\s -> s { stLastMouse = (x,y) })
                                   
mouseMoved (x,y) = do
  (oldx, oldy) <- gets stLastMouse
  let theta = 0.01 * fromIntegral (oldx-x)
  let phi   = 0.01 * fromIntegral (oldy-y)
  modify (\s -> s { stLastMouse = (x,y) })
  rotateCamera theta phi

endGame = gets stGameOver >>= (lift . flip zPutChan True)

keyPress c = case c of
               'q' -> endGame
               'r' -> modify (\s -> s { stObjects = formation })
               'h' -> gets stSubWindowMsg >>= lift . sendWindow HideWindow
               'y' -> gets stSubWindowMsg >>= lift . sendWindow ShowWindow
               _ ->  do
                 kv <- gets stPressedKeys
                 let kv' =
                       case c of 
                         'a' -> (kv `zSetKey` zAKey) `zClearKey` zDKey
                         's' -> (kv `zSetKey` zSKey) `zClearKey` zWKey
                         'd' -> (kv `zSetKey` zDKey) `zClearKey` zAKey
                         'w' -> (kv `zSetKey` zWKey) `zClearKey` zSKey
                         _ -> kv
                 modify (\s -> s { stPressedKeys = kv' })
    where sendWindow msg chan = zModifyChan_ chan (\m -> return (m++[msg]))

keyRelease c = do 
  kv <- gets stPressedKeys
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
  root <- gets stScene
  let scale = 0.01
  let right = if zKeySet kv zDKey
              then scale else if zKeySet kv zAKey
                              then -scale else 0
  let forward = if zKeySet kv zWKey
                then scale else if zKeySet kv zSKey
                                then -scale else 0
  let root' = zMoveCamera right forward root
  modify (\s -> s { stScene = root' } )
  lift $ zSwapChan chan root'
  return ()
                                                
rotateCamera theta phi = do
  root <- gets stScene
  chan <- gets stSceneChannel
  let root' = zRotateCamera theta phi root
  lift $ zSwapChan chan root'
  modify (\s -> s { stScene = root' } )
