{-# LANGUAGE MultiParamTypeClasses #-}
module Main where
import Zoepis
import Data.Array
import Control.Monad.State
import Graphics.Rendering.OpenGL hiding (get)
import Control.Concurrent
import Unsafe.Coerce

data Conway = ConwayState {
    stGraphics :: ZGraphicsGL ConwayScene
  , stSceneChannel :: ZChannel ConwayScene
  , stScene    :: ConwayScene
  , stGameOver :: ZChannel Bool
  , stLastTick :: !Integer
  , stCells    :: ConwayGrid
  , stKeys     :: KeyVector
  }
              
instance KeysEnabled Conway where
  keKV = stKeys
  keSetKeys c kv = c { stKeys = kv }
  
instance Timed Conway where  
  tGetTicks = stLastTick
  tUpdateTicks c t = c { stLastTick = t }
              
instance EventSub Conway where                     
  esEventChannel = gEventChannel . stGraphics
  
instance GraphicsEnabled Conway ZSceneRoot where
  geGraphics = stGraphics
                     
type ConwayScene = ZSceneRoot              
startScene = ZSceneRoot Nothing (10*zAxis + 5*xAxis, origin, yAxis) [ZEmptyScene]
conwayTitle = "Conway's Game of Life - Press 'Q' to quit"              
width = 800
height = 600

type ConwayGrid = Array (Int, Int) Bool
neighbors :: ConwayGrid -> (Int, Int) -> Int
neighbors g (i,j) =
  sum [count i' j' | i' <- [i-1..i+1], j' <- [j-1..j+1], i' /= i || j' /= j]
    where count i' j' = valueOf (g ! ((i' - min_i) `mod` rR + min_i ,
                                      (j' - min_j) `mod` cR + min_j))
          ((min_i,min_j), (max_i,max_j)) = bounds g
          rR        =  max_i - min_i + 1
          cR        =  max_j - min_j + 1
          valueOf True  = 1
          valueOf False = 0

emptyGrid n = array ((-n,-n), (n,n)) values
  where values = [((i,j), False) | i<-[-n..n], j<-[-n..n]]
        
stepConway :: ConwayGrid -> ConwayGrid
stepConway g = g // (map (\i -> (i,updateCell i)) $ indices g)
  where liveOrDie :: Bool -> Int -> Bool
        liveOrDie c num | c && num < 2 = False
                        | c && num < 4 = True
                        | c && num > 3 = False
                        | not c && num == 3 = True
                        | otherwise = False
        updateCell (i,j) = liveOrDie (g ! (i,j)) (neighbors g (i,j))

renderConway :: ConwayGrid -> [ZSceneObject]        
renderConway g = map drawCellAt $ indices g
  where drawCellAt (i,j) = ZLiteral $ preservingMatrix $ do
          materialDiffuse Front $= Color4 0.1 0.1 0.1 1
          when (g ! (i,j)) (materialDiffuse Front  $= Color4 0.2 0.1 0.6 1)
          translate $ Vector3 (toGL i) (toGL j) (0::GLfloat)
          renderQuadric style (Sphere 0.15 8 8)
        style = QuadricStyle (Just Flat) NoTextureCoordinates Outside FillStyle
        toGL :: Int -> GLfloat
        toGL = unsafeCoerce . (0.45*) . (fromIntegral :: Int -> Float)
        
main = do
  time <- zGetTime
  gameOver <- zNewEmptyChan
  (gr, start) <- zInitialize conwayTitle width height (return ()) startScene gameOver
  let gs = ConwayState { stGraphics = gr
                       , stSceneChannel = gSceneChannel gr
                       , stGameOver = gameOver
                       , stLastTick = time
                       , stCells = emptyGrid 10 // [((1,2), True),
                                                   ((1,1), True),
                                                   ((1,0), True),
                                                   ((0,0), True),
                                                   ((-1,1), True)
                                                   ]
                       , stKeys = KV 0
                       , stScene = startScene
                       }
  start $ zGameLoop gs conwayGame
            
conwayGame :: ZGameLoop Conway ()
conwayGame = do
  zHandleEvents $ zEmptyHandler { zKeyPress = keyDown
                                , zKeyRelease = keyUp
                                }
  moveCamera
  zEveryNTicks 1500 $ do
    s <- get
    scene <- gets stScene
    let grid = stCells s
    let chan = stSceneChannel s
    let scene' = scene { zSceneObjects = renderConway grid }
    modify (\s -> s { stCells = stepConway grid, stScene = scene' })
    lift $ zPutChan chan scene'
  lift $ threadDelay 10
                                         
endGame = gets stGameOver >>= (lift . flip zPutChan True)

keyDown :: ZHandlerFunc Char Conway
keyDown c = case c of
              'q' -> endGame
              'h' -> modify (\s -> s { stKeys = KV 1 })
              'l' -> modify (\s -> s { stKeys = KV 2 })
              _ -> return ()
  
keyUp :: ZHandlerFunc Char Conway
keyUp c = modify (\s -> s { stKeys = KV 0 })
  
moveCamera :: ZGameLoop Conway ()
moveCamera = do
  (KV k) <- gets stKeys
  case k of
    1 -> orbitCamera 0.1
    2 -> orbitCamera (-0.1)
    _ -> return ()
    
orbitCamera theta = do
  scene <- gets stScene
  chan <- gets stSceneChannel
  let scene' = zOrbitCamera theta scene
  modify (\s -> s { stScene = scene' })
  lift $ zPutChan chan $ scene'
  return ()