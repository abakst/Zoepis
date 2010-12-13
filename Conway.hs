module Main where
import Zoepis
import Data.Array
import Control.Monad.State
import Graphics.Rendering.OpenGL
import Unsafe.Coerce
--import Graphics.Rendering.OpenGL.GLU

data Conway = ConwayState {
    stGraphics :: ZGraphicsGL ConwayScene
  , stSceneChannel :: ZChannel ConwayScene
  , stGameOver :: ZChannel Bool
  , stLastTick :: !Integer
  , stCells    :: ConwayGrid
  }
              
type ConwayScene = ZSceneRoot              
startScene = ZSceneRoot Nothing (10*zAxis + 5*xAxis, origin, yAxis) [ZEmptyScene]
conwayTitle = "Conway's Game of Life"              
width = 800
height = 600

type ConwayGrid = Array (Int, Int) Bool
neighbors :: ConwayGrid -> (Int, Int) -> Int
neighbors g (i,j) =
  sum [count i' j' | i' <- [i-1..i+1], j' <- [j-1..j+1], i' /= i || j' /= j]
    where count i' j' = if or [i' < min_i,j' < min_j,i' > max_i,j' > max_j]
                        then 0
                        else valueOf (g ! (i',j'))
          ((min_i,min_j), (max_i,max_j)) = bounds g
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
          materialAmbient Front $= Color4 0.1 0.1 0.1 1
          when (g ! (i,j)) (materialAmbient Front  $= Color4 0.2 0.1 0.6 1)
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
                       }
  zRunWithGraphics start gr (gameloop gs conwayGame)
    where gameloop st lp = do
            st' <- zGameLoopStep st lp
            gameloop st' lp
            
conwayGame = do
  everyNTicks 2000 $ do
    grid <- gets stCells
    chan <- gets stSceneChannel
    modify (\s -> s { stCells = stepConway grid })
    lift $ do
      root <- zPeekChan chan
      zSwapChan chan $ root { zSceneObjects = renderConway grid }
    lift zUpdateGraphics
            
everyNTicks :: Integer -> ZGameLoop Conway a -> ZGameLoop Conway ()
everyNTicks n a = do
  t0 <- gets stLastTick
  t1 <- lift $ zGetTime
  loop t1 (t1 - t0)
  where loop t1 dif = if (dif > n)
                      then a >> loop t1 (dif - n)
                      else modify (\s -> s { stLastTick = t1 - dif })  