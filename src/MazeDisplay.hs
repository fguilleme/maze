module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Animate
import Control.Monad
import MazeSolver
import MazeUtils hiding (display)
import Data.Array((!))
import System.Environment(getArgs)
import Control.Concurrent
import qualified Data.Set as Set
import Data.IORef

cellSize' :: Float
cellSize' = 16

parseOptions :: [String] -> (Int,MazeMode,String)
parseOptions xs' = go xs' (100, DFS, "")
    where go ("--dfs":xs) (s,_,fn) = go xs (s,DFS,fn) 
          go ("--bfs":xs) (s,_,fn) = go xs (s,BFS,fn)
          go ("--astar":xs) (s,_,fn) = go xs (s,ASTAR,fn)
          go ("--speed":speed:xs) (_,m,fn) = go xs (read speed,m,fn)
          go [x@(_:_)] (s,m,_) = (s,m,x)
          go _ _ = error "Usage [--dfs|--bfs|--astar] [--speed <value>] <filename"

main :: IO ()
main = do 
          world <- newIORef blank
          kw <- newSem 0
          (speed,mode',file) <- liftM parseOptions getArgs
          str <- readFile file
          let (m, initial, final) = parseMaze str
              (w,h) = mazeSize m
              (width, height) =  (floor cellSize' * (w + 2), floor cellSize' * (h + 2))
              d :: Display
              d = InWindow "MAZE" (width,height) (200,200)
              update :: Float -> IO Picture
              update _ = do
                  p <- readIORef world
                  when (speed > 0) $ signalSem kw >> threadDelay (1000 * speed)
                  return $! Pictures $ drawBG : [p] 
              solverDisplay :: MazeState -> IO ()
              solverDisplay st =
                  let (tp,_) = head $ fringe st
                      vis = Set.elems $ visited st
                      pict = Pictures $ [drawF m (x,y) drawVis | (x,y) <- vis] ++
                                        [drawF m (x,y) (drawGood red) | (x,y) <- tp] in do
                      writeIORef world pict 
                      when (speed > 0) $ waitSem kw
                      return ()
              drawBG = drawMaze m initial final
          forkIO $ void (mazeSolver m initial final solverDisplay mode')
          animateIO d white update 

data Sem = Sem (MVar Int) (MVar Int)

newSem :: Int -> IO Sem
newSem initial = liftM2 Sem (newMVar initial) newEmptyMVar

-- | Wait for a unit to become available
waitSem :: Sem -> IO ()
waitSem (Sem sem wakeup) = do
       avail' <- modifyMVar sem (\avail -> return (avail-1, avail-1))
       when (avail' < 0) $ takeMVar wakeup >>= putMVar sem

-- | Signal that a unit of the 'Sem' is available
signalSem :: Sem -> IO ()
signalSem (Sem sem wakeup) = do
       avail <- takeMVar sem
       if avail < 0 then putMVar wakeup (avail+1)
                    else putMVar sem (avail+1)

drawMaze :: Maze -> Position -> Position -> Picture
drawMaze m initial final = 
         let (w,h)       = mazeSize m
             drawBoard   = [drawMazeCell m x y | x <- [1..w], y <- [1..h]] in
                   Pictures $ drawBoard ++ 
                              [drawMark m initial blue] ++
                              [drawMark m final orange]

drawMazeCell :: Maze -> Int -> Int -> Picture
drawMazeCell m x y = drawF m (x,y) (drawRect c)
                where c              = cellColor $ m ! (x,y)
                      cellColor :: MazeCell -> Color
                      cellColor WALL = greyN 0.2
                      cellColor _    = greyN 0.8

drawPath ::  Maze -> [Position] -> Color -> Picture
drawPath m s c = Pictures [ drawStep pos | pos <- s]
                where
                    drawStep :: (Int,Int) -> Picture
                    drawStep (x,y) = drawF m (x,y) (drawGood c)

drawMark :: Maze -> (Int,Int) -> Color -> Picture
drawMark m (x,y) c = drawF m (x,y) (drawStar c)

drawF :: Maze -> (Int,Int) -> Picture -> Picture
drawF m (x,y) = Translate transX transY
                where (w,  h)  = mazeSize m
                      (w', h') = ((w + 2) `div` 2, (h + 2) `div` 2)
                      transX   = -cellSize' * fromIntegral  (w' - x)
                      transY   = -cellSize' * fromIntegral (-h' + y)

drawRect :: Color ->Picture
drawRect c = Color c $ rectangleSolid cellSize' cellSize'

drawStar :: Color -> Picture
drawStar c = Color c $ circleSolid (cellSize' / 2)

drawGood :: Color -> Picture
drawGood c = Color c $ circleSolid (cellSize' / 4)

drawVis :: Picture
drawVis = Color (greyN 0.1)  $ circleSolid 2
