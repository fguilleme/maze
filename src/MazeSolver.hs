--{-# LANGUAGE BangPatterns #-}
module MazeSolver where

import Control.Applicative
import Control.Monad.RWS
import Data.List
import qualified Data.Set as Set (empty,elems)

import MazeUtils

-- compute the manhattan distance between two positions
manhattan :: Position -> Position -> Int
manhattan (x1,y1) (x2,y2) = abs (x2 - x1) + abs (y2 - y1)

-- sort the fringe by its cost + a heuristic cost
sortFringe :: (Position -> Int) -> [Cord] -> [Cord]
sortFringe h = sortBy best
        where best (p1:_,c1) (p2:_,c2) = compare (c1 + h p1) (c2 + h p2)

-- return the next fringe from the current one
-- it consist to get the top item from the fringe (which is a list of
-- preceding actions, the last one at top)
-- get possibke action from this position and return a new fringe that is
-- the ol one minus the top plus the list of possible pathes
-- The next fringe is sorted by cost
getNewFringeDFS, getNewFringeBFS, getNewFringeAStar :: MazeM [Cord]
getNewFringeAStar = do
        ((l@(pos:_), cost) : fringe') <-  gets fringe
        dest  <- asks destination
        h     <- asks heuristic
        moves <- validMoves pos
        let sorter = if length moves == 1 then id else sortFringe (h dest) 
        return $! sorter $ [ (p : l, cc)  | p <- moves, let cc = 1 + cost] ++ fringe'

getNewFringeDFS = do
        ((l@(pos:_), cost) : fringe') <-  gets fringe
        moves <- validMoves pos
        return $! [ (p : l, cc)  | p <- moves, let cc = 1 + cost] ++ fringe'

getNewFringeBFS = do
        ((l@(pos:_), cost):fringe') <-  gets fringe
        moves <- validMoves pos
        return $! fringe' ++ [ (p : l, cc)  | p <- moves, let cc = 1 + cost] 

getNewFringe :: MazeMode -> MazeM [Cord]
getNewFringe DFS   = getNewFringeDFS
getNewFringe BFS   = getNewFringeBFS
getNewFringe ASTAR = getNewFringeAStar

-- the main loop of the solver
-- first check if the maze is solved or the top fringe path is a solution (no point to check more solutions)
-- if mark position as visited get a new fringe and loop
runMaze :: MazeM Cord
runMaze = do
        st              <- get
        path@(pos:_,_)  <- gets $! head . fringe
        dest            <- asks destination
        let res   | mazeDone st = return path
                   | dest == pos = markSolved >> return path
                   | otherwise   = setPositionVisited pos >> 
                            tell [show pos] >>
                            asks method >>= 
                            getNewFringe >>= \f ->
                            updateFringe f >> 
                            runMaze
                in res

solve :: MazeM MazeResult
solve = makeres ([],0) <$> runMaze
        where
            makeres def ([],_) = def
            makeres _ (xs,cost) = (xs, cost)

mazeSolver :: Maze -> Position -> Position -> (MazeState -> IO ()) -> MazeMode -> IO (MazeResult, [Position])
mazeSolver m initial final disp md = do
            (sol, st', _) <- runRWST solve env st
            let vis = Set.elems $! visited st' in
                return (sol, vis)
        where st    = MazeState { visited = Set.empty
                                , mazeDone = False 
                                , fringe = [([initial],0)]
                                }
              env   = MazeEnv   { maze = m
                                , destination = final
                                , heuristic = manhattan
                                , display = disp
                                , method = md
                                }
