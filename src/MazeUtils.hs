{-# LANGUAGE BangPatterns #-}
module MazeUtils where

import Control.Applicative
import Control.Monad.RWS
import Data.Array(Array,listArray,(!),bounds)
import Data.List
import qualified Data.Set as Set

data Direction = NORTH | SOUTH  | EAST | WEST deriving(Show)
data MazeCell  = WALL | EMPTY deriving (Show,Eq)
data MazeMode  = DFS | BFS | ASTAR

type Maze     = Array (Int,Int) MazeCell
type Position = (Int, Int)
type Cord     = ([Position],Int)

-- In the environement we keep :
-- the maze it self
-- the destination
-- an heuristic function to evaluate the cost of a position (for example
-- we can use the mahattan distance to the destination)
data MazeEnv = MazeEnv 
            { maze        :: Maze
            , destination :: Position
            , heuristic   :: Position -> Position -> Int
            , display     :: MazeState -> IO ()
            , method      :: MazeMode
            }

-- the state changes overtime. it contains:
-- a set of already visited positions
-- the current fringe
-- a flag that says if we are done
data MazeState = MazeState 
            { visited      ::  Set.Set Position
            , fringe       ::  [Cord]
            , mazeDone     ::  Bool
            }

-- Our monad: it is a simple Reader (for the env)  and a state
type MazeM t = RWST MazeEnv [String] MazeState IO t

type MazeResult = ([Position],Int)

-- build a maze (an 2D array) from a list of equaly lenhted characters
parseMaze :: String -> (Maze, Position, Position)
parseMaze s = (listArray ((1, 1), (width, height)) $ map cellType $ concat s', initial, final)
        where 
            width        = maximum $ map length s'
            height       = length s'
            initial      = mark '.'
            final        = mark 'P'
            -- get the importsition of a given mark
            mark c       = head $ findMark c (unlines s')
            -- we must reorder the maze description as it must defined column major
            s'           = transpose $ lines s
            -- simple mapping from column to a name
            cellType '%' = WALL
            cellType '█' = WALL
            cellType  _  = EMPTY

-- retrieve the maze size
-- to do so just get the size of the array
mazeSize :: Maze -> (Int,Int)
mazeSize m = (xmax - xmin + 1, ymax - ymin + 1) 
            where ((xmin,ymin),(xmax,ymax)) = bounds m

-- helper to transfor a maze description given as a list of string to a list of position for a given character
findMark :: Char -> String -> [Position]
findMark x xs = [ (n,z) | (l,n) <- zip (lines xs) [1..] , (c, z) <- zip l [1..], c == x]

-- check if the maze contains a specific mark at the given position
hasMark :: MazeCell -> Position ->  MazeM Bool
hasMark c pos = asks $ is_mark . maze
                where is_mark m = m ! pos == c

-- check if the maze contains nothing (basically no wall) at the given position
isFree :: Position -> MazeM Bool
isFree pos = liftM not (hasMark WALL pos)

-- compute a move's next position
computeMove :: Position -> Direction -> Position
computeMove (x,y)  NORTH  = (x, y-1)
computeMove (x,y)  SOUTH  = (x, y+1)
computeMove (x,y)  EAST   = (x+1, y)
computeMove (x,y)  WEST   = (x-1, y)

-- get a list of possible action (including out-of-bounds and illegal moves)
computeMoves :: Position -> [Direction] -> MazeM [Position]
computeMoves pos = mapM mkNewPos
        where mkNewPos dir = return $ computeMove pos dir

-- check if a position is within the maze bounds
isPositionInBounds :: Position -> MazeM Bool
isPositionInBounds (x,y) = do
        (w,h) <- asks $ mazeSize . maze
        return $ x >= 1 && y >=1 && x <= w && y <= h

-- check if a position has not been visited yet
isPositionNotVisited :: Position -> MazeM Bool
isPositionNotVisited pos = gets isnew
        where isnew v =  Set.notMember pos $ visited v

-- mark a position as visited
setPositionVisited :: Position -> MazeM ()
setPositionVisited pos = do
        st <- get
        v <- gets visited
        put st { visited = Set.insert pos v }

-- A position is legal if:
-- it is within maze bounds
-- it is free (no walls)
-- it has not been visited before
isPositionLegal :: Position -> MazeM Bool
isPositionLegal pos = and3 <$> isPositionInBounds pos <*> isFree pos <*> isPositionNotVisited pos
                      where and3 x y z = x && y && z

validMoves :: Position -> MazeM [Position]
validMoves pos = computeMoves pos d >>= filterM isPositionLegal
        where !d =  [NORTH, EAST, SOUTH, WEST]

updateDisplay :: MazeM () 
updateDisplay  = do
        d  <- asks display
        st <- get
        liftIO $ d st

-- mark the maze as solved
markSolved :: MazeM () 
markSolved = do
        f <- gets $ head . fringe
        modify (\s -> s {mazeDone = True, fringe = [f] })
        updateDisplay

-- update the current fringe
-- it is also a good opportunity to call the display callback as it is
-- a state change
updateFringe :: [Cord] -> MazeM ()
updateFringe !new = do
        modify (\s -> s { fringe = new })
        updateDisplay

