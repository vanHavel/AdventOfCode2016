module Utility.AStar where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List
import Data.Function
import Control.Monad.State

-- data types for the A* search
-- node stores f value (cost + heuristic), cost to get there and value
data Node a = Node {
    getF :: Int,
    getC :: Int,
    getVal :: a
}
-- successor function
type SuccFun a = a -> Set a
data SearchState a = SearchState {
    -- open list of nodes
    open :: [Node a],
    -- closed list of visited states
    closed :: Set a
}
-- heuristic for the problem: manhattan distance
type Heuristic a = a -> a -> Int

-- A* search returning length of minimal path to solution
aStarSearch :: (Ord a) => a -> a -> Heuristic a -> SuccFun a -> Int
aStarSearch initial final h succFun = evalState (runAStar final h succFun) 
    (SearchState {
        open = [Node {getF = (h initial final), getC = 0, getVal = initial}], 
        closed = Set.empty
    })
runAStar :: (Ord a) => a -> Heuristic a -> SuccFun a -> State (SearchState a) Int
runAStar final h succFun = do
    lists <- get
    let Node f c next = head $ open lists
    -- check for goal
    if next == final 
        then return c
        -- check if node already visited
        else if elem next (closed lists) 
            then do
                modify (\lists -> lists{open = tail $ open lists})
                runAStar final h succFun
            else do
                -- add to closed list
                modify (\lists -> lists{closed = (Set.insert next (closed lists))})
                -- add successors to open list
                let successors = Set.difference (succFun next) (closed lists)
                let newOpen = foldl (\open new -> insertSorted (newNode new) open) (tail $ open lists) successors where
                    insertSorted = insertBy (compare `on` (\node -> getF node))
                    newNode new = Node {getF = fNew new, getC = c + 1, getVal = new}
                    -- f value for successors, with path max correction
                    fNew new = maximum[c + 1 + (h new final), f]
                modify (\lists -> lists{open = newOpen})
                -- continue
                runAStar final h succFun