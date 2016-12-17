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
type Heuristic a = a -> Int
-- function which checks if a goal was reached
type GoalTest a = a -> Bool

-- A* search returning length of minimal path to solution, with a single final state
aStarSearch :: (Ord a) => a -> a -> Heuristic a -> SuccFun a -> Int
aStarSearch initial final h succFun = fst $ evalState (runAStar h succFun (== final)) 
    (SearchState {
        open = [Node {getF = (h initial), getC = 0, getVal = initial}], 
        closed = Set.empty
    })
-- A* serach returning length of minimal path to solution and final state reached. Allows several final states
genAStarSearch :: (Ord a) => a -> Heuristic a -> SuccFun a -> GoalTest a -> (Int, a)
genAStarSearch initial h succFun goal = evalState (runAStar h succFun goal) 
    (SearchState {
        open = [Node {getF = (h initial), getC = 0, getVal = initial}], 
        closed = Set.empty
    })
    
-- run A*, returning cost of best solution and the final state reached
runAStar :: (Ord a) => Heuristic a -> SuccFun a -> GoalTest a -> State (SearchState a) (Int, a)
runAStar h succFun goal = do
    lists <- get
    let Node f c next = head $ open lists
    -- check for goal
    if goal next  
        then return (c, next)
        -- check if node already visited
        else if elem next (closed lists) 
            then do
                modify (\lists -> lists{open = tail $ open lists})
                runAStar h succFun goal
            else do
                -- add to closed list
                modify (\lists -> lists{closed = (Set.insert next (closed lists))})
                -- add successors to open list
                let successors = Set.difference (succFun next) (closed lists)
                let newOpen = foldl (\open new -> insertSorted (newNode new) open) (tail $ open lists) successors where
                    insertSorted = insertBy (compare `on` (\node -> getF node))
                    newNode new = Node {getF = fNew new, getC = c + 1, getVal = new}
                    -- f value for successors, with path max correction
                    fNew new = maximum[c + 1 + (h new), f]
                modify (\lists -> lists{open = newOpen})
                -- continue
                runAStar h succFun goal