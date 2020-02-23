--------------------------------------------------------------------------------
{-
         http://web.cs.wpi.edu/%7Ecs4536/c06/index.html
         Homework 2: Haskell and Laziness   
         Part 1: Haskell Programming
 
 Problem 3: Minimax Search

In this exercise, you will implement a strategy to play a simple game. The game 
is called Mancala, but you don't need to understand the rules because we have 
implemented that part for you. Your job is to build a tree of possible move sequences 
and choose the move that appears best.

Download the support code, which provides the following set of data types and functions:

 * Player: values of this type represent the players of the game (PlayerA or PlayerB).
 * State: values of this type represent game configurations.
 * initialState :: Player -> State represents the initial configuration of the game board 
   (the given player goes first).
 * getPlayer: State -> Player: given a configuration, returns the player who makes the next move.
 * getScore :: Player -> State -> Int returns the score for the given player in the given 
   configuration. (Bigger numbers are desirable for your player.)
 * nextStates :: State -> [State] gives the possible configurations after the next move. 
   If the returned list is empty, then the game is over.

 1. Define a datatype GameTree to represent the game state after any sequence of moves. 
    Each node should have its current configuration and a list of trees, where each tree 
    corresponds to containing the configurations obtainable following a specific single move.
 2. Define the tree of all legal board configurations (those obtainable by repeated 
    application of nextStates to the initialState).
 3. Define prune :: Int -> GameTree -> GameTree, which prunes a tree to a given height.
 4. Define minimax :: Player -> GameTree -> Int, which consumes a (pruned) tree and evaluates 
    the configuration by looking ahead and applying the following minimax algorithm. If a node 
    has no children, it receives its own immediate score. If it corresponds to Player's turn, 
    it receives the maximum of the recursively-computed child scores, otherwise it receives the 
    minimum.

You do not need to understand the support code to do this assignment. The API given above is 
sufficient for solving these problems.

To use the support code, put the following two lines at the top of your file, where your file 
is called Filename.hs and Game.hs (the support code) is in the same directory as your file.

  module Filename where
  import Game 

-}
--------------------------------------------------------------------------------
-- Game.hs

module P_Game (State, Player(PlayerA, PlayerB), initialState, getScore, nextStates, getPlayer, simulateGame) where

--------------------------------------------------------------------------------
data Player = PlayerA | PlayerB deriving Eq
data PlayerState = PlayerState Player Int [Int]
data State = State PlayerState PlayerState

instance Show Player where
  show PlayerA = "A"
  show PlayerB = "B"

instance Show PlayerState where
  show (PlayerState player score pits) =
    (show player) ++ ": " ++ (show score) ++ "; " ++ (show pits)

instance Show State where
  show s =
    "(A " ++ (show (getScore PlayerA s)) ++ ") " ++ (show (reverse (getPits PlayerA s))) ++
    "\n(B " ++ (show (getScore PlayerB s)) ++ ") " ++ (show (getPits PlayerB s))

--------------------------------------------------------------------------------
otherPlayer :: Player -> Player
otherPlayer PlayerA = PlayerB
otherPlayer PlayerB = PlayerA

initialState :: Player -> State
initialState p = 
  State (PlayerState p 0 (replicate 6 4)) (PlayerState (otherPlayer p) 0 (replicate 6 4))

getScore :: Player -> State -> Int
getScore p (State (PlayerState a sa _) (PlayerState b sb _)) =
  if p == a then sa else sb

getPits :: Player -> State -> [Int]
getPits p (State (PlayerState a _ pa) (PlayerState b _ pb)) =
  if p == a then pa else pb

getPlayer :: State -> Player
getPlayer (State (PlayerState p _ _) _) = p

incStones :: PlayerState -> PlayerState
incStones p = distStones [1..6] p

incStonesBy :: Int -> PlayerState -> PlayerState
incStonesBy n p = (iterate incStones p) !! n

incScore :: Int -> PlayerState -> PlayerState
incScore i (PlayerState p s ps) = (PlayerState p (s + i) ps)

distStones :: [Int] -> PlayerState -> PlayerState
distStones is (PlayerState p s ps) =
  (PlayerState p s (map (\ (i, n) -> if (elem i is) then n + 1 else n) (zip (iterate (+ 1) 1) ps)))

isWinningState :: State -> Bool
isWinningState (State (PlayerState _ a _) (PlayerState _ b _)) =
  a >= 24 || b >= 24

isMoveValid :: Int -> Bool
isMoveValid n = elem n [1..6]

isMoveLegal :: State -> Int -> Bool
isMoveLegal _ n | not (isMoveValid n) = False
isMoveLegal s _ | (isWinningState s) = False
isMoveLegal s n = case s of
                    (State (PlayerState a sa pa) _) -> not $ (pa !! (n - 1)) == 0

applyMove :: State -> Int -> Maybe State
applyMove s n | not (isMoveLegal s n) = Nothing
applyMove s n = Just (applyMove' s' p')
  where
    (s', p') = case s of
                (State (PlayerState a sa pa) playerother) ->
                  ((State (PlayerState a sa (killPit pa)) playerother), pa !! (n - 1))
                  
    killPit xs = (take (n - 1) xs) ++ [0] ++ (drop n xs)
    
    applyMove' (State a b) p = 
      let (rounds, extras) = divMod p 13
          sinc             = rounds + (if (6 - n) < extras then 1 else 0)
          extras' = if n == 6
                      then extras - 1
                      else if null adist then 0 else extras - (last adist) + (head adist) - 2
          extras''  = if null bdist then 0 else extras' - (last bdist) + (head bdist) - 1
          adist     = [i | i <- [(n + 1)..6], i - n <= extras]
          bdist     = [i | i <- [1..6], i <= extras']
          adist'    = [i | i <- [1..6], i <= extras'']
          currP = ((incScore sinc) . (distStones adist) . (distStones adist') . (incStonesBy rounds) $ a)
          otherP = ((distStones bdist) . (incStonesBy rounds) $ b) in
        if extras == (6 - n + 1) then (State currP otherP) else (State otherP currP)
        --(State otherP currP)

nextStates :: State -> [State]
nextStates s = [s | (Just s) <- (map (applyMove s) [1..6])]

--------------------------------------------------------------------------------
simulateGame :: IO ()
simulateGame = simulateGame' (Just (initialState PlayerA)) where
  simulateGame' Nothing = return ()
  simulateGame' (Just (State (PlayerState p s ps) b)) =
    do
      print (State (PlayerState p s ps) b)
      putStr ((show p) ++ ": ")
      move <- getLine
      move <- return (read move) :: IO (Int)
      s' <- return (applyMove (State (PlayerState p s ps) b) move)
      case s' of
         Nothing -> putStr "ERROR\n"
         (Just a) -> if (isWinningState a) 
                          then putStr ((show p) ++ " wins!\n")
                          else simulateGame' s'
      return ()

--------------------------------------------------------------------------------

simulateGame2 :: (State -> Int) -> IO ()
simulateGame2 chooseMove = simulateGame' (Just (initialState PlayerA)) where
  simulateGame' Nothing = return ()
  simulateGame' (Just state@(State (PlayerState p s ps) b)) =
    do
      print (State (PlayerState p s ps) b)
      putStr ((show p) ++ ": ")
      move <-
	  if p == PlayerB then
             do move <- getLine
		return (read move) :: IO (Int)
	  else return (chooseMove state)
      s' <- return (applyMove (State (PlayerState p s ps) b) move)
      case s' of
         Nothing -> putStr "ERROR\n"
         (Just a) -> if (isWinningState a) 
                          then putStr ((show p) ++ " wins!\n")
                          else simulateGame' s'
      return ()
--------------------------------------------------------------------------------
