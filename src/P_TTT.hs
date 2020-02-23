--------------------------------------------------------------------------------

module P_TTT where


--------------------------------------------------------------------------------
{-
 G5BAFP Coursework 1 
 Unbeatable Noughts and Crosses 
 Graham Hutton 
 University of Nottingham 

 Abstract 
 The aim of this coursework is to write a Haskell program to play 
 noughts and crosses, which uses game trees and the minimax algorithm 
 to ensure that it never looses, i.e. always achieves a win or a draw. 
-}

-- The board 
-- For flexibility, the size ofthe board is definedas a constant: 

size :: Int 
size = 3 

-- The board itself is represented as a list of list of player values, with the width 
-- and height of the board always being of the above size: 

type Board =[[Player ]] 

-- In turn, a player value is either a nought, a blank, or a cross, with a blank 
-- representing a space on the board that is not yet occupied: 

data Player = Nought | Blank | Cross 
              deriving (Ord, Eq, Show ) 

-- For example, here is a typical board: 
-- [[Blank, Nought, Blank], [Cross , Nought, Cross ], [Cross , Blank, Blank]] 

-- The user interface 
-- The following code displays a board on the screen: 

showBoard :: Board -> IO () 
showBoard = putStrLn . unlines . concat . sepby hbar . map showRow 
            where hbar =[replicate (size * 6) '-' ] 

showRow :: [Player ]-> [String ]
showRow = beside . sepby vbar . map showPlayer
          where 
            beside = foldr1 (zipWith (++)) 
            vbar = replicate (size +2)"|"

showPlayer :: Player -> [String ]
showPlayer Nought =["     ", " +-+ ", " | | ", " +-+ ", "     " ]
showPlayer Blank  =["     ", "     ", "     ", "     ", "     " ]
showPlayer Cross  =["     ", " \\ / ", "  X  ", " / \\ ", "     " ]

sepby :: a -> [a]-> [a]
sepby x [] =[]
sepby x [y] =[y]
sepby x (y : ys)= y : x : sepby x ys

{-
For the purposes ofthis coursework, how showBoard worksisnot important; 
just use this function in your own program. For example, applying showBoard 
to the board from the previous section gives the following output: 

     |     | 
     | +-+ | 
     | | | | 
     | +-+ | 
     |     | 
--------------------
     |     |
 \ / | +-+ | \ /
  X  | | | |  X
 / \ | +-+ | / \
     |     |
--------------------
     |     | 
 \ / |     | 
  X  |     | 
 / \ |     | 
     |     | 

Exercise 
Write a Haskell program 
main :: IO () 
that allows a human player to play noughts and crosses against the computer, 
using game trees and the minimax algorithm (which will be explained in the 
lectures)to ensure that the computer never looses. 
Hint: construct your program from the bottom-up, starting by defining 
a number of utility functions on boards, and only then proceeding to think 
about game trees, minimax, and the user-interface. 
-}

--------------------------------------------------------------------------------
