-- /////////////////////////////////////////////////////////////////////////////

module Main_Test where

--------------------------------------------------------------------------------

import D_Calendar 
import D_Examples
import D_Expr
import D_Gofer
--import D_Literate
import D_Say
import D_Tree

-- import GA_Brown00

-- import JH_31
--import JH_32
--import JH_40
--import Main_Test

import P_Game
import P_Shuffle
import P_TTT
 
--import LogicT

--------------------------------------------------------------------------------
--import System.Environment(getEnv)
--import System.Directory

--import System.Directory
--import Directory
--import Data.Char   ListSeq                                                             
--import Data.ListSeq                                                             
--import Data.ListSeq                                                             
--import System.Time 
                                                             
import Time                                                              
--import Data                                                              

--------------------------------------------------------------------------------

my_string = "   45 78  "

test_examples = do
          putStr ("fact 5 = " ++ show (fact 5) ++ "\n")
          putStr ("\n")
          putStr ("ljustify 20 = " ++ show (ljustify 20 my_string) ++ "\n")
          putStr ("cjustify 20 = " ++ show (cjustify 20 my_string) ++ "\n")
          putStr ("rjustify 20 = " ++ show (rjustify 20 my_string) ++ "\n")
          putStr ("\n")


-- /////////////////////////////////////////////////////////////////////////////
                                                                                 
-- test_board = [[Blank, Nought, Blank], [Cross , Nought, Cross ], [Cross , Blank, Blank]]

--------------------------------------------------------------------------------
main_test = do

         putStr ("\n")

         test_examples
         putStr ("\n")

         sayit "sayit"
         putStr ("\n")

         putStr (show (eval "(2*3+4)/2") ++ "\n")
         putStr ("\n")

--          showBoard test_board
--          simulateGame

         drawTree sample
--           show sample
         putStr ("\n")
--           drawTree sample2
--           putStr ("\n")

--           calFor "2006"
--           putStr ("\n")

         putStr ("\n")
         putStr (show (extract 2 [1,2,3,4,5]))
         putStr ("\n")
         putStr (show (build_tree ['a','b','c','d','e']))
         putStr ("\n")
--           drawTree (build_tree ['a','b','c','d','e'])
--           putStr ("\n")
         putStr (shuffle1 ['a','b','c','d','e'] [0,0,0,0])
         putStr ("\n\n")

          -- main_literate 

--          putStr ("\n")
--          putStr (show (jh_sqrt 2.9 0.1 4) ++ "\n")
--          putStr (show (jh_relativesqrt 2.9 0.1 4) ++ "\n")
--          putStr ("\n")

--------------------------------------------------------------------------------
-- /////////////////////////////////////////////////////////////////////////////
--------------------------------------------------------------------------------

-- функция выводит на экран файл, добавляя в начало каждой строки ее номер
-- (пример из спецификации Haskel-98, стр. 251, монады)

-- listFile :: String -> IO ()
-- listFile nm =
--   do cts <- readFile nm
--     zipWithM_ (\i line -> do putStr (show i); putStr ": "; putStrLn line)
--               [1..]
--               (lines cts)

-- //////////////////////////////////////////////////////////////////////////////
-- QSort.html - из пакета от Ralf

-- Naive Implementierung                        
-- Mit Listenbeschreibungen.                                                    
                                                                                
-------------------------------qsort1-------------------------------------------

qsort1                :: Ord a => [a] -> [a]
qsort1 []             =  []
qsort1 (a:x)          =  qsort1 [ b | b <- x, b <= a ]
                      ++ a : qsort1 [ b | b <- x, b >  a ]

-------------------------------qsort2--------------------------------------------

-- Nachteil: die Liste x wird zweimal durchlaufen. Mit partition.

-- qsort2                :: Ord a => [a] -> [a]
-- qsort2 []             =  []
-- qsort2 (a:x)          =  qsort2 l ++ a : qsort2 r
--     where (l, r)      =  partition (>= a) x

-------------------------------qsort3-------------------------------------------

-- Endrekursive Variante                                                           

qsort3                :: Ord a => [a] -> [a]
qsort3 []             =  []
qsort3 [a]            =  [a]
qsort3 (a:x)          =  partition [] [] x
    where
    partition l r []  =  qsort3 l ++ a : qsort3 r
    partition l r (b:y)
        | b <= a      =  partition (b:l) r y
        | otherwise   =  partition l (b:r) y

-------------------------------qsort4--------------------------------------------

-- Verwendung akkumulierender Parametern
--   Die Variante ist Paulson, ML for the working programmer entnommen. qsort4 ist 
--   zusaetzlich mit der Ordnungsrelation <= parametrisiert.

qsort4                :: (a -> a -> Bool) -> [a] -> [a] -> [a]
qsort4 (<=) []    y   =  y
qsort4 (<=) [a]   y   =  a:y
qsort4 (<=) (a:x) y   =  partition [] [] x
    where
    partition l r []  =  qsort4 (<=) l (a : qsort4 (<=) r y)
    partition l r (b:y)
        | b <= a      =  partition (b:l) r y
        | otherwise   =  partition l (b:r) y

-------------------------------qsort---------------------------------------------

-- Stabile Variante                                                                
                                                                                
--   Die Variante geht auf Lennart Augustsson zurueck. Im wesentlichen wird die   
--   Funktionsdefinition "verdoppelt".                                            

qsort                 :: (a -> a -> Bool) -> [a] -> [a] -> [a]
qsort (<=) []    y    =  y
qsort (<=) [a]   y    =  a:y
qsort (<=) (a:x) y    =  qpart (<=) a x [] [] y

--   qpart partitions and sorts the sublists. Note that l and r are in reverse order 
--   and must be sorted with an anti-stable sorting.

qpart (<=) a [] l r y =  rqsort (<=) l (a : rqsort (<=) r y)
qpart (<=) a (b:x) l r y
    | a <= b          =  qpart (<=) a x l (b:r) y
    | otherwise       =  qpart (<=) a x (b:l) r y

-------------------------------rqsort--------------------------------------------

--    rqsort is as qsort but anti-stable, ie reverses equal elements.              

rqsort                :: (a -> a -> Bool) -> [a] -> [a] -> [a]
rqsort (<=) []    y   =  y
rqsort (<=) [a]   y   =  a:y
rqsort (<=) (a:x) y   =  rqpart (<=) a x [] [] y


rqpart (<=) a [] l r y=  qsort (<=) l (a : qsort (<=) r y)
rqpart (<=) a (b:x) l r y
    | b <= a          =  rqpart (<=) a x (b:l) r y
    | otherwise       =  rqpart (<=) a x l (b:r) y

--------------------------------------------------------------------------------
-- //////////////////////////////////////////////////////////////////////////////
--------------------------------------------------------------------------------



-------------------------------qsort0-------------------------------------------

-- это стандартный пример из учебника

qsort0 []     = []
qsort0 (x:xs) = qsort0 less ++ [x] ++ qsort0 more
                where less = filter (< x) xs
                      more = filter (>=x) xs

-------------------------------insertion_sort-----------------------------------
   
insertion_sort :: (a -> a -> Bool) -> [a] -> [a]                                  
insertion_sort pred []     = []                           
insertion_sort pred (x:xs) = insert2 pred x (insertion_sort pred xs)              
                                                                                 
insert2 :: (a -> a -> Bool) -> a -> [a] -> [a]                       
insert2 pred x [] = [x]                                                        
insert2 pred x (y:ys)                                                             
  | pred x y = (x:y:ys)                                                          
  | otherwise = y:(insert2 pred x ys)                                             
                                                                                 
-------------------------------gofer_sort---------------------------------------

gofer_sort  :: Ord a => [a] -> [a]
gofer_sort   = foldr gofer_insert []

gofer_insert              :: Ord a => a -> [a] -> [a]
gofer_insert x []          = [x]
gofer_insert x (y:ys)
        | x <= y     = x:y:ys
        | otherwise  = y:gofer_insert x ys

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- /////////////////////////////////////////////////////////////////////////////
--------------------------------------------------------------------------------

orign_spisok = [44, 55, 12, 42, 94, 18,  6, 67]
sorts_spisok = [ 6, 12, 18, 42, 44, 55, 67, 94]

test_sorts = do
        putStr ("qsort0 " ++ show (qsort0  orign_spisok) ++ "  End \n")
        putStr ("qsort1 " ++ show (qsort1  orign_spisok) ++ "  End \n")
        putStr ("qsort3 " ++ show (qsort3  orign_spisok) ++ "  End \n")
        putStr ("\n")

        putStr ("inser_sort " ++ show (insertion_sort (<=) orign_spisok) ++ "  End \n")
        putStr ("Gofer_sort " ++ show (gofer_sort orign_spisok) ++ "  End \n")
        putStr ("\n")
        putStr (show (insertion_sort (<=) ["bob","alice","zoe","barry"]) ++ "\n")
  
--------------------------------------------------------------------------------
-- /////////////////////////////////////////////////////////////////////////////
--------------------------------------------------------------------------------
