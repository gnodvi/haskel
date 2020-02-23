-- ========================================================================== --
-- Main program
--------------------------------------------------------------------------------

module RS_Main where

import RS_MergeSort

-- import HeapSort (heapSort, bottomUpHeapSort)
-- import BraunHeap (braunSort, bottomUpBraunSort)
-- import RedBlackTree (redBlackSort, redBlackSort')
-- import Trees12 (sort12)
-- import SplaySort (splaySort)
-- import PairingHeap (pairingSort, mpPairingSort, optimalMergeSort)

import RS_QuickSort (introSort)

-- import AdaptiveHeapSort (adaptiveHeapSort)
-- import DigitalSort (intRadixSort)
-- import FingerSearchtree (fingerTreeSort) -- requires polymorphic recursion
-- import MargeSort (margeSort, naturalMargeSort) -- requires polymorphic recursion
-- import QuickSortInPlace (qsort)

import RS_TestData

import System

import RS_Benchmark
import RS_Killer
import RS_RandomMonad
import RS_ListLib

--------------------------------------------------------------------------------

ralf_main
  =  do args <- getArgs
        putStrLn "Sort!\n\n"

        case args of
          []           -> run sorter 10000
          ["-d"]       -> run checkedSorter 10000
          "-d" : n : _ -> run checkedSorter (read n)
          n : _        -> run sorter (read n)

--------------------------------------------------------------------------------

run sorter n
  =  sequence [ benchmark sorter (inputs gs) | gs <- generators n ] 

--------------------------------------------------------------------------------

-- |checkedSorter 		:: (Ord a) => [(String, [a] -> Bool)]|
checkedSorter 		:: [(String, [Int] -> Bool)]
checkedSorter			= [ (s, check f) | (s, f) <- sorter ]

--------------------------------------------------------------------------------

check f a
  | f a == sort a		=  True
  | otherwise			=  error ("<error: sequence is not sorted>\n"
				++ show (sort a) ++ "\n"
				++ show (f a))
--------------------------------------------------------------------------------

-- |sorter :: (Ord a) => [(String, [a] -> [a])]|
sorter :: [(String, [Int] -> [Int])]
sorter
  =  [ 
       ("m",    mergeSort)

--      , ("opt",  optimalMergeSort)
--      , ("bum",  bottomUpMergeSort)
--      , ("sm",   straightMergeSort)
--      , ("oem",  oddEvenMergeSort)
--      , ("lpm",  lpMergeSort)
--      , ("am",   adaptiveMergeSort)
--      , ("nm",   naturalMergeSort)
--      , ("snm",  symmetricNaturalMergeSort)
--      , ("om",   onlineMergeSort)
--      , ("om'",  onlineMergeSort')
--      , ("fm",   flipSort)
--      , ("om3",  onlineMergeSort3)
--      , ("h",    heapSort)
--      , ("buh",  bottomUpHeapSort)
--      , ("b",    braunSort)
--      , ("bub",  bottomUpBraunSort)
--      , ("rb",   redBlackSort)
--      , ("rb'",  redBlackSort')
--      , ("12",   sort12)
--      , ("sp",   splaySort)
--      , ("ph",   pairingSort)
--      , ("mph",  mpPairingSort)

     , ("iq",   introSort)

--      , ("rs",   intRadixSort)	-- works only on |Int|
--      , ("ft",   fingerTreeSort)
--      , ("ma",   margeSort)
--      , ("nma",  naturalMargeSort)

--      , ("qs",   qsort)	-- an array-based qsort (median of three)

--      , ("ah",   adaptiveHeapSort)

     ]

-- NB the space leaks if |qsort| or |fingerTreeSort| is included!
--------------------------------------------------------------------------------

generators :: Int -> [[(String, RandomMonad [Int])]]
generators n
  =  [ [ ("<", return [1 .. n])
--        , ("<=", return (increasing n))
       , (">", return [n, n - 1 .. 1])
--        , (">=", return (decreasing n))
--        , ("==", return (replicate n 0))
--        , ("!ms", return (bad4merge n))
--        , ("!mph", return (bad4mpp n))

       , ("random", randomInts n) ]

--      , [ ("//" ++ show k, return (repIncreasing k n)) | k <- powers2 n ]
--      , [ ("\\\\" ++ show k, return (repDecreasing k n)) | k <- powers2 n ]
--      , [ ("/\\" ++ show k, return (oscillating k n)) | k <- powers2 n ]

--      , [ ("runs " ++ show k, runs k n) | k <- powers2 n ]
--      , [ ("invs " ++ show k, invs k n) | k <- powers2 n ]
--      , [ ("dis " ++ show k, psorted k n) | k <- powers2 n ]
--      , [ ("rem " ++ show k, rems k n) | k <- powers2 n ]

     ]
--------------------------------------------------------------------------------

inputs gs			=  generate $ mapM gen $ gs
    where gen (s, g)		=  do { x <- g; return (s, x) }

--------------------------------------------------------------------------------

powers2 n			=  takeWhile (< n) $ iterate (* 2) 1

-- ========================================================================== --
