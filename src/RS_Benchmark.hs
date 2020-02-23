--------------------------------------------------------------------------------
-- /////////////////////////////////////////////////////////////////////////////
--------------------------------------------------------------------------------
-- Benchmarks

--------------------------------------------------------------------------------

module RS_Benchmark (module RS_Benchmark)

where

import List
import IOExts
import CPUTime

import RS_Force
import RS_ListLib (cjustifyWith, rjustify)

--------------------------------------------------------------------------------

timeIt		        	:: IO a -> IO Integer
timeIt cmd			=  do performGC
                                      t1 <- getCPUTime
				      cmd
                                      t2 <- getCPUTime
				      return (milliSeconds (t2 - t1))
   where milliSeconds t	=  (t + 500000000) `div` 1000000000

time				:: IO a -> IO ()
time cmd			=  do t <- timeIt cmd
				      putStrLn (showMilliSeconds t)

showMilliSeconds		:: Integer -> String
showMilliSeconds t		=  show (t `div` 1000) ++ "." ++ frac
    where ms			=  (t `mod` 1000) `div` 10
          frac | ms < 10	=  "0"  ++ show ms
               | otherwise	=          show ms

--------------------------------------------------------------------------------

benchmark			:: (Force b, Force a) => [(String, a -> b)] -> [(String, a)] -> IO ()
benchmark cs gs		        =
    let sgs = map fst gs in
    force sgs `seq`
    do putTitle "Running programs"
       rts <- sequence [
                  sequence [
	                do putStrLn ("** Running " ++ strc ++ " on " ++ strg)
                           force g `seq` timeIt (return (force (c g) `seq` "done") >>= putStrLn)
                  | (strc, c) <- cs ]
              | (strg, g) <- gs ]
       putStrLn ""
       putTitle "Results"
       printResult (map fst cs) sgs rts

--------------------------------------------------------------------------------

putTitle			:: String -> IO ()
putTitle s			=  do putStrLn (cjustifyWith '*' 78 "")
				      putStrLn (cjustifyWith '*' 78 (" "++s++" "))
				      putStrLn (cjustifyWith '*' 78 "")
				      putStrLn ""

--------------------------------------------------------------------------------

printResult			:: [String] -> [String] -> [[Integer]] -> IO ()
printResult col0 row0 results
				=  putStrLn (formatTable table)
    where table	        	=  ("" : col0 ++ ["*best*"]) :
				   [ e0 : [ showMilliSeconds e | e <- col ]
				       ++ [ bestOf (zip col0 col) ]
				   | (e0, col) <- zip row0 results ]

bestOf		        	:: (Ord b) => [(a, b)] -> a
bestOf		        	=  fst . foldl1 min2
    where min2 p@(_,a) q@(_,b)
              | a <= b  	=  p
              | otherwise	=  q

formatTable			:: [[String]] -> String
formatTable table		=  unlines (map (concat . intersperse "|") lines)
    where lines	        	=  transpose [
				     [ " " 
				       ++ rjustify (maximum [ length e | e <- col ]) e
				       ++ " "
				     | e <- col ]
			         | col <- table ]

--------------------------------------------------------------------------------
-- /////////////////////////////////////////////////////////////////////////////
--------------------------------------------------------------------------------
