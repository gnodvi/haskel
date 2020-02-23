%-------------------------------=  --------------------------------------------
\section{Random library}
%-------------------------------=  --------------------------------------------

%align

> module RS_RandomLib (module RS_RandomLib)

> where
> import Array
> import ST
> import Int 

%align 33

Stolen from the library |Random|.

> randomDouble        		:: Seed -> (Double, Seed)
> randomDouble seed		=  (fromIntegral r * 4.6566130638969828e-10, seed')
>     where (r, seed')		=  rand seed

Zufallszahl in einem vorgegebenen Intervall.

> randomInteger			:: (Integral a) => (a, a) -> Seed -> (a, Seed)
> randomInteger (l, h) seed
>     | l > h			=  error "Random.random: Empty interval"
>     | otherwise		=  (convert b rs `mod` k + l, seed')
>     where k			=  h - l + 1
>           b			=  2147483561
>           n			=  iLogBase b k
>           (rs, seed')		=  rands n seed

> randomIntegers		:: (Integral a) => Int -> (a, a) -> Seed -> ([a], Seed)
> randomIntegers m (l, h) seed
>     | l > h			=  error "Random.random: Empty interval"
>     | otherwise		=  (f rs, seed')
>     where k			=  h - l + 1
>           b			=  2147483561
>           n			=  iLogBase b k
>           (rs, seed')		=  rands (m * n) seed
>           f []                =  []
>           f is                =  convert b xs `mod` k + l : f is'
>             where (xs, is')   =  splitAt n is

> convert			:: (Num a) => a -> [Int] -> a
> convert base			=  foldr (\i r -> fromIntegral i + r * base) 0

Random permutation. 

> randomPerm			:: [a] -> Seed -> ([a], Seed)
> randomPerm x seed		=  runST (
>				   do { a <- newSTArray bs undefined;
>				        sequence [
>				            writeSTArray a i e
>				        | (i, e) <- zip is x ];
>				        sequence [
>				            swap a i r
>				        | (i, r) <- zip is [ r `mod` n| r <- rs] ];
>				        p <-mapM (readSTArray a) is;
>				        return (p, seed') })
>     where n			=  length x
>           bs			=  (0, n - 1)
>           is			=  range bs
>           (rs, seed')		=  rands n seed

> type Seed			=  (Int, Int)

> rand                		:: Seed -> (Int, Seed)
> rand (s1, s2)
>     | z < 1			=  (z + 2147483562, (s1'', s2''))
>     | otherwise		=  (z             , (s1'', s2''))
>     where k			=  s1 `div` 53668
>           s1'			=  40014 * (s1 - k * 53668) - k * 12211
>           s1'' | s1' < 0	=  s1' + 2147483563
>                | otherwise	=  s1'
>           k'			=  s2 `div` 52774
>           s2'			=  40692 * (s2 - k' * 52774) - k' * 3791
>           s2'' | s2' < 0	=  s2' + 2147483399
>                | otherwise    =  s2'
>           z			=  s1'' - s2''

> rands                		:: Int -> Seed -> ([Int], Seed)
> rands 0 seed			=  ([], seed)
> rands (n + 1) seed		=  (r : rs, seed2)
>     where (r, seed1)		=  rand seed
>           (rs, seed2)		=  rands n seed1

> twoSeed			:: (Integral a) => a -> (Int, Int)
> twoSeed s
>     | s < 0 			=  twoSeed (- s)
>     | otherwise		=  (fromIntegral (s1 + 1), fromIntegral (s2 + 1))
>     where (q, s1)		=  s `divMod` 2147483562
>           s2			=  q `mod` 2147483398

Auxiliary functions.

> swap				:: (Ix a) => STArray s a b -> a -> a -> ST s ()
> swap a i j			=  do { x <- readSTArray a i;
>				        y <- readSTArray a j;
>				        writeSTArray a i y;
>				        writeSTArray a j x }

> iLogBase			:: (Integral a, Num b) => a -> a -> b
> iLogBase b i
>     | i < b 			=  1
>     | otherwise		=  1 + iLogBase b (i `div` b)