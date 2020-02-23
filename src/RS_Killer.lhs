%-------------------------------=  --------------------------------------------
\chapter{Killer sequences}
%-------------------------------=  --------------------------------------------

%align

> module RS_Killer ( increasing, decreasing, repIncreasing, repDecreasing, oscillating, bad4merge, bad4mpp)

> where
> import RS_ListLib

%align 5

> increasing n			=  take n (interleave as as)
>     where as			=  [1 ..]
> decreasing n			=  take n (interleave as as)
>     where as			=  [n, n-1 ..]

The following generators produce lists containing many equal elements
(provided $k\ll n$).

> repIncreasing k n		=  take n (cycle [0 .. k])
> repDecreasing k n		=  take n (cycle [k, k - 1 .. 0])
> oscillating k n		=  take n (cycle ([0 .. k] ++ [k, k - 1 .. 0]))

|bad4merge| and |bad4mpp| are due to Jon Fairbairn.

> bad4merge n = take n (wcm m 1 m []) where
>          m = least_power_of_2_not_less_than n
>          wcm 0 l h acc = acc -- This never happens?
>          wcm 1 l h acc = h: acc
>          wcm 2 l h acc = h: l: acc
>          wcm n l h acc = wcm mid l h (wcm (n - mid) (l + mid - 1) (l + n - 2) acc)
>                          where mid = n `div` 2

> least_power_of_2_not_less_than n = lptbt n 1
>                                    where lptbt n p = if p >= n
>                                                      then p
>                                                      else lptbt n (2*p)

> bad4mpp n = take n (wch m 1 1 []) where
>         m = least_power_of_2_not_less_than n
>         wch 0 _ _ = \x -> x
>         wch 1 i s = \x -> i : x
>         wch n i s = (wch h (i) (2*s)) . (wch h (i+s) (2*s))
>                     where h = n `div` 2

Corresponds to the indexing scheme for Braun trees (branch according
to the LSB).

> evenOddShuffle as
>     | simple as		=  as
>     | otherwise		=  evenOddShuffle as1 ++ evenOddShuffle as2
>     where (as1, as2)		=  uninterleave as

|bad4mpp n| entspricht |evenOddShuffle [1 .. n]|.

%-------------------------------=  --------------------------------------------
\section{Counting comparisons}
%-------------------------------=  --------------------------------------------

> data Count a			=  Pair a !Int

> data Counter a		=  C (Int -> Count a)
> unC (C a)                     =  a

> instance Functor Counter where
>     fmap f m                   =  m >>= return . f
>
> instance Monad Counter where
>     return a                  =  C (\n -> Pair a n)
>
>     m >>= k                   =  C (\n -> let Pair a n' = unC m n
>                                           in unC (k a) n')
>
> inc				=  C (\n -> Pair () (n + 1))
>
> run m				=  let Pair a n = unC m 0 in (a, n)

> mergeBy			:: (Monad m) => (a -> a -> m Bool) -> [a] -> [a] -> m [a]
> mergeBy (<=)			=  merge
>     where
>     merge [] bs		=  return bs
>     merge as@(_ : _) []	=  return as
>     merge as@(a : as') bs@(b : bs')
>				=  cond (a <= b) (a <: merge as' bs)
> 				                 (b <: merge as  bs')

> cond m t e			=  m >>= \a -> if a then t else e
> a <: ms			=  ms >>= \as -> return (a : as)

> mergeSortBy (<=) as
>     | simple as		=  return as
>     | otherwise		=  do { s1 <- mergeSortBy (<=) as1
>				      ; s2 <- mergeSortBy (<=) as2
>				      ; mergeBy (<=) s1 s2 }
>     where (as1, as2)		=  halve as

> lse a b			=  inc >> return (a <= b)

|snd $ run $ mergeSortBy lse [1 .. 128]|
|snd $ run $ mergeSortBy lse $ bad4merge 1000|
|snd $ run $ mergeSortBy lse $ bad4mpp 1000|
