%-------------------------------=  --------------------------------------------
\section{Test data}
%-------------------------------=  --------------------------------------------

%align

> module RS_TestData (module RS_TestData)

> where
> import RS_RandomMonad
> import RS_ListLib
> import RS_MergeSort
>
> default (Integer)

%align 33

Testfolgen f"ur adaptive Sortieralgorithmen.

Verbesserung: Zustandsmonade f"ur die Zufallszahlen.

Sequenz mit \emph{h"ochstens} |k| runs.

> runs				:: Int -> Int -> RandomMonad [Int]
> runs k n			=  do { as <- randomInts n;
>				        bs <- parts k as;
>				        return (concat [ sort b | b <- bs]) }

Unterteile eine Liste in maximal |k| Teile.

> parts				:: Int -> [a] -> RandomMonad [[a]]
> parts k as			=  do { ps <- randomIntegers (k - 1) (1, n - 1);
>				        return (repSplit (diffs (0 : sort ps)) as) }
>     where n			=  length as

Problem: wenn mehrere Positionen gleich sind, werden auch leere
Teillisten erzeugt.

> diffs				:: (Num a) => [a] -> [a]
> diffs []			=  []
> diffs [a]			=  []
> diffs (a1 : as@(a2 : _))	=  a2 - a1 : diffs as

Folge mit h"ochstens |kn| Inversionen.

> invs				:: Int -> Int -> RandomMonad [Int]
> invs k n			=  fmap concat (mapM randomPerm bs)
>     where bs			=  repSplit (floorRoot n : repeat k) [1 .. n]

|p|-sortierte Folge.

> psorted			:: (Num a, Enum a) => Int -> a -> RandomMonad [a]
> psorted p n			=  fmap concat (mapM randomPerm bs)
>     where bs			=  repSplit (repeat (p + 1)) [1 .. n]

Folge mit |Rem = k|.

> rems				:: Int -> Int -> RandomMonad [Int]
> rems k n			=  do { as <- randomInts n
>				      ; let (rs, as') = splitAt k as
>				      ; ps <- randomIntegers k (0, n)
>				      ;	return (inject (diffs (0 : sort ps)) rs (sort as')) }

> inject [] _ as		=  as
> inject (p : ps) (r : rs) as	=  as1 ++ [r] ++ inject ps rs as2
>     where (as1, as2)		=  splitAt p as

Integer square root (siehe Paulson, S.52).

> floorRoot			:: (Integral a) => a -> a
> floorRoot 0			=  0
> floorRoot n			=  increase (2 * floorRoot (n `div` 4))
>     where increase k
>               | k' * k' > n	=  k
>               | otherwise	=  k'
>               where k'	=  k + 1
