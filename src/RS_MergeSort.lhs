%-------------------------------=  --------------------------------------------
\chapter{Sorting by merging}
%-------------------------------=  --------------------------------------------

%align

> module RS_MergeSort (module RS_MergeSort)

> where
> import RS_LibBase
> import RS_ListLib
> import RS_IntExts

> infixr {-"\,"-} `merge`, \+/

%align 33

> sort				:: (Ord a) => [a] -> [a]
> sort				=  bottomUpMergeSort

%-------------------------------=  --------------------------------------------
\section{Top-down merge sort}
%-------------------------------=  --------------------------------------------

The archetypical functional sorting algorithm is without any doubt
merge sort. It follows the divide and conquer scheme: the input list is
split into two halves, both are sorted recursively and the results are
finally merged together.

> merge				:: (Ord a) => [a] -> [a] -> [a]
> merge				=  mergeBy (<=)
>
> (\+/)				:: (Ord a) => [a] -> [a] -> [a]
> (\+/)				=  mergeBy (<=)
>
> rmergeBy			:: Rel a -> [a] -> [a] -> [a]
> rmergeBy (<=)			=  merge
>     where
>     merge [] bs		=  bs
>     merge as@(_ : _) []	=  as
>     merge as@(a : as') bs@(b : bs')
>         | a <= b		=  a : merge as' bs
>	  | otherwise		=  b : merge as  bs'
>
> mergeSort			:: (Ord a) => [a] -> [a]
> mergeSort			=  mergeSortBy (<=)
>
> mergeSortBy			:: Rel a -> [a] -> [a]
> mergeSortBy (<=) as
>     | simple as		=  as
>     | otherwise		=  mergeBy (<=) (mergeSortBy (<=) as1)
>				                (mergeSortBy (<=) as2)
>     where (as1, as2)		=  halve as

Since the divide phase takes $\Theta(n\log n)$ time, |mergeSort| is
not lazy: |head . mergeSort| has a running time of $\Theta(n\log n)$.

An iterative variation of |mergeBy|, which is less lazy.

> imergeBy			:: Rel a -> [a] -> [a] -> [a]
> imergeBy (<=) as bs           =  imerge as bs []
>     where
>     imerge [] bs acc          =  reverseCat acc bs
>     imerge as [] acc          =  reverseCat acc as
>     imerge as@(a : as') bs@(b : bs') acc
>         | a <= b              =  imerge as' bs  (a : acc)
>         | otherwise           =  imerge as  bs' (b : acc)

> reverseCat                     :: [a] -> [a] -> [a]
> reverseCat [] bs               =  bs
> reverseCat (a : as) bs         =  reverseCat as (a : bs)

> mergeBy                        =  imergeBy

%-------------------------------=  --------------------------------------------
\section{Bottom-up merge sort}
%-------------------------------=  --------------------------------------------

The function |bottomUpMergeSort| improves the divide phase to
$\Theta(n)$ and is consequently asymptotically optimal, stable, and
lazy, but alas not in any way adaptive.

> bottomUpMergeSort		:: (Ord a) => [a] -> [a]
> bottomUpMergeSort		=  bottomUpMergeSortBy (<=)
>
> bottomUpMergeSortBy		:: Rel a -> [a] -> [a]
> bottomUpMergeSortBy (<=)	=  gfoldm [] (\a -> [a]) (mergeBy (<=))

%-------------------------------=  --------------------------------------------
\section{Straight merge sort}
%-------------------------------=  --------------------------------------------

Both |mergeSort| and |bottomUpMergeSort| take $\Theta(n\log n)$
irrespective of the presortedness of the input. If we replace the test
|simple| by |ordered| we obtain an adaptive variant which is optimal
with respect to the measure |Runs| and adaptive wrt |Inv| and |Rem|
\cite[p.~449]{ECW92Sur}.

> straightMergeSort		:: (Ord a) => [a] -> [a]
> straightMergeSort		=  straightMergeSortBy (<=)

> straightMergeSortBy		:: Rel a -> [a] -> [a]
> straightMergeSortBy (<=) as
>     | orderedBy (<=) as	=  as
>     | otherwise		=  mergeBy (<=) (straightMergeSortBy (<=) as1)
>				                (straightMergeSortBy (<=) as2)
>     where (as1, as2)		=  halve as

\Todo{To adapt to ascending as well as descending sequences we could
reverse the sublists in every step and apply Augustson's
stable/anti-stable trick.}

%-------------------------------=  --------------------------------------------
\section{Odd-even merge sort}
%-------------------------------=  --------------------------------------------

If we use a different partioning scheme, |uninterleave| instead of
|halve|, we obtain an adaptive variant which is optimal wrt |Dis = Max|
\cite[p.~450]{ECW92Sur}.

> oddEvenMergeSort		:: (Ord a) => [a] -> [a]
> oddEvenMergeSort		=  oddEvenMergeSortBy (<=)
>
> oddEvenMergeSortBy		:: Rel a -> [a] -> [a]
> oddEvenMergeSortBy (<=) as
>     | orderedBy (<=) as	=  as
>     | otherwise		=  mergeBy (<=) (oddEvenMergeSortBy (<=) as1)
>						(oddEvenMergeSortBy (<=) as2)
>     where (as1, as2)		=  uninterleave as

Unfortunately, |oddEvenMergeSort| is no longer stable. Consider, for
instance, |uninterleave [a1, a2, a3] = ([a1, a3], [a2])| and assume
that the three elements are equal. However, |oddEvenMergeSort| can be
improved so that the divide phase takes only linear time. This is left
as an instructive exercise to the reader.

%-------------------------------=  --------------------------------------------
\section{Split sort}
%-------------------------------=  --------------------------------------------

A partioning scheme which adapts to |Rem| was given by Levcopoulos
and Petersson \cite[p.~451]{ECW92Sur} and is based on a method by
Cook and Kim for removing $\Theta(|Rem(as)|)$ elements from a list
|as|, such that an ordered sequence is left over.

The function |lpDivision as| divides its input into three lists |g|,
|s|, and |l| such that |s| is sorted and |g| and |l| have the same
length which is at most |Rem(as)|. The tricky thing is to ensure that
the splitting is performed in a stable way, ie the order in which the
elements in |g| and |l| appear is the same in which they appear in
|as|. Consider the sequence |1 2 5 1 4 3 1 9 2 8 9 1|:
%
\[
\begin{array}{l||l||r}
\text{|g| and |s|} & |l| & |as| \\\hline
|1 2 5| & & |1 4 3 1 9 2 8 9 1| \\
|1 2 [5]| & |1| & |4 3 1 9 2 8 9 1| \\
|1 2 [5] 4| & |1| & |3 1 9 2 8 9 1| \\
|1 2 [5 4]| & |3 1| & |1 9 2 8 9 1| \\
|1 [2 5 4]| & |1 3 1| & |9 2 8 9 1| \\
|1 [2 5 4] 9| & |1 3 1| & |2 8 9 1| \\
|1 [2 5 4 9]| & |2 1 3 1| & |8 9 1| \\
|1 [2 5 4 9] 8| & |2 1 3 1| & |9 1| \\
|1 [2 5 4 9] 8 9| & |2 1 3 1| & |1| \\
|1 [2 5 4 9] 8 [9]| & |1 2 1 3 1| &
\end{array}
\]
The data type |Region| is designed to represent |g| and |s|. Elements
in |g| are grouped to allow for efficient access to the last element in
|s|. For instance, |1 [2 5 4 9] 8 [9]| is essentially represented by |G
(S (G (S Nil 1) [2, 5, 4, 9]) 8) [9]|.

> type Sequ a			=  [a] -> [a]
>
> data Region a			=  Nil
>				|  S (Region a) a
>				|  G (Region a) (Sequ a)
>
> single			:: a -> Sequ a
> single a			=  \x -> a : x
>
> g				:: Region a -> Sequ a -> Region a
> g Nil gs			=  G Nil gs
> g (S s a) gs			=  G (S s a) gs
> g (G s gs1) gs2		=  G s (gs1 . gs2)
>
> lpDivisionBy			:: Rel a -> [a] -> ([a], [a], [a])
> lpDivisionBy (<=) []          =  ([], [], [])
> lpDivisionBy (<=) (a : as)    =  lp (S Nil a) [] as
>     where
>     lp s l []              	=  (g [], reverse s', reverse l)
>         where (g, s')		=  lpPart s
>     lp Nil l (a : as)		=  lp (S Nil a) l as
>     lp s@(G Nil gs) l (a : as)=  lp (S s a) l as
>     lp s@(G (S s' m) gs) l (a : as)
>         | m <= a		=  lp (S s a) l as
>         | otherwise		=  lp (g s' (single m . gs)) (a : l) as
>     lp (G (G _ _) _) _ (_ : _)=  error "lp"
>     lp s@(S s' m) l (a : as)
>         | m <= a		=  lp (S s a) l as
>         | otherwise		=  lp (g s' (single m)) (a : l) as
>
> lpPart			:: Region a -> ([a] -> [a], [a])
> lpPart Nil			=  (id, [])
> lpPart (S s a)		=  (g, a : s')
>     where (g, s')		=  lpPart s
> lpPart (G s gs)		=  (gs . g, s')
>     where (g, s')		=  lpPart s

Unfortunately, the relative order between equal elements in |g ++ s ++ l|
is not the same as in |as|. Hence, |lpMergeSort| is not stable either.

> lpMergeSort			:: (Ord a) => [a] -> [a]
> lpMergeSort			=  lpMergeSortBy (<=)
>
> lpMergeSortBy			:: Rel a -> [a] -> [a]
> lpMergeSortBy (<=) as
>     | simple as		=  as
>     | otherwise		=  mergeBy (<=) (lpMergeSortBy (<=) as1)
>				       (mergeBy (<=) s (lpMergeSortBy (<=) as2))
>     where (as1, s, as2)	=  lpDivisionBy (<=) as

\Todo{To adapt to ascending as well as descending sequences we could
reverse the sublists in every step. \NB stability is already lost.}

%-------------------------------=  --------------------------------------------
\section{Adaptive merge sort}
%-------------------------------=  --------------------------------------------
%format sort1			=  sort "_1"
%format sort2			=  sort "_2"
%format sort3			=  sort "_3"

If we combine |halve|, |uninterleave|, and |lpDivision| we obtain
a sorting algorithm which is adaptive wrt |Exc|, |Dis|, |Inv|, |Rem|,
and |Runs| \cite[p.~451]{ECW92Sur}.

> adaptiveMergeSort		:: (Ord a) => [a] -> [a]
> adaptiveMergeSort		=  adaptiveMergeSortBy (<=)
>
> adaptiveMergeSortBy		:: Rel a -> [a] -> [a]
> adaptiveMergeSortBy (<=)	=  sort1
>     where
>     (\+/)			=  mergeBy (<=)
>
>     sort1 as
>         | simple as		=  as
>         | otherwise		=  sort2 as1 \+/ s \+/ sort2 as2
>         where (as1, s, as2)	=  lpDivisionBy (<=) as
>
>     sort2 as			=  sort3 as1 \+/ sort3 as2
>         where (as1, as2)	=  uninterleave as
>
>     sort3 as			=  sort1 as1 \+/ sort1 as2
>         where (as1, as2)	=  halve as

\Todo{How about reversing the lists in the first step in order to adapt
to descending sequences as well? cf.~\cite[p.~52]{ECW91Pra}}

%-------------------------------=  --------------------------------------------
\section{Natural merge sort}
%-------------------------------=  --------------------------------------------

The function |straightMergeSort| somehow guesses the number of runs. It is
more efficient to group the input into runs beforehand.

> naturalMergeSort		:: (Ord a) => [a] -> [a]
> naturalMergeSort		=  naturalMergeSortBy (<=)
>
> naturalMergeSortBy		:: Rel a -> [a] -> [a]
> naturalMergeSortBy (<=)	=  foldm (mergeBy (<=)) [] . runsBy (<=)
>
> runsBy			:: Rel a -> [a] -> [[a]]
> runsBy (<=) []		=  [[]]
> runsBy (<=) (a : as)		=  upRun a [] as
>     where
>     upRun m r []		=  [reverse (m : r)]
>     upRun m r (a : as)
>         | m <= a		=  upRun a (m : r) as
>         | otherwise		=  reverse (m : r) : upRun a [] as

Natural merge sort was first studied in the context of external
sorting.  The hbc library contains a similar function.

The function |runs| recognized only ascending runs. With little
additional effort we can also detect descending runs.

> symmetricNaturalMergeSort	:: (Ord a) => [a] -> [a]
> symmetricNaturalMergeSort	=  symmetricNaturalMergeSortBy (<=)
>
> symmetricNaturalMergeSortBy	:: Rel a -> [a] -> [a]
> symmetricNaturalMergeSortBy (<=)
>				=  foldm (mergeBy (<=)) [] . upDownRunsBy (<=)
>
> upDownRunsBy			:: Rel a -> [a] -> [[a]]
> upDownRunsBy (<=) []		=  []
> upDownRunsBy (<=) (a : as)	=  upDownRun a as
>     where
>     upDownRun a []		=  [[a]]
>     upDownRun a1 (a2 : as)
>         | a1 <= a2		=  upRun   a2 [a1] as
>         | otherwise		=  downRun a2 [a1] as
>     upRun m r []		=  [reverse (m : r)]
>     upRun m r (a : as)
>         | m <= a		=  upRun a (m : r) as
>         | otherwise		=  reverse (m : r) : upDownRun a as
>
>     downRun m r []		=  [m : r]
>     downRun m r (a : as)
>         | m <= a		=  (m : r) : upDownRun a as
>         | otherwise		=  downRun a (m : r) as

\NB To preserve stability |downRun| uses only \emph{strictly}
decreasing sequences: |[n, n, n-1, n-1, .. 1, 1]| is split into |n|
runs. Does anybody know of a better solution?

%-------------------------------=  --------------------------------------------
\section{CAML's flip sort}
%-------------------------------=  --------------------------------------------

> flipSort	                :: (Ord a) => [a] -> [a]
> flipSort                      =  flipSortBy (<=)

> flipSortBy	                :: Rel a -> [a] -> [a]
> flipSortBy (<=) as
>     | n < 2                   =  as
>     | otherwise               =  fst (sort n as)
>     where
>     n                         =  length as
>
> --  sort 1 xs              =  case xs of  (x1 : xs) -> ([x1], xs)
>     sort 2 xs                 =  case xs of { (x1 : x2 : xs) -> 
>                                  (if x1 <= x2 then [x1, x2] else [x2, x1], xs) }
>     sort 3 xs                 =  case xs of { (x1 : x2: x3 : xs) ->
>                                  ( if x1 <= x2
>                                    then if x2 <= x3 then [x1, x2, x3]
>                                         else if x1 <= x3 then [x1, x3, x2]
>                                              else [x3, x1, x2]
>                                    else if x1 <= x3 then [x2, x1, x3]
>                                         else if x2 <= x3 then [x2, x3, x1]
>                                              else [x3, x2, x1]
>                                  , xs) }
>     sort n xs                 =  (mergeRevRevBy (<=) r1 r2, ys2)
>         where k               =  n `div` 2
>               (r1, ys1)       =  revSort k       xs
>               (r2, ys2)       =  revSort (n - k) ys1
>     
> --  revSort 1 xs              =  case xs of (x1 : xs) -> ([x1], xs)
>     revSort 2 xs              =  case xs of { (x1 : x2: xs) ->
>                                  ( if x1 <= x2 then [x2, x1] else [x1, x2], xs) }
>     revSort 3 xs              =  case xs of { (x1 : x2: x3 : xs) ->
>                                  ( if x1 <= x2
>                                    then if x2 <= x3 then [x3, x2, x1]
>                                         else if x1 <= x3 then [x2, x3, x1]
>                                              else [x2, x1, x3]
>                                    else if x1 <= x3 then [x3, x1, x2]
>                                         else if x2 <= x3 then [x1, x3, x2]
>                                              else [x1, x2, x3]
>                                  , xs) }
>     revSort n xs              =  (revMergeBy (<=) s1 s2, ys2)
>         where k               =  n `div` 2
>               (s1, ys1)       =  sort k       xs
>               (s2, ys2)       =  sort (n - k) ys1

Stably merge two ordered lists into a reverse ordered list.

> revMergeBy		        :: Rel a -> [a] -> [a] -> [a]
> revMergeBy (<=) as bs         =  merge as bs []
>     where
>     merge [] bs acc           =  reverseCat bs acc
>     merge as [] acc           =  reverseCat as acc
>     merge as@(a : as') bs@(b : bs') acc
>         | a <= b              =  merge as' bs  (a : acc)
>         | otherwise           =  merge as  bs' (b : acc)

Stably merge two reverse ordered lists into an ordered list.

> mergeRevRevBy		        :: Rel a -> [a] -> [a] -> [a]
> mergeRevRevBy (<=) as bs      =  merge as bs []
>     where
>     merge [] bs acc           =  reverseCat bs acc
>     merge as [] acc           =  reverseCat as acc
>     merge as@(a : as') bs@(b : bs') acc
>         | a <= b              =  merge as  bs' (b : acc)
>         | otherwise           =  merge as' bs  (a : acc)

%-------------------------------=  --------------------------------------------
\section{Online adaptive merge sort}
%-------------------------------=  --------------------------------------------

Olin Shiver's family of online adaptive merge sorts.

> igetRunBy (<=) a as           =  (n, reverse rs, tl)
>     where
>     (n, rs, tl)               =  getRun 1 a [a] as
>     getRun i p acc []         =  (i, acc, [])
>     getRun i p acc xs@(x : xs')
>         | p <= x              =  getRun (i + 1) x (x : acc) xs'
>         | otherwise           =  (i, acc, xs)

> getRunBy                      =  igetRunBy

Spot increasing and descreasing runs and forces runs to be at least
three elements long.

> igetRun3By (<=) a []          =  (1, [a], [])
> igetRun3By (<=) a1 (a2 : as)
>     | a1 <= a2                =  up   2 a2 [a2, a1] as
>     | otherwise               =  down 2 a2 [a2, a1] as
>     where
>
>     up i p run []             =  (i, reverse run, [])
>     up i p run as@(a : as')
>         | p <= a              =  up (i + 1) a (a : run) as'
>         | i > 2               =  (i, reverse run, as)
>         | otherwise           =  (3, if a1 <= a then [a1, a, a2] else [a, a1, a2], as')
>
>     down i p run []           =  (i, run, [])
>     down i p run as@(a : as')
>         | not (p <= a)        =  down (i + 1) a (a : run) as'
>         | i > 2               =  (i, run, as)
>         | otherwise           =  (3, if a1 <= a then [a2, a1, a] else [a2, a, a1], as')

> getRun3By                     =  igetRun3By

> onlineMergeSort	        :: (Ord a) => [a] -> [a]
> onlineMergeSort	        =  onlineMergeSortBy (<=)

> onlineMergeSortBy (<=) []     =  []
> onlineMergeSortBy (<=) (a : as)
>                               =  s
>     where 
>     (lr, r, tl)               =  getRunBy (<=) a as
>     (s, _, _)                 =  grow r lr tl infinity
>
>     grow s ls u lw
>         | ls >= lw            =  (s, ls, u)
>         | otherwise           =  case u of
>                                  []         -> (s, ls, u)
>                                  u1 : urest -> grow (mergeBy (<=) s t) (ls + lt) u3 lw
>                                      where
>                                      (lr, r, u2) =  getRunBy (<=) u1 urest
>                                      (t, lt, u3) =  grow r lr u2 (leastPowerOf2 ls)

> onlineMergeSort'	        :: (Ord a) => [a] -> [a]
> onlineMergeSort'	        =  onlineMergeSortBy' (<=)

> onlineMergeSortBy' (<=) []     =  []
> onlineMergeSortBy' (<=) (a : as)
>                               =  s
>     where 
>     (lr, r, tl)               =  getRunBy (<=) a as
>     (s, _, _)                 =  grow r lr tl infinity
>
>     grow s ls u lw
>         | ls >= lw            =  (s, ls, u)
>         | otherwise           =  case u of
>                                  []         -> (s, ls, u)
>                                  u1 : urest -> grow (mergeBy (<=) s t) (ls + lt) u3 lw
>                                      where
>                                      (lr, r, u2) =  getRunBy (<=) u1 urest
>                                      (t, lt, u3) =  grow r lr u2 (ls `div` 2) -- !!!!

> onlineMergeSort3	        :: (Ord a) => [a] -> [a]
> onlineMergeSort3	        =  onlineMergeSort3By (<=)

> onlineMergeSort3By (<=) []     =  []
> onlineMergeSort3By (<=) (a : as)
>                               =  s
>     where 
>     (lr, r, tl)               =  getRun3By (<=) a as
>     (s, _, _)                 =  grow r lr tl infinity
>
>     grow s ls u lw
>         | ls >= lw            =  (s, ls, u)
>         | otherwise           =  case u of
>                                  []         -> (s, ls, u)
>                                  u1 : urest -> grow (mergeBy (<=) s t) (ls + lt) u3 lw
>                                      where
>                                      (lr, r, u2) =  getRun3By (<=) u1 urest
>                                      (t, lt, u3) =  grow r lr u2 (leastPowerOf2 ls)

> {-
> doubleup                      :: Int -> Int -> Int
> doubleup i j                  =  lp i
>     where
>     lp i
>         | i2 > j              =  i
>         | otherwise           =  lp i2
>         where i2              =  i + i
> -}

> infinity                      :: Int
> infinity                      =  1000000000