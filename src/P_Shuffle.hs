--------------------------------------------------------------------------------
-- /////////////////////////////////////////////////////////////////////////////
--------------------------------------------------------------------------------
 
module P_Shuffle where

--------------------------------------------------------------------------------
{-
From posting-system@google.com Mon Sep  3 14:05:52 2001
Date: Mon, 3 Sep 2001 13:59:46 -0700
From: oleg@pobox.com
Newsgroups: comp.lang.functional
Subject: _Provably_ perfect shuffle algorithms [Was: Shuffle]


This article will give two pure functional programs that _perfectly_
shuffle a sequence of arbitrary elements. We prove that the algorithms
are correct. The algorithms are implemented in Haskell and can
trivially be re-written into other (functional) languages. We also
discuss why a commonly used sort-based shuffle algorithm falls short
of perfect shuffling.

* What is the perfect shuffle
* An imperative implementation: swapping
* A sort-based algorithm and its critique
* A pure-functional perfect-shuffle algorithm
  * Naive -- lucid but inefficient -- implementation
  * Efficient implementation based on complete binary trees
* A note on repeats in a sequence of random numbers


* What is the perfect shuffle

Let's consider a sequence of n elements: (e1, e2, ...en). Intuitively, the perfect 
random shuffle will be a permutation chosen uniformly at random from the set of 
all possible n! permutations. 
Let (b1, b2, ... bn) be such a random permutation. 
Of all n! permutations, (n-1)! of them will have e1 at the first position, (n-1)! 
more will have element e2 at the first position, etc. 
Therefore, in the perfect shuffle (b1, b2, ... bn) b1 is uniformly randomly chosen 
among {e1, ... en}. The second element b2 of the shuffled sequence is uniformly
randomly chosen among {e1, ... en} - {b1}. The third element of the
shuffle b3 is uniformly randomly chosen among {e1, ... en} - {b1, b2},
etc. 
Therefore, to perform a perfect shuffle, we need a sequence of
numbers (r1, r2, ... rn) where r1 is a sample of a random
quantity uniformly distributed within [0..n-1]. r2 is an independent
sample of a random quantity uniformly distributed within
[0..n-2]. Finally, rn is 0. It easy to see that the joint probability
of (r1, r2, ... rn) is 1/n * 1/(n-1) * ... 1 = 1/n! -- which is to be
expected.


* An imperative implementation: swapping

The imperative implementation of the algorithm is well known. Let's
arrange the input sequence (e1, e2, ...en) into an array 'a' so that
initially a[i] = ei, i=1..n. 
At step 1, we choose a random number r1 uniformly from [0..n-1]. 
Thus a[1+r1] will be the first element of the
permuted sample, b1. We swap a[1+r1] and a[1]. Thus a[1] will contain
b1. At step 2, we choose a random number r2 uniformly from
[0..n-2]. a[2+r2] will be a uniform sample from {e1..en} - {b1}. After
we swap a[2] and a[2+r2], a[2] will be b2, and a[3..n] will contain
the remaining elements of the original sequence, {e1..en} -
{b1,b2}. After n-1 steps, we're done. The imperative algorithm in
OCaml has been already posted on this thread.


* A sort-based algorithm and its critique

A commonly used shuffle algorithm attaches random tags to elements
of a sequence and then sorts the sequence by the tags. Although this
algorithm performs well in practice, it does not achieve the perfect
shuffling. 
 
Let us consider the simplest example (which happens to be the worst
case): a sequence of two elements, [a b]. According to the
shuffle-by-random-keys algorithm, we pick two binary random numbers,
and associate the first number with the 'a' element, and the second
number with the 'b' element. The two numbers are the tags (keys) for
the elements of the sequence. We then sort the keyed sequence in the
ascending order of the keys. We assume a stable sort algorithm. There
are only 4 possible combinations of two binary random
numbers. Therefore, there are only four possible tagged sequences:
        [(0,a) (0,b)]
        [(0,a) (1,b)]
        [(1,a) (0,b)]
        [(1,a) (1,b)]
After the sorting, the sequences become:
        [(0,a) (0,b)]
        [(0,a) (1,b)]
        [(0,b) (1,a)]
        [(1,a) (1,b)]
As we can see, after the sorting, the sequence [a, b] is three times
more likely than the sequence [b, a]. That can't be a perfect shuffle.
 
If we use the algorithm described in the previous two sections, we
choose one binary random number. If it is 0, we leave the sequence 
[a, b] as it is. If the number is 1, we swap the elements. Both
alternatives are equally likely.

Furthermore, if we have a sequence of N elements and associate with
each element a key -- a random number uniformly distributed within [0,
M-1] (where N!>M>=N), we have the configurational space of size M^N
(i.e., M^N ways to key the sequence). There are N! possible
permutations of the sequence. Alas, for N>2 and M<N!, M^N is not
evenly divisible by N!. Therefore, certain permutations are bound to
be a bit more likely than the others.


* A pure-functional perfect-shuffle algorithm

Let's consider a functional implementation. The code will take as its
input a sequence (r1, r2, ... rn) where ri is an independent sample of
a random quantity uniformly distributed within [0..n-i]. We can obtain
r[i] as rng[i] mod (n-i), i<n; r[n] is 0. Here, rng is a sequence of
samples independently drawn from a uniform random distribution
[0..M-1]. We can obtain rng from radioactive decay measurements or
from published tables of random numbers. We can compute rng by
applying a digest function (e.g., md5) to a big collection of events
considered random (keystroke presses, Ethernet packet arriving times,
checksums in TCP packets from several IP addresses, etc).  Some
systems have a /dev/random, which is a source of cryptographically
strong random numbers. Finally, rng can be a suitable pseudo-random
number generator (PRNG). Note, PRNG are specifically tested that (rng
mod k) is uniformly distributed. The latter phrase does not mean that
the histogram of (rng mod k) is perfectly flat. Even the "real" random
numbers generally don't have the perfectly flat histogram, for any
finite number of samples. It is important that the histogram is
"statistically" flat, in the sense of chi-squared or
Kolmogorov-Smirnov criteria. A volume of Knuth discusses PRNG tests in
great detail.
-}
-- /////////////////////////////////////////////////////////////////////////////

-- ** Naive -- lucid but inefficient -- implementation

-- First, a naive functional shuffle. We note that in the sequence
-- (r1,...rn), rn is always zero. Therefore, we pass to the function
-- 'shuffle' a sequence of n-1 random numbers.


extract:: Integer -> [a] -> (a,[a])
-- given a list l, extract the n-th element and return the element and
-- the remaining list. We won't worry about the order of the list
-- of the remaining elements. n>=0. n==0 corresponds to the first element.
extract 0 (h:t) = (h,t)
extract n l = loop n l []
	where
	    loop 0 (h:t) accum = (h,accum ++ t)
	    loop n (h:t) accum = loop (n-1) t (h:accum)

-- given a sequence (e1,...en) to shuffle, and a sequence
-- (r1,...r[n-1]) of numbers such that r[i] is an independent sample
-- from a uniform random distribution [0..n-i], compute the
-- corresponding permutation of the input sequence.

shuffle:: [b] -> [Integer] -> [b]
shuffle [e] [] = [e]
shuffle elements (r:r_others) = let (b,rest) = extract r elements
				in b:(shuffle rest r_others)

-- Obviously the running time of "extract n l" is
-- O(length(l)). Therefore, the running time of shuffle is O(n^2).


-- ** Efficient implementation based on complete binary trees

-- The following is a more sophisticated algorithm

-- A complete binary tree, of leaves and internal nodes.
-- Internal node: Node card l r
-- where card is the number of leaves under the node.
-- Invariant: card >=2. All internal tree nodes are always full.
data Tree a = Leaf a | Node Integer (Tree a) (Tree a) deriving Show

fix f = g where g = f g -- The fixed point combinator

-- Convert a sequence (e1...en) to a complete binary tree

build_tree = (fix grow_level) . (map Leaf)
	where
	     grow_level self [node] = node
	     grow_level self l = self $ inner l
	     
	     inner [] = []
	     inner [e] = [e]
	     inner (e1:e2:rest) = (join e1 e2) : inner rest
	     
	     join l@(Leaf _)       r@(Leaf _)       = Node 2 l r
	     join l@(Node ct _ _)  r@(Leaf _)       = Node (ct+1) l r
	     join l@(Leaf _)       r@(Node ct _ _)  = Node (ct+1) l r
	     join l@(Node ctl _ _) r@(Node ctr _ _) = Node (ctl+ctr) l r

-- example:

-- Main> build_tree ['a','b','c','d','e']
-- Node 5 (Node 4 (Node 2 (Leaf 'a') (Leaf 'b'))
--                (Node 2 (Leaf 'c') (Leaf 'd')))
--        (Leaf 'e')


-- given a sequence (e1,...en) to shuffle, and a sequence
-- (r1,...r[n-1]) of numbers such that r[i] is an independent sample
-- from a uniform random distribution [0..n-i], compute the
-- corresponding permutation of the input sequence.

shuffle1 elements rseq = shuffle1' (build_tree elements) rseq
	where
	     shuffle1' (Leaf e) [] = [e]
	     shuffle1' tree (r:r_others) = 
		let (b,rest) = extract_tree r tree
		in b:(shuffle1' rest r_others)
		
	     -- extract_tree n tree
	     -- extracts the n-th element from the tree and returns
	     -- that element, paired with a tree with the element
	     -- deleted.
	     -- The function maintains the invariant of the completeness
	     -- of the tree: all internal nodes are always full.
	     -- The collection of patterns below is deliberately not complete.
	     -- All the missing cases may not occur (and if they do,
	     -- that's an error.
	     extract_tree 0 (Node _ (Leaf e) r) = (e,r)
	     extract_tree 1 (Node 2 (Leaf l) (Leaf r)) = (r,Leaf l)
	     extract_tree n (Node c (Leaf l) r) =
		let (e,new_r) = extract_tree (n-1) r
		in (e,Node (c-1) (Leaf l) new_r)
	     extract_tree n (Node n1 l (Leaf e)) 
			| n+1 == n1 = (e,l)
				       
	     extract_tree n (Node c l@(Node cl _ _) r) 
			| n < cl = let (e,new_l) = extract_tree n l
				   in (e,Node (c-1) new_l r)
			| otherwise = let (e,new_r) = extract_tree (n-cl) r
				      in (e,Node (c-1) l new_r)

-- /////////////////////////////////////////////////////////////////////////////
-- examples
{-
Main> shuffle1 ['a','b','c','d','e'] [0,0,0,0]
"abcde"

Note, that rseq of all zeros leaves the sequence unperturbed.

Main> shuffle1 ['a','b','c','d','e'] [4,3,2,1]
"edcba"

The rseq of (n-i | i<-[1..n-1]) reverses the original sequence of elements

Main> shuffle1 ['a','b','c','d','e'] [2,1,2,0]
"cbead"

Just some random shuffle.

The function build_tree builds a complete binary tree, of depth
ceil(log2(n)). The function 'extract_tree' traverses the tree and
rebuilds a truncated branch. This requires as many steps as the length
of the rebuilt branch, which is at most ceil(log2(n)). To be more
precise, the complexity of 'extract_tree' is ceil(log2(size(tree))),
because extract_tree keeps the tree complete. The function shuffle1'
invokes 'extract_tree' (n-1) times. Thus the overall complexity is
O(n*logn).


* A note on repeats in a sequence of random numbers

> > ...though you need to make sure that the randoms are all distinct, 
> Linear congruential generators have that property. Since they are of
> the form x' <- (ax + b) mod m, you get a repeat only when the prng
> loops. So a long-period will generate numbers without repeats.

I'm afraid this is not true. Indeed, a linear congruential generator
repeats _completely_ after a period: that is sample[i] ==
sample[i+period] for _all_ i. That does not mean that a particular
sample cannot repeat within a period. Suppose we have a sequence of
random numbers uniformly distributed within [0..M-1]. If the i-th
sample has the value of x, the (i+1)-th sample may be x as well. The
probability of this event is the same as the probability of the
(i+1)-th sample being x+1 or any other given number within
[0..M-1]. Each sample in a sequence is chosen independently. The
sample of random numbers may contain subsequences (0,0,0,0) -- which
are just as likely as (1,2,3,4) or (3,1,4,1) or any other given
sequence of four values. In fact, this is one of the PRNG tests --
making sure that all the tuples (r1,r2) or (r1,r2,r3) appear equally
likely. The infamous rand() generator fails the triples test -- and
the generator was much maligned in the G.Forsythe, M.Malcolm, C.Moler
book. BTW, given a sequence of random numbers, the probability that
two consecutive numbers in the sequence are the same is 1/M. If we're
to shuffle a sequence of 1600 elements, we are interested in random
numbers distributed within [0..1599]. We need to draw at least 1599
elements to shuffle the sequence, i.e, 1598 pairs of consecutive
drawings. Given that the probability of two consecutive samples being
the same is 1/1600, we should rather expect to see one such pair. To
be precise, the probability of occurrence of such a pair is, by
binomial distribution, 1598*(1/1600)*(1599/1600)^1597, or approx
0.368.

-}
-- /////////////////////////////////////////////////////////////////////////////

