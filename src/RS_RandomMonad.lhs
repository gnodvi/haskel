%-------------------------------=  --------------------------------------------
\section{Random monad}
%-------------------------------=  --------------------------------------------

%align

> module RS_RandomMonad (module RS_RandomMonad)

> where
> import qualified RS_RandomLib as R

%align 33

High-level Interface.

Eine einfache Zustandsmonade.

> newtype RandomMonad a		=  R (R.Seed -> (a, R.Seed))
>
> unR				:: RandomMonad a -> R.Seed -> (a, R.Seed)
> unR (R a)			=  a

> instance Functor RandomMonad where
>     fmap f m			=  m >>= return . f
>
> instance Monad RandomMonad where
>     return a			=  R (\seed -> (a, seed))
>
>     m >>= k			=  R (\seed -> let (a, seed') = unR m seed
>				               in unR (k a) seed')

> setSeed			:: (Integral a) => a -> RandomMonad ()
> setSeed seed			=  R (\_ -> ((), R.twoSeed seed))

> randomInt			:: RandomMonad Int
> randomInt			=  R R.rand
>
> randomInts			:: Int -> RandomMonad [Int]
> randomInts n			=  R (R.rands n)
>
> randomInteger			:: (Integral a) => (a, a) -> RandomMonad a
> randomInteger bs		=  R (R.randomInteger bs)
>
> randomIntegers		:: (Integral a) => Int -> (a, a) -> RandomMonad [a]
> randomIntegers n bs		=  R (R.randomIntegers n bs)
>
> randomDouble			:: RandomMonad Double
> randomDouble			=  R R.randomDouble
>
> randomPerm			:: [a] -> RandomMonad [a]
> randomPerm x			=  R (R.randomPerm x)

> generate			:: RandomMonad a -> a
> generate (R f)		=  fst (f (R.twoSeed 31415926))
