-- /////////////////////////////////////////////////////////////////////////////
-- 
-- A functional design framework for genetic algorithms
-- Fethi A. Rabhi(UK),   Guy Lapalme(Canada),  Albert Y. Zomaya(Australia)

--------------------------------------------------------------------------------

module GA_Fpbook where

-- /////////////////////////////////////////////////////////////////////////////

-- randInts    :: Seed -> [Int]    --generate random ints
-- randDoubles :: Seed -> [Double] --generate random doubles
-- randSeeds   :: Seed -> [Seed]   --generate random seeds

--------------------------------------------------------------------------------
-- 2.2.1 The GA interface
-- Our framework presents an interface to the programmer in the form of the three
-- higher-order functions. The first one (function initialPop) creates the initial
-- population; the second one (function nextGen) computes the next generation
-- and the third one (function searchGA) controls the overall GA search. Figure
-- 1 shows the connection between these three functions and their input parameters,
-- most of which must be supplied by the programmer. The programmer may also
-- need to specify how to process the final population (which is returned by the
-- function searchGA) to obtain the desired result.

-- 2.2.2 A simple implementation

searchGA noIts test succ (pop,s)
  | noIts==0 = pop
  | test pop = pop
  | otherwise = searchGA (noIts-1) test succ (succ (pop,s))

initialPop chromSize popSize genChrom s
    = ([genChrom chromSize (rs!!i) | i<-[1..popSize]],
       rs!!0)
    where rs = randSeeds s

nextGen chromSize popSize fitness pCross cross mutOp
    = (mutate mutOp)
      .
      (crossAll cross chromSize pCross)
      .
      (select fitness popSize)


-- The searchGA function is self-explanatory: given a population, it computes
-- the next generation repeatedly until the termination condition is satisfied or a specified
-- limit is reached. The initialPop function computes the initial population
-- by calling the function that generates a (random) chromosome (i.e. function
-- genChrom) as many times as popSize.
--   The nextGen function computes the next generation by applying a selectioncrossover-
-- mutation cycle. Considering selection, determining the fittest chromosomes
-- will be made according to the fitness function. We assume a roulette wheel
-- selection where a percentage value is computed for each chromosome so that it
-- occupies a slice in the 0-100% range of size equal to the value. Selected chromosomes
-- are those for which a random percentage value falls within their range.
-- The resulting function is:

select f n (p,s)
  = ([select' f p t (fromInt (rs!!i)) | i<-[0..n-1]] , s1)
    where
          (s1:s2:_) = randSeeds s
          rs        = [r 'mod' 100 | r <- randInts s2]
          t         = sum (map f p)

-- find an item for which the cumulative fitness exceeds r
select' fitness pop 0 r = error "fitness should never be 0"
select' fitness pop t r = select'' 0 pop
    where select'' s (item : rest)
              = let relFitness = (fitness item/t)*100.0
                    in if (relFitness + s) > r
                       then item
                       else select'' (s+relFitness) rest

-- Considering crossover, the corresponding function selects two items in the
-- population then uses a random number r3 to decide whether crossover should
-- be applied. This will happen only if the random number is below the crossover
-- probability pCross. In this case, the two items are crossed using the crossover
-- function cross.

crossAll cross cl pCross (p,s)
    = (cr (randDoubles s1) [r 'mod' cl | r<-randInts s2] p,
       s3)
    where
      (s1:s2:s3:_) = randSeeds s
      cr _ _ [] = []
      cr _ _ [i] = [i]
      cr (r3:rs3) (r4:rs4) (i1:i2:r)
        | r3 > pCross = (i1:i2:(cr rs3 rs4 r))
        | otherwise = (cross r4 i1 i2)++(cr rs3 rs4 r)

-- Next, each chromosome in the population is subject to mutation. This is
-- achieved through the parameter function mutOp. Two number generators are
-- used: one to decide whether mutation should take place at all and the second to
-- decide on the value of the new chromosome.

mutate mutOp (p,s)
    = ([mutOp chrom (rs!!i) | (i,chrom)<- zip [1..] p],
       rs!!0)
      where rs = randSeeds s

--------------------------------------------------------------------------------
-- 4 SOLVING THE SRR PROBLEM WITH THE GA FRAMEWORK

-- 4.1 Assumptions and auxiliary functions

nodeList = [(1,5),(2,6),(3,11),(4,7),(8,13),(10,12),(9,14)]

-- 4.2 Defining the initial population

genChromSRR size seed
             = makeUniqueEntries [rs!!i|i<-[0..size-1]]
    where rs = randomsFrom1To size seed

randomsFrom1To n s = [ (r 'mod' n)+1 | r <- randInts s]

-- Finally, the initial population can be defined as follows:
seed = ... some random seed
pop1 = initialPop chromSizeSRR popSizeSRR
                  genChromSRR seed

-- 4.3 Computing the next generation

{-
A simple approach to the definition of a fitness function will be based on the
wiring width C of the solution. Since the fittest chromosomes are those with high
values, the fitness of a chromosome is simply equal to qM C 1 where qM is the
maximum net cut number (and also the maximum possible wiring width).
-}

fitnessSRR s = (fromInt q_M) - (cost (genSolution s))+1

-- This function uses a function genSolution converts a chromosome into its
-- interval list equivalent:

genSolution chrom = map (\i -> nodeList!!((chrom!!(i-1))-1))
                        (sort chrom)

{-
Next, the functions crossSRR and mutOpSRR that cross two chromosomes
and mutate a chromosome respectively are presented. They also rely on the function
makeUniqueEntries described earlier. Crossover is based on a single
point operator and a crossover probability of 60%. Mutation takes place with a
probability of 1% and consists of changing one integer value in the chromosome
randomly.
-}

--crossover probability
pCrossSRR = 0.6
--crossover function
crossSRR r chrom1 chrom2 =
    [makeUniqueEntries (lower1++upper2),
     makeUniqueEntries (lower2++upper1)]
    where (lower1,upper1) = splitAt r chrom1
          (lower2,upper2) = splitAt r chrom2
--mutation function
mutOpSRR chrom s
  = makeUniqueEntries [if rs2!!i > 0.01 then c else rs1!!i |
                       (i,c)<-zip [0..] chrom]
      where rs1 = randomsFrom1To chromSizeSRR s1
            rs2 = randDoubles s2
            (s1:s2:_)= randSeeds s

-- Finally, the function that computes the next generation can be defined as follows:

succSRR = nextGen chromSizeSRR popSizeSRR fitnessSRR
                  pCrossSRR crossSRR mutOpSRR

-- 4.4 Conducting the search
{-
Solving the SRR problem can now be achieved by "gluing together" all the components
defined earlier into the searchGA function. Normally, termination is
detected when the percentage difference between two successive iterations falls
below a specified threshold. Due to the small number of values that the fitness
function can take, this implementation does not carry out such a test. Instead, the
algorithm always completes the specified number of iterations.
-}

--maximum generations
noItsSRR = 20
lastpop  = searchGA noItsSRR (\_-> False) succSRR pop1

test     = (cost s,s)
 where s = genSolution
              (snd (maximum (map (\s->(fitnessSRR s,s))
                                 lastpop)))

-- As an example, consider the following execution with the list of nets defined
-- in Section 4.1 (using a population 30 chromosomes):

test
=> (2,[(2,6),(1,5),(8,13),(10,12),(4,7),(3,11),(9,14)])

-- In this case, an optimal solution is returned1. In general, several runs of the
-- algorithm are needed, each with a different set of parameters before an optimal
-- solution can be found.

---------------------------------------------------------------------------------
-- /////////////////////////////////////////////////////////////////////////////
-------------------------------------------------------------------------------
