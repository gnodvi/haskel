
-- /////////////////////////////////////////////////////////////////////////////

> module GA_Brown00 where

-- /////////////////////////////////////////////////////////////////////////////
-- /////////////////////////////////////////////////////////////////////////////

      A genetic algoruthm framework using Haskell 
      Deryck F. Brown  A. Beatriz Garmendia-Doval   John A.W. McCall

             School of Computer and Math Sciences
             The Robert Gordon University,
             St Andrew Street, Aberdeen AB25 1HG, Scotland.
             {dbbgd jm}@scms. rgu ac.uk

--------------------------------------------------------------------------------
Abstract

We present a generic functional framework for the construction of GAs using Haskell. 
The framework consists of a general structure for evolving populations, a library 
of standard functions and operators, and two user-defined modules used to implement 
specific GAs. Four very different GAs, taken from the literature, are implemented in 
this framework demonstrating its flexibility. The framework also allows precise control 
of the stochastic processes underlying the GA, allowing experimentation, as demonstrated 
by the Inverse Pendulum Problem. The framework provides a bridge between emerging 
theories and practical implementation of GAs Keywords: genetic algorithm, functional 
programming.

1 Introduction

Genetic Algorithms (GAs) [1] are a heuristic search technique inspired by the Darwinian 
survival of the fittest concept. GAs have been successfully applied to an impressive 
range of difficult problems, but the typically ad-hoc use of these techniques has hindered 
the development of a theoretical understanding of their operation. Without such an 
understanding, further progress is unlikely.
   Research aimed at understanding and improving GA performance has traditionally 
explored various methods of GA configuration, GA parameter selection, and hybridization 
with other techniques. However, the results obtained from such work tend to be tied 
to special features of specific problems, and it is hard to extract general principles. 
On the other hand, traditional theory in the area, while stimulating interest, has 
delivered little in terms of implementable results. More recent approaches have attempted 
to construct theoretical models of GAs that capture their essential properties [2] 
To be of any interest such models must also provide assistance in developing GAs.
   This article is an attempt to move in this direction. Section 2 describes a generic 
functional framework for the construction of genetic algorithms. In Section 3, we 
demonstrate that the framework encapsulates the essential properties of GAs by 
specializing it to reproduce a range of published applications. Section 4 illustrates 
how the framework can be used to analyze a specific GA under varying configuration and 
parameter values. Finally, Section 5 concludes and suggests how the framework might be 
used to support a theory of genetic algorithms

--------------------------------------------------------------------------------
2 The functional framework

A genetic algorithm may be described loosely as having the following structure:

1. Create an initial population by randomly generating individuals
2. Repeat until best individual created or maximum number of generations:
  (a) Assign a fitness value to each member of the current population, using the evaluation 
      function.
  (b) Select a pool of individuals that will act as parents, using fitness as a selection 
     criterion.
  (c) Mate a group of parents to create offspring.
  (d) Combine the offspring and the current population to create a new population.

   This structure naturally leads to a functional approach to defining a GA. Each action 
"select", "mate" and "combine" becomes a function to be defined, in addition to the 
evaluation function. The operation of the GA itself is a function which has as input 
a starting population and some random seeds, and outputs a set of successive populations 
In what follows, the short sections of code given are in the functional language Haskell [3].
   The difficulty in setting up this structure is in choosing the right level of 
genericity. Too generic and the framework becomes trivial, lacking sufficient complexity 
to support a meaningful theory. Too specific and the properties of the framework will 
not be those of a sufficiently wide class of GAs.
   The framework is split into three levels: population handling, the mechanics of selection, 
and the evaluation of individuals. At the population level, movement from one population to 
the next is controlled by a function 'breed' defined as follows:

--------------------------------------------------------------------------------

> breed :: Population -> Prob Population
> breed pop
>     = selectAll pop           >>= \pool      ->
>       (applyMate (mate) pool) >>= \offspring ->
>       combine pop offspring

   [Here Prob is a monad used to give precise control over random numbers used in the GA. 
We will address Prob in more detail in Section 4.]
    First "selectAll pop" selects groups of parents from the current population to form 
the mating pool.
The result is an infinite list of such groups. This list then given to the second step
where "applyMate (mate) pool" applies the mating function "mate" to each element of the 
list in turn. The mating function combines the parents using the genetic operators to 
produce the offspring. The overall result of this step is, therefore, an infinite list 
of new offspring. This list is then given to the final step where "combine pop offspring" 
combines the original population and the offspring to produce the new population.
    The mechanics of selection is handled by a library of generic operators where
 standard operators are implemented:
 * Crossover: 1-Point, order, etc.
 * Mutate: bit wise, one position, etc.
 * Selection: roulette-wheel, tournament, linear ranking, etc.
 * Combine: generational, steady state, etc.

For example, the module for 1-point crossover is as follows:

> onePointCross :: [Chromosome] -> Prob [Chromosome]
> onePointCross [p1, p2]
>   = posProb 1 >>= \[pos] ->
>     let
>       (v1', v1'')  = splitAt pos (alleles p1)
>       (v2', v2'')  = splitAt pos (alleles p2)
>       bs1   = v1' ++ v2''
>       bs2   = v2' ++ v1''
>     in
>       newChrom bs1 >>= \c1 ->
>       newChrom bs2 >>= \c2 ->
>       return ([c1, c2])

   onePointCross takes a pair of chromosomes, pi and p2, and a random number from Prob 
used to generate the position pos at which crossover is to take place. Each parent 
chromosome is split at po into segments v   and v''. The segments are recombined and 
returned as child chromosomes cl and c2
   This allows a wide spectrum of GAs to be implemented within the framework, while at 
the same time standardising their configuration. This makes possible widely applicable 
abstract reasoning. Another practical advantage is that GAs can be implemented with 
minimal writing of new code.
   One of the many benefits of using Haskell is the ability to specify functions that 
operate on (potentially) infinite data structures. A program will evaluate only as 
much of an infinite structure as required to compute its result. This liberates our 
program from considerations such as the number of offspring needed -- we can generate 
the infinite list of offspring and rely on the combine function to use only as many 
offspring as necessary to construct the new population. For example, a generational 
GA will typically require as many offspring as the size of the population, whereas a 
steady state GA will require only a single pair of offspring. The actual number of 
offspring required will feed backwards to force the mate function to mate sufficient 
sets of parents, and the seect function to select the required number of parents from 
the original population.
   Specification of the framework to a particular problem now boils down to the creation 
of two user defined modules. The first, ChromUser, contains the definition of a chromosome, 
the fitness function and GA parameters. The second, User, defines seectAll_c mate_c, 
and combine_c. selectAll_c defines how chromosomes are selected for mating from the 
current population; mate_c defines how a set of chromosomes is mated to produce offspring; 
and combine_c defines how offspring are combined with the current population to create 
the next population. In most cases, these routines will simply call on functions already 
in the library.

--------------------------------------------------------------------------------
3 Probms impmented

As tests of the genericity and usefulness of our framework, this section presents several 
problems from the literature that we have implemented using it. In each case, we were able 
to reproduce the results given in the original references. We now briefly describe the 
problems, indicating the amount of work needed in each case to adapt the framework.

--------------------------------------------------------------------------------
3.1     Simple function optimization

This problem was taken from [4, pages 18-22] and is to find x in the range [-1, 2] 
which maximizes the following function f:

f(x) = xsm(10irx) + 1

Each chromosome is a binary vector of length 22, representing a number x in the given 
range. The evaluation function is equal to the value of f(x). Crossover is applied 
with probability 0.25 and the resulting offspring are mutated with probability 0.01. 
Population size is equal to 50. All of these are defined in ChromUser as follows:

> module ChromUser where
> import Prob
> type Probability  = Double
> type RandomNumber = Integer
> maxRandom = 10000   :: Integer

The following are the default types for the Chromosome. The user will have to modify 
this part if his chromosomes are not bit strings.

> type Allete   = Bool
> type Fitness  = Float
> type Position = Int
> newtype Chromosome = Chromosome (Fitness, [Allele])

Next we set the chromosome length, population size, mutation rate and operator rate.

> chromLength = 22   :: Int
> popSize            :: Prob Int
> popSize = getProb snd
> mutate_p = 0.01    :: Probability
> operate_p = 0.25   :: Probability

bits2int,  bits2double, and evluate together define the evaluation function. The first 
two are generic for bitstring chromosomes.

> bits2int           :: [Allele] -> Int
> bits2int bs
>   = sum [p | (p, b) <- zip powers2 bs, b]
>     where         powers2 = 1 : map (2 *) powers2

> bits2double        :: [Allele] -> Double
> bits2double bs
>   = -1.0 + (x' * 3) / (2 ^^22 -1)
>     where   x' = powers2 = 1 : map (2 *) powers2

> evaluate           :: [Allele] -> Double
> evaluate bs
>   = x * sin(10 * pi * x) + 1.0
>     where             x = bits2double bs

newChrom sets up a chromosome as a list of alleles paired with the associated 
fitness value.

> newChrom           :: [Allele] -> Chromosome
> newChrom bs = Chromosome (evaluate bs, bs)

The genetic operators used are one-point crossover and bit-wise mutation. 
Selection is roulette wheel selection. All of these are defined in the library, 
and the User module is as follows.

--------------------------------------------------------------------------------
> module User (module GenericOper, combine_c, selectall_c, mate_c) where
> import GenericOper  {GenericOper is the library of pre-defined operators}

bitwisemut is the predefined bitwise mutation operator.  It has as input a function 
that mutates one allele.

> mutate_c            :: Chromosome -> Prob Chromosome
> mutate_c = bitwisemut (\a -> return (not a)) 

onePointCross is a pre-defined crossover operator

> operate_c           :: [Chromosome] -> Prob [Chromosome]
> operate_c = onePointCross 

operateThenMutate is a prdefined mating opeation combining a cosoveype opeation 
followed by a mutation

> mate_c              :: [Chromosome] -> Prob [Chromosome]
> mate_c = opereateThenMutate operate_c mutate_C

Roulette wheel selection is also predefined.

> select_c            :: Population -> Prob [Chromosome]
> select_c = rouletteselect

replaceSelectAll uses seect_c and pop to select (popsize/2) groups of 2 parents each.

> selectAll_c         :: population -> Prob [[Chromosome]]
> selectAll_c pop
>   = popsize >>= \ps -> replaceSelectAll select_c pop (ps 'div' 2) 2

generelitismis a pre-defined repopulation strategy generational with elitism 
(preserving the best member of the current population)

> combine_c           :: Population -> [Chromosome] -> Prob Population
> combine_c = generalitism 1


3.2. Traveling salesman problem

    The Traveling Salesman Problem (TSP) is taken from [5]. A chromosome represents a 
tour of the cities. Each city is represented by a number, so that each allele is a 
number between 0 and the number of cities The length of the chromosome is equal to 
the number of cities      he initial population is generated by creating random 
permutations of the cities.
    The genetic operator used is the edge recombination operator. This operator is not 
considered standard, and it was therefore not included in the library. In this case, 
a new module, EdgeOp, was created. It contains the operator edgerecombination. 
The edge recombination operator has to make random repairs to ensure that valid paths 
result For this reason, no mutation operator is used. he User module changes to reflect 
this as follows:

> mate_c              :: [Chromosome] -> Prob [Chromosome]
> mate_c = edgerecombination

The selection operator used is linear selection. The combination used is steady state.

-- /////////////////////////////////////////////////////////////////////////////

3. Invered pndulum

This problem was taken from [6]. The problem is to balance an inverted pendulum 
that sits over a cart This is achieved by pushing the cart to the right or to the 
left. The cart can only move in one dimension on a finite track. A representation 
of the system can be seen in Figure 1.
    The genetic algorithm is used in this case to train the weights for a neural 
network with five input nodes, five hidden nodes and one output node controlling 
the inverted pendulum. Each chromosome is implemented as a list of 36 real numbers: 
the values of the weights, plus a number that represents the probability of crossover. 
The initial population is created at random. The evaluation function counts the 
number of pushes the system gives to the cart before the cart reaches the end of 
the track or the pole reaches an unstable position. If the neural network manages 
to balance the pendulum for 120,000 time steps, it is considered to have learned 
how to balance the pendulum. Therefore, in this case the evaluation function requires 
the implementation of the neural network in Haskell. The initial position of the 
system can be chosen to be either balanced (perpendicualr to the floor), or random 
and different for each evaluation.

Figure 1: The Inverted Pendulum	
Figure 2: Fitnes funcion for Intons

   Two individuals are selected according to their relative fitness using linear-bias 
selection. Crossover is applied with probability determined by the crossover 
probability allele of the string selected as parent 1; otherwise mutation is performed 
on parent 1. The offspring always inherits the crossover probability of parent 1. 
If parent 1 has a higher fitness than the offspring, the offspring's crossover 
probability is incremented by a factor of 0.10 (to maximum 0.95); otherwise it is 
decreased by a factor of 0.10 (to minimum 0.05).
   The new offspring are evaluated and inserted in the population according to fitness 
Iteration continues until error is acceptable or MAXITERATIONS = True.
   Mutation involves mutating all weights on the first selected individual by adding 
a random value with range plus/minus 10.0. This is not in the library and must be 
defined by the user. Crossover is not performed if the parents differ by two or fewer 
alleles. Otherwise, recombination of the strings is carried out using one-point 
crossover between the first and last positions at which the parents have different 
weight values. Here some of the one point crossover function can be reused, but there 
is still an element of user definition.

--------------------------------------------------------------------------------
3.4  Introns

This problem is taken from [7], and is designed to explore the evolution of a flying 
creature. Various combinations of genes, labeled A to F, are rewarded cumulatively, 
the highest fitness being awarded to a unique combination equated to the power of flight.
Each gene is represented as six positions in the bit string. When all six positions 
are equal to 1, the individual is said to possess the corresponding gene. All possible 
scores are added (see fig. 2), to get the value of the fitness function. herefore, 
the maximum value the fitness function can take is 1156.
   The length of the chromosome is 58. Each gene is represented by six positions, so 
thirty bits are used to represent the five genes. he remaining bits are irrelevant to 
the calculation of fitness but are there to emulate introns in DNA. The population sizes 
used are 16, 64, 256, and 1024.
   The genetic operators used are bitwise mutation (0.003 probability) and onepoint 
crossover (0.5 probability). The selection operator is roulette wheel. Combine is steady 
state. These functions were already defined in the library, so most of the work for this 
problem, defining the chromosome and the fitness function, was done in ChrmUser.

4 Analysis of the Inverted Pendulum Problem

A major goal in constructing our framework is to allow the analysis of GA performance. 
One feature of this is the random number monad Prob mentioned earlier. Monads allow the 
incorporation of imperative features into a purely functional language [8]. 
Prob maintains several infinite lists of random numbers associated to each stochastic 
process, while keeping the liss hidden fom he us his allows vey fine control of 
factors affecting the outcome of a GA run.
   We conducted experiments into how the inverted pendulum GA fared as we varied its 
operating conditions. As an example of what is possible, Figures 3(a) and 3(b) each 
show the results of five GA runs (mean and best fitness at each generation). The seeds 
for the random numbers controlling the generation of the initial population are the 
same for all runs, but the remaining seeds are different Therefore, all runs start 
with the same initial population but vary afterwards. The differences between the runs 
show the effect of a balanced or a random starting position on the performance of the 
inverted pendulum GA. Figures 3(c) and 3(d) show runs using the same seeds but with 
the crossover probability set to zero, i.e., using mutation only. The results suggest 
that mutation acting alone is a more successful configuration than using crossover 
and mutation together

5 Conclusion

A framework has been designed for the construction of genetic algorithms. Four very 
different GAs have been implemented with a high degree of standard module re-use. The 
framework also allows fine control of the stochastic processes underlying the GA. This 
allows the effects of varying different parameters to be explored in isolation. One 
aspect of functional programs remains unmentioned -- the facility with which one can 
reason logically about their properties. The next step in developing our approach will 
be to build a theory of GAs by reasoning about the generic framework. The framework will 
therefore provide a bridge between sound theoretical foundations and practical 
implementation.

-- /////////////////////////////////////////////////////////////////////////////

References

[1] Goldberg, D. E. Genetic algorithms in search, optimization and machine learning. 
    Addison-Wesley, (1989)
[2] Vose, M. D. The Simple Genetic Algorithm: Foundations and Theory. he M Press, Cambridge, 
    Massachusetts, USA, (19)
[3] Peyton Jones, S. and Hughes, J., editors. Haskell 98: a non-strict, purely-functional 
    language, (19) htt//www.hasllonlereport.
[4] Michalewicz, Z. Genetic Algorithms + Data Structures = Evolution Programs. 
    SpringerVerlag, 3rd edition, (16)
[5] Whitley, D. The genitor algorithm and selection pressure: why rank-based allocation of 
    reproductive trials is best. In 3rd International Conference on Genetic Algorithms, 
    Schaffer, D. J., editor, 116-121. Morgan Kaufmann, USA, (1989)
[6] Whitley, D., Dominic, S., Das, R., and Anderson, C. W. Genetic reinforcement learning 
    for neuro-control problems. Machine Learning (13), 259-284 (13)
[7] Levenick, R. Inserting introns improves genetic algorithm success rate: taking a cue 
    from biology. In ^th International Conference on Genetic Algorithms, Belew, R. K. and 
    Booker, L. Â., editors Morgan Kaufmann, USA, (191)
[8] Hill, J. M. D. and Clarke, K. An introduction to category theory, category theory 
    monads, and their relationship to functional programming. Technical Report 681, 
    Queen Mary and Westfield College, England, August (14)


-- /////////////////////////////////////////////////////////////////////////////

