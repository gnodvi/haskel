-- =============================================================================
-- 7.1 The Main program


module PP_Main where

-- =============================================================================

import AllFudgets 

import PP_Class 
import PP_Misc 

import Array 
import Net1 
import Time 

import PP_Expr 

-- =============================================================================

type Grey = Array Int Pixel 
type Fits = Array Int Float 
data State = State [Int] Fits NState Population Mating Choose 

data GOutput = GoShow Int Population 
             | GoFit Int Float 
             | GoDisp Bool 
             | GoSize Int 
             | GoMax (Float,Float) 
             | GoPoint Point 
             | GoPrint String 

data GFitn = GFitn Int Float 
data Mating = Mate 
             | Mutant 
             | Both 
     deriving Eq 

data Choose = Function 
             | User 
             | Net 
             | Learn 
     deriving Eq 

data Method = Method Mating Choose 
data GInput = GiNext Int 
             | GiMeth Mating 
             | GiChos Choose 
             | GiNew Population 
             | GiDisp Bool 
             | GiSize Int 
             | GiPrint 
             | GiLoad Population 

-- main = do 
pp_main = do 
        t <ญ getClockTime 
        fudlogue (shellF "Genetics" 
                   (mainF' (getSeed t) >*< 
                    (quitButtonF >==< nullF))) 
  where getSeed = fromInteger . ctPicosec . toUTCTime 

mainF' s = loopLeftF 
           (dispPopupF 9 >==< 
           mapstateF eval (startstate 9) >==< 
           idF >+< buttonsF 
           ) 
  where 
  startstate x = 
    State (randomInts s s) (listArray (1,x) [0,0..]) 
           nstate (mkGenes g0 x s) Mutant User 
  nstate = NState (replicate 3 0) ญญ(length (flat g0)+1) 

parse1 :: String -> [String] 
parse1 s = let (ns,os) = head (reads s) in 
           if os=="" then ns else parse1 os 

readGenesF :: F (Either Click GInput) GInput 
readGenesF = mapstateF readG (head genes) >==< 
             ((filterRightSP >^^=< 
               (nullF >+< mapF parse1) >==< 
               snd >^=< readFileF >==< 
               snd >^=< stringPopupF "" >=^< 
               (\Click -> (Just "Filename:",Nothing)) 
              ) >+< idF 
             ) 
  where 
  readG :: Population -> (Either [String] GInput) -> 
           (Population,[GInput]) 
  readG g@(Pop (g':_)) (Left es) = 
    (g,[GiLoad (Pop (map (setval g') es))]) 
  readG _ (Right (GiNew g)) = (g,[]) 
  readG g _ = (g,[]) 

printF :: F String a 

printF = 
  dynF nullF >==< 
  mapstateF writeG ("","") >==< 
  (snd >^=< stringPopupF "") >+< idF >==< 
  putlistF (\s -> [Left (Just "Filename:",Nothing),Right s]) 
  where 
  writeG (n,_) (Right s) = 
    if n /= "" 
    then (("",""),[Right s]) 
    else ((n,s),[]) 
  writeG (_,s) (Left n) = 
    if s /= "" 
    then ((n,s),[Left (outputF n),Right s]) 
    else ((n,s),[Left (outputF n)]) 

buttonsF :: F GInput GInput 
buttonsF = loopF $ placerF (revP verticalP) $ 
  (GiDisp >^=< startupF [False] 
    (spacerF leftS (toggleButtonF "Show")) >==< nullF) >*< 
  (GiSize >^=< inputDoneSP >^^=< 
    ("Size of population: " `labLeftOfF` intF) >==< nullF) >*< 
  (GiNext >^=< inputDoneSP >^^=< 
    ("Run x generations: " `labLeftOfF` intF) >==< nullF) >*< 
  placerF horizontalP 
    ((const GiPrint >^=< buttonF "Save" >==< nullF) >*< 
     (readGenesF >==< ((buttonF "Load" >==< nullF) >+< idF) >=^< 
      Right)) >*< 
  graphicsLabelF (hFiller 2) >*< 
  (untaggedListLF horizontalP 
  [(\_ -> GiNext 1) >^=< buttonF "Next" >==< nullF, 
   GiMeth >^=< radioGroupF 
     [(Mate,"Mate"),(Mutant,"Mutant"),(Both,"Both")] 
       Mutant >==< nullF, 
   GiChos >^=< radioGroupF 
     [(Function,"Fun"),(User,"User")] User >==< nullF, 
   GiNew . head . flip drop genes >^=< radioGroupF 
     [(1,"Picture"),(2,"Planning"),(3,"Tree")] 1 >==< nullF 
  ] >==< nullF) 
    >=^^< filterSP (\x -> case x of 
                          GiNew _ -> True 
                          _ -> False) 
--                          _ -> False}) 

displayGeneF :: Grey -> Point -> 
  F (Either Float Population) Float 
displayGeneF grey s = 
  filterLeftSP >^^=< placerF (revP verticalP) 
    ((scF >==< dF) >+< gF) 

  where 
  scF = (\(x,_,_) -> (fromInt x)/100.0) >^=< 
     startupF [ResizePot 10 110] (hPotF Nothing) >=^< 
     (MovePot . fromInt . round . ((*) 100.0)) 
  gF = nullF >==< showF grey s 
  dF = (displayF >=^< show) >*< idF 

showF :: Grey -> Point -> F Population GfxOutput 
showF grey s = 
  spacerF (sizeS s) (graphicsDispF >=^< 
          (\(Pop (g:_)) -> replaceAllGfx 
            (atomicD (FD (picsize g) (toGfx g grey)))) 

displayerF :: Int -> Point -> 
  F (Int,Either Float Population) (Either GFitn b) 
displayerF x s = 
  allocGreyScale $ \grey -> 
  shellF "Showing Phenotypes" $ 
  scrollF $ 
  (\(x,y) -> Left (GFitn x y)) >^=< 
  placerF ((matrixP . round . sqrt . fromInt) x) 
  (listF $ zip [1..x] (replicate x (displayGeneF grey s))) 

dispPopupF :: Int -> F GOutput (Either GFitn a) 
dispPopupF x = dynF nullF >==< 
               filterRightSP >^^=< 
               ((maxavF >+< printF) >+< 
                mapstateF dyneval (False,x,Point 0 0)) >=^< 
               (\x ญ> case x of 
                 GoMax (m,a) -> Left (Left (m,a)) 
                 GoPrint s -> Left (Right s) 
                 y -> Right y 
               ) 
  where 
  dyneval (_,i,p) (GoDisp b) = 
    ((b,i,p),[Left (if b then displayerF i p else nullF)]) 
  dyneval (b,_,p) (GoSize i) = 
    ((b,i,p),if b then [Left (displayerF i p)] else []) 
  dyneval (b,i,_) (GoPoint p) = 
    ((b,i,p),if b then [Left (displayerF i p)] else []) 
  dyneval (False,i,p) _ = ((False,i,p),[]) 
  dyneval b (GoShow i g) = (b,[Right (i,Right g)]) 
  dyneval b (GoFit i f) = (b,[Right (i,Left f)]) 
  maxavF = placerF verticalP 
    (labLeftOfF "Max fitness:" (displayF >=^< (show . fst)) >*< 
     labLeftOfF "Average fitness: " (displayF >=^< (show . snd))) 

sr1 (State _ fa ns gs m c) ss = State ss fa ns gs m c 
sr2 (State ss _ ns gs m c) fa = State ss fa ns gs m c 
sr3 (State ss fa _ gs m c) ns = State ss fa ns gs m c 
sr4 (State ss fa ns _ m c) gs = State ss fa ns gs m c 
sr5 (State ss fa ns gs _ c) m = State ss fa ns gs m c 
sr6 (State ss fa ns gs m _) c = State ss fa ns gs m c 

gosize :: Population -> Point 
gosize (Pop ps) = picsize (head ps) 

eval :: State -> Either GFitn GInput -> (State,[GOutput]) 
eval s@(State ss fa ns gs@(Pop gs') m c) x = 
  (case x of 
    Left (GFitn i f) ญ> let nfa = fa//[(i,f)] 
      in (sr2 s nfa,[GoFit i f]) 
    Right GiPrint -> (s,[GoPrint (show (map value gs'))]) 
    Right (GiLoad ngs@(Pop ngs'')) -> 
      let nfa = newfit ngs c 
          nns = NState 
            (replicate (length (flat (head ngs'')) + 1) 0)} 
      in (sr2 (sr3 (sr4 s ngs) nns) nfa, 
        GoSize (length ngs'') : 
          GoPoint (gosize ngs) : outgen ngs nfa) 
    Right (GiDisp b) -> 
      (s,GoPoint (gosize gs) : GoDisp b : outgen gs fa) 
    Right (GiMeth m') -> (sr5 s m',[]) 
    Right (GiChos c') -> let nfa = newfit gs c' 
      in (sr2 (sr6 s c') nfa,outfit nfa) 
    Right (GiNew g) -> 
      let x = snd (bounds fa) 
          ngs = mkGenes g x (head ss) 
          nfa = newfit ngs c 
          nns = NState (replicate 3 0)} ญญ(length (flat g1) + 1) 
      in (sr1 (sr2 (sr3 (sr4 s ngs) nns) nfa) (tail ss), 
          GoPoint (gosize ngs) : outgen ngs nfa) 
    Right (GiNext x) -> 
      let (nss,ngs,nfa,nns) = next x (ss,gs,fa,ns) 
      in (sr1 (sr2 (sr3 (sr4 s ngs) nns) nfa) nss, 
          outgen ngs nfa) 
      where 
      next 0 sgfn = sgfn 
      next y (s,g@(Pop g'),f,n) = 
        next (yญ1) (tail s,ng,newfit ng c,nn) 
        where 
        ng = mkGeneration (head s) m c f nn g (snd $ bounds f) 
        nn = 
          (case c of 
          Learn -> 
            snd (neteval (NLearns (map flat g') (elems f)) n) 
          _ -> n) 
    Right (GiSize x) -> 
      let ngs = mkGeneration (head ss) m c fa nns gs x 
          nfa = newfit ngs c 
          nns = 
            (case c of 
             Learn -> 
               snd (neteval (NLearns (map flat gs') 
                    (elems fa)) ns) 
               _ -> ns) 
      in (sr1 (sr2 (sr3 (sr4 s ngs) nns) nfa) (tail ss), 
          GoSize x : outgen ngs nfa) 
) 
where 
outfit :: Fits -> [GOutput] 
outfit a = 
 GoMax (maximum (elems a), sum (elems a) / 
        fromInt (snd (bounds a))) : 
        map (\(x,y) -> GoFit x y) (assocs a) 
newfit :: Population ญ> Choose ญ> Fits 
newfit (Pop gs') c' = let l = length gs' in 
  case c' of 
    Function ญ> listArray (1,l) (map fitness gs') 
    User -> listArray (1,l) (replicate l 0) 
    Net -> 
      listArray (1,l) (fst (neteval (NInput (map flat gs')) ns)) 
    Learn ญ> 
      listArray (1,l) (replicate l 0) ญญ(map fitness gs') 
outgen :: Population -> Fits -> [GOutput] 
outgen (Pop gs') a = outfit a ++ 
  zipWith (\x y -> GoShow x (Pop [y])) [1..] gs' 

mkGeneration :: Int -> Mating -> Choose -> Fits -> NState -> 
  Population -> Int -> Population 
mkGeneration seed m c fa ns (Pop gs) nr = 
  mkGen nr (randomFloats seed (seed+1)) [] 
  where 
  fitness' x = let y = fitness x 
    in y*y 
  fits = map (\x -> x*x) (elems fa) 
  gsum = case c of 
           Function -> foldl (\x g -> x + fitness' g) 0 gs 
           _ -> sum fits 
  mkGen 0 _ ms = Pop ms 
  mkGen x (r1:r2:r3:r4:rs) ms = mkGen (xญ1) rs (mate:ms) 
    where 
    mate = 
      case m of 
      Mate -> 
        love (getG (r1*gsum) gs fits) (getG (r2*gsum) gs fits) r3 
      Mutant -> mutant (getG (r1*gsum) gs fits) r2 
      Both -> 
        mutant (love (getG (r1*gsum) gs fits) (getG (r2*gsum) 
                gs fits) r3) r4 
    getG y gs' fs = case c of 
      Function -> getGf y gs' 
      _ -> getG_ y gs' fs 
    getGf _ (g':[]) = g' 
    getGf y (g':gs') = if y < fitness' g' 
                       then g' 
                       else getGf (y ญ fitness' g') gs' 
    getG_ _ (g':[]) _ = g' 
    getG_ y (g':gs') (f:fs) = if y < f 
                              then g' 
                              else getG_ (y - f) gs' fs 

-- =============================================================================
