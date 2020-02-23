-- =============================================================================
-- 7.2 The Gene class 

-- =============================================================================
module PP_Class where 

import PP_Misc 
import PP_Expr 
import PP_Plan 
import PP_Polyps 

import Array 
import AllFudgets 

-- =============================================================================

class GeneObj a where 
  value   :: a -> String 
  setval  :: a -> String -> a 
  fitness :: a -> Float 
  love    :: a -> a -> Float -> a 
  mutant  :: a -> Float -> a 
  mkGene  :: a -> [Int] -> (a,[Int]) 
  picsize :: a -> Point 
  toGfx   :: a -> (Array Int Pixel) -> [DrawCommand] 
  flat    :: a -> [Float] 

data Gene1 = Gene1 Int Int 
  deriving (Show,Read) 
                                      
instance GeneObj (Gene1) where 
  value g = show g 
  setval g s = read s 
  fitness (Gene1 x _) = fromInt x / 100 
  love (Gene1 x x') (Gene1 y y') rnd = 
    if rnd < 0.5 then (Gene1 x y') else (Gene1 x' y) 
  mutant (Gene1 x y) rnd = Gene1 y x 
  mkGene _ (x:y:xs) = (Gene1 (mod x 100) (mod y 100),xs) 
  picsize _ = Point 100 20 
  toGfx (Gene1 x y) _ = [DrawString (Point 10 10) (show (x,y))] 
  flat (Gene1 x y) = [fromInt x / 100,fromInt y / 100] 

data GPic = GPic (Expr Float) 
  deriving (Show,Read) 

newrnd rnd = (sin (rnd*100)) / 2 + 0.5 

instance GeneObj (GPic) where 
  value g = show g 
  setval _ s = read s 
  fitness (GPic e) = fromInt (size_Expr e) / 1000 
  love (GPic e) (GPic e') rnd = 
    GPic (rep repl_Expr size_Expr rnd (newrnd rnd) e e') 
  mutant (GPic e) rnd = 
    let s = round (rnd*999999) 
        (e',_) = mkExpr (randomInts s (s*2)) 
    in GPic (rep repl_Expr size_Expr rnd 0 e e') 
  mkGene _ xs = let (e,ss) = mkExpr xs 
    in (GPic e,ss) 
  toGfx (GPic e) grey = 
    [CreatePutImage (Rect origin (Point 100 100)) zPixmap pix] 
    where 
    cut x = if x < 0 then 0 else if x > 0.96 then 0.96 else x 
    pix = [grey<pixels x y | x <­ [0 .. 99], y <­ [0 .. 99]] 
    pixels x y = 
      let p = xyeval e ((fromInt x)/50­1) ((fromInt y)/50­1) 
      in truncate 
         ((if p<0 then 0 else if p>0.95 then 0.95 else p)* 
           fromInt greyLevels) 
  flat _ = [] 
  picsize _ = Point 100 100 

data GPlan = GPlan (Plan Int) (Int,([(Int,Int)],[(Int,Int)])) 
  deriving (Read,Show) 

instance GeneObj (GPlan) where 
  value (GPlan p _) = show p 
  setval _ s = let np = read s 
    in GPlan np (pGfx np) 
  fitness (GPlan p (sc,_)) = fromInt sc / 100 
  love (GPlan p _) (GPlan p' _) rnd = 
    let np = rep repl_Plan size_Plan rnd (newrnd rnd) p p' 
    in GPlan np (pGfx np) 
  mutant (GPlan p _) rnd = 
    let s = round (rnd*999999) 
        (p',_) = mkPlan (randomInts s (s*2)) 
        np = rep repl_Plan size_Plan rnd 0 p p' 
    in GPlan np (pGfx np) 
  mkGene _ xs = let (p,ss) = mkPlan xs 
    in (GPlan p (pGfx p),ss) 
  toGfx (GPlan _ (_,(es,fs))) grey = 
    [CreatePutImage (Rect origin (Point 64 64)) zPixmap pix] 
    where 
    gl = div greyLevels 2 
    pix0 = (grey<0) 
    pix1 = (grey<gl) 
    pix2 = (grey<(gl+gl­1)) 
    pix = pixels [(x,div y 2) | 
                   y <­ [0 .. 63], x <­ [0 .. 31]] es fs 
    pixels [] _ _ = [] 
    pixels (x:xs) es fs = 
      if es/=[] && x==head es 
      then pix2:pix2: pixels xs (tail es) 
           (if fs/=[] && x==head fs then tail fs else fs) 
      else if fs/=[] && x==head fs 
           then pix1:pix1: pixels xs es (tail fs) 
           else pix0:pix0: pixels xs es fs 
  flat _ = [] 
  picsize _ = Point 64 64 

data Insect = Insect [Int] 
  deriving (Read,Show) 

instance GeneObj (Insect) where 
  value g = show g 
  setval _ s = read s 
  fitness _ = 0 
  love (Insect i) (Insect i') rnd = 
    Insect (rep repl_Ins size_Ins rnd rnd i i') 
  mutant gi@(Insect i) rnd = 
    let s = round (rnd*999999) 
        (Insect i') = fst (mkGene gi (randomInts s (s*2))) 
    in Insect (if rnd < 0.5 
               then rep repl_Ins size_Ins rnd rnd i i' 
               else rep repl_Ins size_Ins rnd rnd i' i) 
  mkGene _ xs = 
    (Insect (map (\x -> (mod x 9) - 4) (take 9 xs)),drop 9 xs) 
  toGfx (Insect gs) _ = 
    lcenter (map DrawLine (mktree 50 50 2 ((gs<<8) + 5))) 
    where 
    size = 3 
    dx = listArray (0,7) 
      [-gs<<1,-gs<<0,0,gs<<0,gs<<1,gs<<2,0,-gs<<2] 
    dy = listArray (0,7) 
      [gs<<5,gs<<4,gs<<3,gs<<4,gs<<5,gs<<6,gs<<7,gs<<6] 
    mktree x y dir len = 
      let ndir = mod dir 8 
          xnew = x + div (len * (dx<ndir) * size + 2) 4 
          ynew = y + div (len * (dy<ndir) * size + 2) 4 
      in if len == 0 then [] else lL x y xnew ynew : 
            mktree xnew ynew (ndir­1) (len­1) ++ 
            mktree xnew ynew (ndir+1) (len­1) 
  flat (Insect i) = map (((*) 0.1) . fromInt) i 
  picsize _ = Point 100 100 

lcenter :: [DrawCommand] -> [DrawCommand] 
lcenter ds = 
  map (flip moveDrawCommand 
       (Point 0 (getcenter (getedges ds (50,50))))) ds 
  where 
  getcenter (y1,y2) = (div (y1+100­y2) 2) ­ y1 
  getedges :: [DrawCommand] -> (Int,Int) -> (Int,Int) 
  getedges [] (y1,y2) = (y1,y2) 
  getedges (DrawLine (Line (Point _ y1) (Point _ y2)):ds) 
    (y1',y2') = getedges ds (min y1 y1',max y2 y2') 
  getedges (_:ds) yp = getedges ds yp 

data Population = (GeneObj ?a) => Pop [?a] 

mkGenes :: Population -> Int -> Int -> Population 
mkGenes (Pop (g1:_)) nr rnd = 
  mkGenes' nr (randomInts rnd (rnd*2)) [] 
  where 
  mkGenes' 0 _ gs = Pop gs 
  mkGenes' n rs gs = let (g2,rs') = mkGene g1 rs 
    in mkGenes' (n­1) rs' (g2:gs) 

g0 = Pop [Gene1 0 0] 
g1 = Pop [GPic (Var 'X')] 
g2 = Pop [GPlan Move (0,([],[]))] 
g3 = Pop [Insect [0 .. 8]] 

genes :: [Population] 
genes = [g0,g1,g2,g3] 

-- =============================================================================
