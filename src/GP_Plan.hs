-- =============================================================================
-- 7.4 The Planning example 

module PP_Plan where 
-- =============================================================================

import Array 

import PP_Polyps 

-- =============================================================================

data Pos = Pos Int Int 
  deriving (Eq) 

type Dir = Int 

data FState = FState Int Pos Dir [(Int,Int)] Int 
data PState = PState Int Pos Dir [(Int,Int)] [(Int,Int)] Int 

mkPlan :: [Int] -> (Plan a,[Int]) 
mkPlan ss = fst (mkPlan' ss 0) 
  where 
  mkPlan' (s:ss) x = case (if x>5 then mod s 3 else mod s 5) of 
    0 -> ((Move,ss),x+1) 
    1 -> ((TurnLeft,ss),x+1) 
    2 -> ((TurnRight,ss),x+1) 
    3 -> let ((p1,ss1),x1) = mkPlan' ss (x+1) 
             ((p2,ss2),x2) = mkPlan' ss1 x1 
         in ((Progn2 p1 p2,ss2),x2) 
    4 -> let ((p1,ss1),x1) = mkPlan' ss (x+1) 
             ((p2,ss2),x2) = mkPlan' ss1 x1 
         in ((Iffood p1 p2,ss2),x2) 

move (Pos x y) d = case d of 
  0 -> Pos x (mod (y­1) 32) 
  1 -> Pos (mod (x+1) 32) y 
  2 -> Pos x (mod (y+1) 32) 
  3 -> Pos (mod (x­1) 32) y 

foodList = 
  [(1,0), (2,0), (3,0), (3,1), (3,2), (25,2), (26,2), (27,2), 
   (3,3), (24,3), (29,3), (3,4), (24,4), (29,4), (3,5), (4,5), 
   (5,5), (6,5), (8,5), (9,5), (10,5), (11,5), (12,5), (21,5), 
   (22,5), (12,6), (29,6), (12,7), (20,7), (12,8), (20,8), (12,9), 
   (20,9), (29,9), (20,10), (12,11), (12,12), (29,12), (12,13), 
   (20,13), (12,14), (20,14), (26,14), (27,14), (28,14), (17,15), 
   (23,15), (12,17), (12,18), (16,18), (24,18), (12,19), (16,19), 
   (27,19), (12,20), (16,20), (12,21), (16,21), (12,22), (12,23), 
   (26,23), (3,24), (4,24), (7,24), (8,24), (9,24), (10,24), 
   (11,24), (16,24), (23,24), (1,25), (16,25), (1,26), (16,26), 
   (1,27), (8,27), (9,27), (10,27), (11,27), (12,27), (13,27), 
   (14,27), (1,28), (7,28), (7,29), (2,30), (3,30), (4,30), (5,30)] 

foodArray = 
  listArray (0,1023) [if elem (x,y) foodList 
                      then 1 
                      else 0 | y <- [0..31], x <- [0..31]] 

food' :: Pos -> [(Int,Int)] -> Bool 
food' (Pos x y) fa = elem (x,y) fa 

eat :: Pos -> [(Int,Int)] -> (Bool,[(Int,Int)]) 
eat _ [] = (False,[]) 
eat p@(Pos x y) ((f@(fx,fy)):fs) = if x==fx && y==fy 
                                   then (True,fs) 
                                   else let (b,fs') = eat p fs 
                                        in (b,f:fs') 

pGfx :: Plan a -> (Int,([(Int,Int)],[(Int,Int)])) 
pGfx plan = (sc,(vs,foodList)) 
  where 
  (PState sc _ _ _ vs _) = 
    pFit' (PState 0 (Pos 0 0) 1 foodList [] 200) 
  pFit' s@(PState _ _ _ _ _ t) = if t <= 0 
                                 then s 
                                 else pFit' (pFit'' s plan) 
pFit'' s@(PState sc p d ps vs t) pl = if t <= 0 then s else 
  case pl of 
    Iffood p1 p2 -> pFit'' s (if food' (move p d) ps 
                              then p1 
                              else p2) 
    Progn2 p1 p2 -> let s' = pFit'' s p1 
                    in pFit'' s' p2 
    Move         -> let p'@(Pos x y) = move p d 
                        (b,ps') = eat p' ps 
                    in (PState (if b 
                                then sc+1 
                                else sc) 
                        p' d ps' (ins (x,y) vs) (t­1)) 
    TurnLeft     -> PState sc p (mod (d­1) 4) ps vs (t­1) 
    TurnRight    -> PState sc p (mod (d+1) 4) ps vs (t­1) 
ins p [] = [p] 
ins p@(x,y) xs'@((x',y'):xs) = if x==x' && y==y' then xs' else 
                               if y<y' || (y==y' && x<=x') 
                               then (x,y):xs' 
                               else (x',y'):ins p xs 

-- =============================================================================

