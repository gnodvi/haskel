-- =============================================================================

-- 7.3 The Picture example 

-- =============================================================================

module PP_Expr where 

import PP_Polyps 

-- =============================================================================

mkVar x = if odd x then 'X' else 'Y'
 
mkbOp :: Int -> Char 
mkbOp x = case mod x 5 of 
  0 -> '*' 
  1 -> '/' 
  2 -> '+' 
  3 -> '­' 
  4 -> 'a' 

mkuOp x = case mod x 2 of 
  0 -> 's' 
  1 -> 'r' 

getbOp :: Char -> (Float -> Float -> Float) 
getbOp o = case o of 
  '*' -> (*) 
  '/' -> (\ x y -> if y==0 then x/0.001 else x/y) 
  '+' -> (+) 
  '­' -> (-) 
  'a' -> (\ x y -> if x==0 && y==0 then 0 else atan2 y x) 

getuOp o = case o of 
  's' -> sin 
  'r' -> sqrt . abs 

mkExpr :: [Int] -> (Expr Float,[Int]) 
mkExpr (s':s:ss) = case mod s' 5 of 
  0 -> let r = fromInt s * 4.6566130638969828e­10 
       in (Num (r*2­1),ss) 
  1 -> (Var 'Y',ss) 
  2 -> (Var 'X',ss) 
  3 -> let (e1,ss1) = mkExpr ss 
       in (Uop (mkuOp s) e1,ss1) 
  4 -> let (e1,ss1) = mkExpr ss 
           (e2,ss2) = mkExpr ss1 
       in (Bop (mkbOp s) e1 e2,ss2) 
  5 -> let (e1,ss1) = mkExpr ss 
           (e2,ss2) = mkExpr ss1 
           (e3,ss3) = mkExpr ss2 
       in (Iflte e1 e2 e3,ss3) 

xyeval :: Expr Float -> Float -> Float -> Float 
xyeval (Num f) _ _ = f 
xyeval (Var 'X') x _ = x 
xyeval (Var 'Y') _ y = y 
xyeval (Bop o e1 e2) x y = 
  (getbOp o) (xyeval e1 x y) (xyeval e2 x y) 
xyeval (Uop o e1) x y = (getuOp o) (xyeval e1 x y) 
xyeval (Iflte e1 e2 e3) x y = if (xyeval e1 x y) <= 0 
                              then (xyeval e2 x y) 
                              else (xyeval e3 x y) 

-- =============================================================================
