-- =============================================================================

-- 7.6 Polytypic functions 

-- Note: This is not Haskell code. The function definitions must be parsed by polyp and then 
-- concatenated with the type definitions before compilation. 

-- =============================================================================
module PP_Polyps where 

-- =============================================================================

add :: Int -> Int -> Int 

rep :: (Int -> Int -> a -> a -> a) -> (a -> Int) -> 
       Float -> Float -> a -> a -> a 
rep r s m n a b = let x = (truncate . ((*) m) . fromInt . s) a 
                      y = (truncate . ((*) n) . fromInt . s) b 
                  in r x y a b 

repl_Expr = replace_f4Expr 
size_Expr = psize_f4Expr 
repl_Plan = replace_f4Plan 
size_Plan = psize_f4Plan 
repl_Ins = replace_f0 
size_Ins = psize_f0 

data Plan a 
     = Iffood (Plan a) (Plan a) 
     | Dummy (a) 
     | Move 
     | TurnLeft 
     | TurnRight 
     | Progn2 (Plan a) (Plan a) 
  deriving (Show,Read) 

data Expr a 
     = Bop (Char) (Expr a) (Expr a) 
     | Uop (Char) (Expr a) 
     | Iflte (Expr a) (Expr a) (Expr a) 
     | Var (Char) 
     | Num (a) 
  deriving (Show,Read) 

polytypic fsize :: f a Int -> Int 
  = case f of 
      Const t -> \x -> inc 0 
      Empty -> \x -> inc 0 
      Par -> \x -> inc 0 
      Rec -> \x -> x 
      f + g -> fsize `junc` fsize 
      f * g -> \(x,y) -> (fsize x) `add` (fsize y) 

psize = cata fsize 

--para :: (d a -> f a b -> b) -> d a -> b 
para i x = i x (fmap id (para i) (out x)) 

polytypic fchildren :: (f a [b] -> [b]) 
  = case f of 
      Const t -> \x -> [] 
      Empty -> \x -> [] 
      Par -> \x -> [] 
      Rec -> \x -> x 
      f * g -> \(x,y) -> fchildren x ++ fchildren y 
      f + g -> fchildren `junc` fchildren 

--scan :: d a -> [d a] 
scan = para (\x -> \y -> x : fchildren y) 
concatt xss = case xss of 
                [] -> [] 
                (xs:xss) -> xs ++ concatt xss 

replace x1 x2 p1 p2 = fst (build ((take x1 . scan) p1 ++ 
                           (scan . head . drop x2 . scan) p2 ++ 
                           (drop (x1+1) . scan) p1)) 

build = \(x:xs) -> let a = case xs of 
                             [] -> fbuild [] (out x) 
                             xs -> let b = build xs 
                               in fbuild (fst b : (snd b)) (out x) 
                   in (inn (fst a),snd a) 

inL x = (Left (fst x),snd x) 
inR x = (Right (fst x),snd x) 

polytypic fbuild :: [b] -> f a b -> (f a b,[b]) 
  = \bs -> case f of 
      Const t -> \x -> (x,bs) 
      Empty -> \x -> ((),bs) 
      Par -> \x -> (x,bs) 
      Rec -> \x -> (head bs,tail bs) 
      f * g -> \(x,y) -> let f1 = fbuild bs x 
                             f2 = fbuild (snd f1) y 
                         in ((fst f1,fst f2),snd f2) 
      f + g -> \x -> case x of 
                       Left x -> inL (fbuild bs x) 
                       Right x -> inR (fbuild bs x) 

-- =============================================================================


-- =============================================================================
