-- =============================================================================

-- 7.7 Neural network, one layer 

-- =============================================================================

module PP_Net1 where 

import PP_Misc 

-- =============================================================================

data NState = NState [Float] 
  deriving Show 

data NInp = NInput [[Float]] 
          | NLearn [Float] Float 
          | NLearns [[Float]] [Float] 
     deriving (Show) 

type NOut = [Float] 
type S a = NState -> (a,NState) 

sS = NState (replicate 4 0.1) 

rate :: Float 
rate = 1 

bias :: Float 
bias = 1 

c :: Float 
c = 6 

unitS :: a -> S a 
unitS a = \s -> (a,s) 

m `bindS` k = \s -> let (a,s1) = m s 
                        (b,s2) = k a s1 
                    in (b,s2) 

sigm :: Float -> Float 
sigm x = 1 / (1 + exp (-c*x)) 
sigm' :: Float -> Float 
sigm' y = ((1/y - 1)**(1/c)) * y * y 

scalp :: [Float] -> [Float] -> Float 
scalp [] _ = 0 
scalp _ [] = 0 
scalp (x:xs) (y:ys) = x*y + scalp xs ys 

errorS :: [Float] -> Float -> S [Float] 
errorS is c (NState ws) = ([fx],NState (newweights is ws)) 
  where 
  fx = sigm (scalp is ws) 
  dx = rate * (c­fx) * sigm' fx 
  newweights _ [] = [] 
  newweights (i:is') (w:ws') = w + dx * i : newweights is' ws' 

errorsS :: [[Float]] -> [Float] -> S () 
errorsS iss cs (NState ws) = ((),NState (zipWith radd ws dws)) 
  where 
  radd x y = x + rate * y / (fromInt (length iss)) 
  dws = addw iss cs (replicate (length ws) 0) 
  addw [] [] dw = dw 
  addw (is:iss') (c:cs') dw = 
    addw iss' cs' (accweights is (sigm $ scalp is ws) c dw) 

accweights is fx c dws = zipWith (\i w -> w + dx*i) is dws 
  where 
  dx = (c-fx) * sigm' fx 

outputS :: [[Float]] -> S [Float] 
outputS iss s@(NState ws) = 
  (map (\is -> sigm (scalp (bias:is) ws)) iss,s) 

neteval :: NInp -> S [Float] 
neteval (NInput iss) = outputS iss 
neteval (NLearn is c) = errorS (bias:is) c 
neteval (NLearns iss cs) = 
  errorsS (map ((:) bias) iss) cs `bindS` \_ ->   
  outputS iss 
                     
-- =============================================================================
