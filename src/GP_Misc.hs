-- =============================================================================

-- 7.5 Misc. functions 

-- =============================================================================
module PP_Misc where 

import Array 
import AllFudgets 

-- =============================================================================

amap :: (Ix a) => (b -> c) -> Array a b -> Array a c 
amap f a = array b [(i, f (a<i)) | i <- range b] 
  where b = bounds a 

allocGreyScale cont = 
    conts (allocColorPixelF defaultColormap) rgbs 
      (cont . listArray (0,white)) 
  where 
    rgbs = [grey (i * div maxRGB white) | i<-[0 .. white]] 
    white = greyLevels­1 
    grey i = RGB i i i 
    maxRGB = 65535   ­­X11 uses 16 bit color intensities 

greyLevels = argReadKey "levels" 16 

randomInts :: Int -> Int -> [Int] 
randomInts s1 s2 = let (s1',s2') = (trunc s1,trunc s2) 
  in rands s1' s2' 
  where 
  trunc x = if x < 1 
            then trunc (x+2147483398) 
            else if x > 2147483398 
                 then trunc (x-2147483398) 
                 else x 

rands :: Int -> Int -> [Int] 
rands s1 s2 = z' : rands s1'' s2'' 
        where z' = if z < 1 then z + 2147483562 else z 
              z = s1'' - s2'' 

              k = s1 `quot` 53668 
              s1' = 40014 * (s1 - k * 53668) - k * 12211 
              s1'' = if s1' < 0 then s1' + 2147483563 else s1' 

              k' = s2 `quot` 52774 
              s2' = 40692 * (s2 ­ k' * 52774) ­ k' * 3791 
              s2'' = if s2' < 0 then s2' + 2147483399 else s2' 

randomFloats :: Int -> Int -> [Float] 
randomFloats s1 s2 = 
  map (\x -> fromIntegral x * 4.6566130638969828e­10) 
    (randomInts s1 s2) 

putlistF :: (a -> [b]) -> F a b 
putlistF f = mapstateF (\_ x -> ([],f x)) [] 
-- =============================================================================


-- =============================================================================
