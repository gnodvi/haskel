--------------------------------------------------------------------------------

> module RS_IntExts (leastPowerOf2)
> where

> import CTypes

-- > foreign import ccall "mso.h mso" cmso :: CUInt -> CUInt

> foreign import ccall "RS_mso" cmso :: CUInt -> CUInt
>
> leastPowerOf2                 :: Int -> Int
> leastPowerOf2 n               =  fromIntegral (cmso (fromIntegral n)) 

--------------------------------------------------------------------------------
