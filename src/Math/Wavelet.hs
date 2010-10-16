module Math.Wavelet where

import Data.Complex(Complex)
import Data.Function(on)
import qualified Data.Array as A
--import DSP.FastConvolution(fast_conv)
import DSP.Convolution(conv)
import Control.Applicative(liftA2)

--conv = fast_conv

aLength :: (A.Ix i, Num i) => A.Array i e -> i
aLength = (+1) . liftA2 (-) snd fst . A.bounds

ceilDiv :: (Integral a, Integral c) => a -> a -> c
ceilDiv x y = ceiling (((/) `on` fromIntegral) x y)

pickMid :: (Integral c, A.Ix c) => c -> A.Array c e -> A.Array c e
pickMid n x = A.ixmap (0, n-1) (+k) x
    where k = (aLength x - n) `ceilDiv` 2

convMid :: (A.Ix i, Integral i, Num e) => A.Array i e -> A.Array i e -> A.Array i e
convMid x = pickMid n . conv x
    where n = aLength x

downsample :: (A.Ix i, Integral i) => i -> A.Array i e1 -> A.Array i e1
downsample k x = A.ixmap (0, (n `ceilDiv` k) - 1) (*k) x
    where n = aLength x

dwt :: (RealFloat b) => A.Array Int (Complex b) 
    -> A.Array Int (Complex b) 
    -> A.Array Int (Complex b)
    -> (A.Array Int (Complex b), A.Array Int (Complex b))
dwt x = (,) `on` (downsample 2 . convMid x)

dwtMaxLevel :: (RealFrac a, Floating a, Integral b) => a -> a -> b
dwtMaxLevel inputLen filterLen | inputLen < 1 || filterLen < 2 = 0
dwtMaxLevel inputLen filterLen = max level 0
  where level = floor . logBase 2.0 $ inputLen / (filterLen - 1)
        
tupleToList :: (t, t) -> [t]
tupleToList (a,b) = [a,b]

waveDec :: (RealFloat b, Num a, Ord a) =>
           a -> A.Array Int (Complex b)
           -> A.Array Int (Complex b)
           -> A.Array Int (Complex b)
           -> [A.Array Int (Complex b)]
waveDec n x lo_d hi_d | n == 1 = tupleToList $ dwt x hi_d lo_d
waveDec n x lo_d hi_d | n > 1  = cD : waveDec (n-1) cA lo_d hi_d
  where (cA, cD) = dwt x lo_d hi_d
waveDec _ _ _    _             = undefined        

ar' = A.listArray (0,7::Int) [1,1,1,1,0,0,0,0::Complex Double]
x' = sqrt 2 / 2 :: Complex Double
lo_d' = A.listArray (0,1::Int) [x',x']
hi_d' = A.listArray (0,1::Int) [-x',x']
pList [] = return ()
pList (x:xs) = print x >> pList xs
  
