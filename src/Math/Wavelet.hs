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

pickMid n x = A.ixmap (0, n-1) (+k) x
    where k = (aLength x - n) `ceilDiv` 2

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


