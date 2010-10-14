module Math.Wavelet where

import Data.Complex(Complex)
import Data.Function(on)
import qualified Data.Array as A
import DSP.FastConvolution(fast_conv)
import Control.Applicative(liftA2)

aLength :: (A.Ix i, Num i) => A.Array i e -> i
aLength = liftA2 (-) snd fst . A.bounds

downsample :: (A.Ix i, Integral i) => i -> A.Array i e1 -> A.Array i e1
downsample k x = A.ixmap (0, n `div` k) (*k) x
    where n = aLength x

dwt :: (RealFloat b) => A.Array Int (Complex b) 
    -> A.Array Int (Complex b) 
    -> A.Array Int (Complex b)
    -> (A.Array Int (Complex b), A.Array Int (Complex b))
dwt x = (,) `on` (downsample 2 . fast_conv x)
