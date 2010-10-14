module Wavelet where


import Data.Array
import DSP.FastConvolution(fast_conv)
import qualified DSP.Basic as DSPB


dwt x lo_d hi_d = (cA, cD)
    where cA = downsample 2 . fast_conv lo_d $ x
          cD = downsample 2 . fast_conv hi_d $ x
