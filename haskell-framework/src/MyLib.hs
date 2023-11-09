module MyLib where

import Foreign.C

hs_double :: CInt -> CInt
hs_double x = 2 * x

