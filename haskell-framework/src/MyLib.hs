module MyLib where

import Foreign.C

hs_factorial :: CInt -> CInt
hs_factorial x = product [1..x]

