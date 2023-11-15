{-# LANGUAGE OverloadedStrings #-}
module MyLib where

import Foreign.C

hs_factorial :: CInt -> CInt
hs_factorial x = product [1..x]

-------

data User = User { birthYear :: {-# UNPACK #-} !CLong
                 , age       :: {-# UNPACK #-} !CLong
                 }

mkSimpleUser :: CLong -- ^ Age
             -> User
mkSimpleUser x = User { birthYear = (2023 - x), age = x }

