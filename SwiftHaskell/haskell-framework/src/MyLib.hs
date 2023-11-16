{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
module MyLib where

import Data.Aeson
import GHC.Generics
import Foreign.C
import Foreign.Storable.Generic

hs_factorial :: CInt -> CInt
hs_factorial x = product [1..x]

-------

data User = User { birthYear :: {-# UNPACK #-} !CLong
                 , age       :: {-# UNPACK #-} !CLong
                 }
                 deriving stock    Generic
                 deriving anyclass GStorable


data User2 = User2
              { birthYear :: {-# UNPACK #-} !Int
              , age       :: {-# UNPACK #-} !Int
              }
              deriving stock    Generic
              deriving anyclass FromJSON
              deriving anyclass ToJSON

mkSimpleUser :: CLong -- ^ Age
             -> User
mkSimpleUser x = User { birthYear = (2023 - x), age = x }

