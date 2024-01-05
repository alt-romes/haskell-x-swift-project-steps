{-# LANGUAGE DerivingStrategies, DeriveAnyClass, OverloadedRecordDot #-}
module MyLib where

import Foreign.C
import Data.Aeson
import GHC.Generics

hs_factorial :: CInt -> CInt
hs_factorial x = product [1..x]

data User = User
  { name :: String
  , age :: Int
  }
  deriving stock Generic
  deriving anyclass (ToJSON, FromJSON)

birthday :: User -> User
birthday user = user{age = user.age + 1}
