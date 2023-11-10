{-# LANGUAGE ForeignFunctionInterface #-}
module MyForeignLib where
import Foreign.C
import MyLib (hs_factorial)

foreign export ccall hs_factorial :: CInt -> CInt

