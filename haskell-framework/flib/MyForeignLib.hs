{-# LANGUAGE ForeignFunctionInterface #-}
module MyForeignLib where
import Foreign.C
import MyLib (hs_double)

foreign export ccall hs_double :: CInt -> CInt

