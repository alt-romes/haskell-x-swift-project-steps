{-# LANGUAGE BangPatterns, ForeignFunctionInterface, TypeApplications, MagicHash, ExtendedLiterals #-}
module MyForeignLib where
import GHC.Exts
import Text.Printf
import GHC.Prim
import Data.Word
import Foreign.C
import Foreign.StablePtr
import Foreign.Ptr
import Data.Primitive.Ptr
import System.IO.Unsafe
import System.IO
import Data.IORef
import Unsafe.Coerce

import MyLib

foreign export ccall hs_factorial :: CInt -> CInt

foreign export ccall mk_simple_user :: CLong -> IO (Ptr User)
foreign export ccall derefsp :: StablePtr a -> IO (Ptr a)

-- mk_simple_user :: CLong -> IO (StablePtr CLong)
-- mk_simple_user !x = do
--   let y = mkSimpleUser x :: User
--   let ptr_to_y = unsafeCoerce# y :: Word64#
--     in do
--       print $ I# (unsafeCoerce# (and64# ptr_to_y 0xfffffffffffffff8#Word64))
--       unsafeCoerce# $
--         and64#
--         ptr_to_y
--         0xfffffffffffffff8#Word64

mk_simple_user :: CLong -> IO (Ptr User)
mk_simple_user x = do
  -- This has to be forced, so we know the tag is 1
  let !y = mkSimpleUser x :: User
  ptr <- newStablePtr y
  !val <- deRefStablePtr ptr
  -- How shall we ever free the stable ptr?
  -- A massive space leak
  return $ Ptr (unsafeCoerce# (subWord64# (unsafeCoerce# val) 1#Word64))

derefsp :: StablePtr a -> IO (Ptr a)
derefsp ptr = do
  !val <- deRefStablePtr ptr
  return $ Ptr (unsafeCoerce# (subWord64# (unsafeCoerce# val) 1#Word64))
