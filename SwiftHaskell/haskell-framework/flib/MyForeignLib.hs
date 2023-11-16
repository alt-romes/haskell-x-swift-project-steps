{-# LANGUAGE BangPatterns, ForeignFunctionInterface, TypeApplications, MagicHash, ExtendedLiterals, DuplicateRecordFields #-}
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

import Data.ByteString

import Data.Aeson
import Data.ByteString.Unsafe
import Foreign.Marshal.Utils
import Foreign.Storable

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
  -- Great if copying collector moves this around (can I bypass this by using the non-moving colector? :P)
  return $ Ptr (unsafeCoerce# (subWord64# (unsafeCoerce# val) 1#Word64))

derefsp :: StablePtr a -> IO (Ptr a)
derefsp ptr = do
  !val <- deRefStablePtr ptr
  return $ Ptr (unsafeCoerce# (subWord64# (unsafeCoerce# val) 1#Word64))

marshalUser :: Ptr User -> IO ()
marshalUser ptr = poke ptr (mkSimpleUser 23)
foreign export ccall marshalUser :: Ptr User -> IO ()


birthday :: User2 -> User2
birthday User2{age=x, birthYear=y}
  = User2{age=x+1, birthYear=y}

c_birthday :: Ptr CChar -> Int -- User
           -> Ptr CChar -> Ptr Int -- Result user
           -> IO ()
c_birthday cstr clen result size_ptr = do
  Just user <- decodeStrict <$> unsafePackCStringLen (cstr, clen)
  let user_new = birthday user
  unsafeUseAsCStringLen (toStrict $ encode user_new) $ \(ptr,len) -> do
    size_avail <- peek size_ptr
    -- Write actual size to intptr
    -- We always do this, either to see if we've overshot the buffer, or to
    -- know the size of what has been written.
    poke size_ptr len
    if size_avail < len
       then do
         -- We need @len@ bytes available
         -- The caller has to retry
         return ()
       else do
         moveBytes result ptr len

foreign export ccall c_birthday :: Ptr CChar -> Int -> Ptr CChar -> Ptr Int -> IO ()

