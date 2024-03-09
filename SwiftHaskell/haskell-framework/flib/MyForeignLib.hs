{-# LANGUAGE ForeignFunctionInterface, MagicHash, UnboxedTuples #-}
module MyForeignLib where
import GHC.Exts
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal
import Data.Aeson
import Data.Bits
import Data.ByteString
import Data.ByteString.Unsafe
import MyLib (hs_factorial, birthday)

import Unsafe.Coerce
import GHC.IO

foreign export ccall hs_factorial :: CInt -> CInt

c_birthday :: Ptr CChar -> Int -> Ptr CChar -> Ptr Int -> IO ()
c_birthday cstr clen result size_ptr = do
  -- (1) Decode C string
  Just user <- decodeStrict <$> unsafePackCStringLen (cstr, clen)
    -- (2) Apply `birthday`
  let user_new = birthday user
  -- (3) Encode result
  unsafeUseAsCStringLen (toStrict $ encode user_new) $ \(ptr,len) -> do

    -- (3.2) What is the size of the result buffer?
    size_avail <- peek size_ptr

    -- (3.3) Write actual size to the int ptr.
    poke size_ptr len

    -- (3.4) If sufficient, we copy the result bytes to the given result buffer
    if size_avail < len
       then do
         -- We need @len@ bytes available
         -- The caller has to retry
         return ()
       else do
         moveBytes result ptr len

foreign export ccall c_birthday :: Ptr CChar -> Int -> Ptr CChar -> Ptr Int -> IO ()

data Rect
  = Rect
    { width :: {-# UNPACK #-} !Int
    , height :: {-# UNPACK #-} !Int
    }

myrect :: Rect
myrect = Rect 12 24

rectToAddr :: Rect -> Ptr ()
rectToAddr x = unsafePerformIO $ IO $ \rw ->
  case anyToAddr# x rw of
    (# rw, addr #) -> (# rw, Ptr addr #)

give_rect :: Ptr ()
give_rect =
  let
    -- Step 1
    tagged_ptr = rectToAddr myrect :: Ptr ()
    -- Step 1.5
    untagged_ptr = wordPtrToPtr (complement 7 .&. ptrToWordPtr tagged_ptr)
    -- Step 2
    ptr_final = untagged_ptr `plusPtr` 8 :: Ptr () -- 8 bytes
   in
    ptr_final

foreign export ccall give_rect :: Ptr ()

