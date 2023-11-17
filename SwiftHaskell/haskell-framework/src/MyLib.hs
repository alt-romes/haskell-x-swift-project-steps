{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
module MyLib where

import Data.Aeson
import GHC.Generics
import Foreign.C
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable.Generic
import Language.Haskell.TH
import Control.Monad
import Data.Aeson
import Data.ByteString.Unsafe
import Foreign.Marshal.Utils
import Foreign.Storable

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


foreignExportSwift :: Name -> Q [Dec]
foreignExportSwift fun_name = do
  let wrapper_name_str = ('h':) (nameBase fun_name)
      wrapper_name = mkName wrapper_name_str
  callresult_name <- newName "callresult"
  fun_ty     <- reifyType fun_name
  wrapper_ty <- makeWrapperTy fun_ty
  unless ((tyFunArity fun_ty)*2 + 2 == tyFunArity wrapper_ty)
    $ error $ unlines [ "wrapper_ty arity does not match fun_ty arity:"
                      , "wrapper_ty: " ++ show wrapper_ty
                      , "fun_ty: " ++ show fun_ty
                      ]
  let
    fexp = ForeignD $ ExportF CCall wrapper_name_str wrapper_name wrapper_ty
    fsig = SigD wrapper_name wrapper_ty

  -- Vars of original function
  orgVars <- mapM (const $ newName "org")
                  [1..tyFunArity fun_ty]
  -- Actual vars
  argVars <- mapM (\x -> newName $ (if odd x then "cstr" else "clen"))
                  [1..tyFunArity fun_ty * 2]
  -- Two arguments to write the output to
  let buffer = mkName "buffer"
      sizeptr = mkName "size_ptr"
  fun <- FunD wrapper_name . (:[]) . (\b -> Clause (map VarP (argVars ++ [buffer, sizeptr])) (NormalB b) []) . DoE Nothing <$> do
           binds <-
             forM (zip3 orgVars argVars (drop 1 argVars)) \(org, varE -> cstr, varE -> clen) -> do
               BindS (ConP 'Just [] [VarP org]) <$> [| decodeStrict <$> unsafePackCStringLen ($cstr, $clen) |]
           let applyF = foldl AppE (VarE fun_name) (map VarE orgVars)
               resultBind
                | resultIsIO fun_ty = BindS (VarP callresult_name) applyF
                | otherwise = LetS [ValD (VarP callresult_name) (NormalB applyF) []]
           body <-
             [| unsafeUseAsCStringLen (toStrict $ encode $(varE callresult_name)) $ \(ptr,len) -> do
                   size_avail <- peek $(varE sizeptr)
                   -- Write actual size to intptr
                   -- We always do this, either to see if we've overshot the buffer, or to
                   -- know the size of what has been written.
                   poke $(varE sizeptr) len
                   if size_avail < len
                      then do
                        -- We need @len@ bytes available
                        -- The caller has to retry
                        return ()
                      else do
                        moveBytes $(varE buffer) ptr len
              |]
           return (binds++[resultBind]++[NoBindS body])
  return [fsig, fun, fexp]


-- | ROMES:TODO: Very incomplete (e.g. ForallT)
makeWrapperTy :: Type -> Q Type
makeWrapperTy (AppT (AppT ArrowT _a) b) = [t| (Ptr CChar) -> Int -> $(makeWrapperTy b) |]
makeWrapperTy _x = [t| Ptr CChar -> Ptr Int -> IO () |]

-- | ROMES:TODO: Very incomplete (e.g. ForallT)
tyFunArity :: Type -> Int
tyFunArity (AppT (AppT ArrowT _) b) = 1 + tyFunArity b
tyFunArity _ = 0

resultIsIO :: Type -> Bool
resultIsIO (AppT (AppT ArrowT _) b) = resultIsIO b
resultIsIO (AppT (ConT n) _) | n == ''IO = True
resultIsIO _ = False

