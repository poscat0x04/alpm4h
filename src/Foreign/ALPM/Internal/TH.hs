{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
module Foreign.ALPM.Internal.TH where

import           Foreign
import           Foreign.C.Types
import           Foreign.Storable.Generic
import           Language.Haskell.TH


deriveStorable :: Name -> Q [Dec]
deriveStorable name = [d|
   instance Storable $a where
    sizeOf _ = 4
    alignment _ = 4
    peek ptr = do
        val <- peek (castPtr ptr)
        return $ toEnum $ fromEnum (val :: CInt)
    poke ptr v = poke (castPtr ptr) (toEnum $ fromEnum v :: CInt)
    |]
    where
      a = conT name

deriveGStorable :: Name -> Q [Dec]
deriveGStorable name = [d|
    instance GStorable $a where
      gsizeOf = sizeOf
      galignment = alignment
      gpeekByteOff = peekByteOff
      gpokeByteOff = pokeByteOff
    |]
    where
      a = conT name
