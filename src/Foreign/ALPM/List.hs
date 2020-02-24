{-# LANGUAGE MultiWayIf #-}
module Foreign.ALPM.List where

import           Control.Monad
import           Data.Text (Text)
import           Data.Text.Foreign
import           Foreign.ALPM.Internal.Types ( AlpmHList (..)
                                             , AlpmList  (..)
                                             , AlpmListPtr
                                             )
import           Foreign.ALPM.AlpmList
import           Foreign.C.Types
import           Foreign


-- | Annotate a HList
fromHList :: AlpmListPtr -> AlpmList a
fromHList = AlpmList

-- | Get the content of an annotated HList
toHList :: AlpmList a -> AlpmListPtr
toHList = unAlpmList

-- | Convert an AlpmList to a list
toList :: AlpmList a -> IO [Ptr a]
toList (AlpmList l) = do
    r <- toList' l
    alpmListFree l
    return r

-- | Convert an AlpmListPtr to a list without freeing the
-- underlying alpm_list_t
toList' :: AlpmListPtr -> IO [Ptr a]
toList' l =
    if l == nullPtr
      then return []
      else do
        hv <- peek l
        let v = payload hv
        next <- alpmListNext l
        tail <- toList' next
        return (castPtr v:tail)

freeList :: AlpmList a -> IO ()
freeList = alpmListFree . toHList

toCStringList :: [Text] -> IO (AlpmList CChar)
toCStringList l = alloca $ \lAddr -> do
    listPtr <- malloc   -- Allocate memory for initial list
    poke lAddr listPtr  -- store address in lAddr
    forM_ l $ \t -> do
        newListPtr <- alpmListAppendStrdup lAddr t  -- duplicate str and append to list
        poke lAddr newListPtr                       -- update address
    hl <- peek lAddr    -- get the pointer
    return (fromHList hl) -- annotate list then return
