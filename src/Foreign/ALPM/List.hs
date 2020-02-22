module Foreign.ALPM.List where

import           Foreign.ALPM.Internal.Types ( AlpmHList(..)
                                             , AlpmList
                                             , AlpmListPtr
                                             )
import           Foreign.ALPM.AlpmList
import           Foreign


-- | Converts a HList to List recursively
fromHList :: AlpmListPtr -> IO (AlpmList a)
fromHList l = do
    head <- alpmListNth l 0
    r <- go head
    alpmListFree head
    return r
    where
      go :: AlpmListPtr -> IO (AlpmList a)
      go h = do
          hv <- peek h
          let v = payload hv
          next <- alpmListNext h
          if next == nullPtr
            then return [castPtr v]
            else do
              tail <- go next
              return (castPtr v:tail)
