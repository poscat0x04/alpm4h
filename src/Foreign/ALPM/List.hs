module Foreign.ALPM.List where

import           Foreign.ALPM.Internal.Types ( AlpmHList (..)
                                             , AlpmList  (..)
                                             , AlpmListPtr
                                             )
import           Foreign.ALPM.AlpmList
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
    head <- alpmListNth l 0
    r <- go head
    alpmListFree head
    return r
    where
      go :: AlpmListPtr -> IO [Ptr a]
      go h = do
          hv <- peek h
          let v = payload hv
          next <- alpmListNext h
          if next == nullPtr
            then return [castPtr v]
            else do
              tail <- go next
              return (castPtr v:tail)
