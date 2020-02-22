{-# LANGUAGE TemplateHaskell #-}
module Foreign.ALPM.Internal.GStorableInstances where

import           Foreign.Storable.Generic
import           Foreign.C.Types
import           Foreign.ALPM.Internal.TH


deriveGStorable ''CSize
