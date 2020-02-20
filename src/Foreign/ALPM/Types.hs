{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Foreign.ALPM.Types
       ( module Foreign.ALPM.Types.Foreign
       , AlpmError (..)
       , AlpmErrno (..)
       , AlpmMonad (..)
       ) where

import           Foreign.ALPM.Types.Foreign
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Except
import           Control.Exception
import           Data.Dynamic


type Trace = [String]

data AlpmError = AlpmError Trace (Maybe AlpmErrno) deriving (Typeable, Show)

instance Exception AlpmError where

newtype AlpmMonad a
    = AlpmMonad (ReaderT AlpmHandle (ExceptT AlpmError IO) a)
    deriving (Functor, Applicative, Monad, MonadIO)

deriving instance MonadError AlpmError AlpmMonad
deriving instance MonadReader AlpmHandle AlpmMonad
