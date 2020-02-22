module Foreign.ALPM
       ( runAlpmMonad
       ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Text (Text)
import           Foreign.ALPM.PublicAPI
import           Foreign.ALPM.Internal.Types
import           Foreign
import           GHC.Stack


-- | Running an AlpmMonad, all errors occured in buissness logic is captured
-- in Either, async excpetions still might be thrown, error will be called if the
-- handled failed to release.
runAlpmMonad
  :: Text         -- ^ path to root
  -> Text         -- ^ path to database
  -> AlpmMonad a
  -> IO (Either AlpmError a)
runAlpmMonad root dbpath (AlpmMonad m) =
    alloca $ \ptr -> do
    h@(AlpmHandle handle) <- alpmInitialize root dbpath ptr
    if handle == nullPtr
        then do
        errno <- peek ptr
        stack <- currentCallStack
        return $ Left $ AlpmError stack (Just errno)
        else do
        r <- runExceptT $ runReaderT m h
        b' <- alpmRelease h
        if b'
            then error "handle release failed"
            else return r
