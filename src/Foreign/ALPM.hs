module Foreign.ALPM
       ( runAlpmM
       , showErrno
       , getErrno
       , fetchPkgurl
       , alpmVersion
       , alpmCapabilities
       ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Set (Set)
import           Data.Text (Text)
import           Foreign.ALPM.PublicAPI
import           Foreign.ALPM.Internal.Types
import           Foreign
import           GHC.Stack


-- | Running an AlpmM, all errors occured in buissness logic is captured
-- in Either, async excpetions still might be thrown, error will be called if the
-- handled failed to release.
runAlpmM
  :: Text         -- ^ path to root
  -> Text         -- ^ path to database
  -> AlpmM a
  -> IO (Either AlpmError a)
runAlpmM root dbpath (AlpmM m) =
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

showErrno :: AlpmErrno -> IO Text
showErrno = alpmStrerror

getErrno :: AlpmM AlpmErrno
getErrno = do
    h <- ask
    liftIO $ alpmErrno h

fetchPkgurl :: Text -> AlpmM Text
fetchPkgurl pkg = do
    h <- ask
    liftIO $ alpmFetchPkgurl h pkg

unlock :: AlpmM ()
unlock = do
    h <- ask
    b <- liftIO $ alpmUnlock h
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

