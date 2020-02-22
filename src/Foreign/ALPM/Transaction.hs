module Foreign.ALPM.Transaction
       ( transInit
       , transPrepare
       , transRelease
       , transInterrupt
       , transCommit
       , transGetFlags
       , transGetAdd
       , transGetRemove
       ) where

import           Data.Set (Set)
import           Foreign.ALPM.Internal.Types
import           Foreign.ALPM.PublicAPI
import           Foreign.ALPM.List
import           GHC.Stack


transInit :: Set AlpmTransFlag -> AlpmMonad ()
transInit s = do
    h <- ask
    b <- liftIO $ alpmTransInit h s
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

transPrepare :: AlpmMonad (Maybe (Trace, AlpmErrno, AlpmList AlpmDepmissing))
transPrepare = do
    h <- ask
    (b, hlist) <- liftIO $ alpmTransPrepare h
    if b
      then do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        let l = fromHList hlist
        return $ Just (stack, errno, l)
      else
        return Nothing

transRelease :: AlpmMonad ()
transRelease = do
    h <- ask
    b <- liftIO $ alpmTransRelease h
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

transInterrupt :: AlpmMonad ()
transInterrupt = do
    h <- ask
    b <- liftIO $ alpmTransInterrupt h
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

transCommit :: AlpmMonad (Maybe (Trace, AlpmErrno, AlpmList AlpmFileConflict))
transCommit = do
    h <- ask
    (b, hlist) <- liftIO $ alpmTransCommit h
    if b
      then do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        let l = fromHList hlist
        return $ Just (stack, errno, l)
      else do
        return Nothing

transGetFlags :: AlpmMonad (Set AlpmTransFlag)
transGetFlags = do
    h <- ask
    liftIO $ alpmTransGetFlags h

transGetAdd :: AlpmMonad (AlpmList AlpmPackage)
transGetAdd = do
    h <- ask
    hlist <- liftIO $ alpmTransGetAdd h
    return $ fromHList hlist

transGetRemove :: AlpmMonad (AlpmList AlpmPackage)
transGetRemove = do
    h <- ask
    hlist <- liftIO $ alpmTransGetRemove h
    return $ fromHList hlist
