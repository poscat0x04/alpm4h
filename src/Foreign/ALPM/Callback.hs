{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleContexts #-}
module Foreign.ALPM.Callback
       ( LogCallback
       , DownloadCallback
       , FetchCallback
       , TotalDlCallback
       , EventCallback
       , QuestionCallback
       , ProgressCallback
       , setLogCallback
       , setDownloadCallback
       , setFetchCallback
       , setTotalDlCallback
       , setEventCallback
       , setQuestionCallback
       , setProgressCallback
       ) where


import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Except
import           Data.Coerce
import           Data.Text (Text)
import           Foreign.ALPM.PublicAPI
import           Foreign.ALPM.Internal.Types
import           Foreign.ALPM.Internal.Marshal
import           Foreign
import           Foreign.C.Types
import           GHC.Stack


foreign import ccall "wrapper"
  mkLogcb :: LogCbFun -> IO LogCb

foreign import ccall "wrapper"
  mkDownloadcb :: DownloadCbFun -> IO DownloadCb

foreign import ccall "wrapper"
  mkFetchcb :: FetchCbFun -> IO FetchCb

foreign import ccall "wrapper"
  mkTotaldlcb :: TotalDlCbFun -> IO TotalDlCb

foreign import ccall "wrapper"
  mkEventcb :: EventCbFun -> IO EventCb

foreign import ccall "wrapper"
  mkQuestioncb :: QuestionCbFun -> IO QuestionCb

foreign import ccall "wrapper"
  mkProgresscb :: ProgressCbFun -> IO ProgressCb


type LogCallback = Int -> Text -> IO ()

type DownloadCallback = Text -> Int64 -> Int64 -> IO ()

type FetchCallback = Text -> Text -> Int -> IO Int

type TotalDlCallback = Int64 -> IO ()

type EventCallback = EventCbFun

type QuestionCallback = QuestionCbFun

type ProgressCallback
    =  AlpmProgress  -- ^ the kind of event that is progressing
    -> Text          -- ^ for package operations, the name of the package being operated on
    -> Int           -- ^ the percent completion of the action
    -> Word64        -- ^ the total amount of items in the action
    -> Word64        -- ^ the current amount of items completed
    -> IO ()

setLogCallback :: LogCallback -> AlpmM ()
setLogCallback cb = do
    h <- ask
    let cbFun i s _ = do
            t <- peekCString s
            cb (fromEnum i) t
    cb' <- liftIO $ mkLogcb cbFun
    b <- liftIO $ alpmOptionSetLogcb h cb'
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

setDownloadCallback :: DownloadCallback -> AlpmM ()
setDownloadCallback cb = do
    h <- ask
    let cbFun s i1 i2 = do
            t <- peekCString s
            cb t (coerce i1) (coerce i2)
    cb' <- liftIO $ mkDownloadcb cbFun
    b <- liftIO $ alpmOptionSetDlcb h cb'
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

setFetchCallback :: FetchCallback -> AlpmM ()
setFetchCallback cb = do
    h <- ask
    let cbFun s1 s2 i = do
            t1 <- peekCString s1
            t2 <- peekCString s2
            r <- cb t1 t2 (fromEnum i)
            return $ toEnum r
    cb' <- liftIO $ mkFetchcb cbFun
    b <- liftIO $ alpmOptionSetFetchcb h cb'
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

setTotalDlCallback :: TotalDlCallback -> AlpmM ()
setTotalDlCallback cb = do
    h <- ask
    let cbFun i = cb (coerce i)
    cb' <- liftIO $ mkTotaldlcb cbFun
    b <- liftIO $ alpmOptionSetTotaldlcb h cb'
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

setEventCallback :: EventCallback -> AlpmM ()
setEventCallback cb = do
    h <- ask
    cb' <- liftIO $ mkEventcb cb
    b <- liftIO $ alpmOptionSetEventcb h cb'
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

setQuestionCallback :: QuestionCallback -> AlpmM ()
setQuestionCallback cb = do
    h <- ask
    cb' <- liftIO $ mkQuestioncb cb
    b <- liftIO $ alpmOptionSetQuestioncb h cb'
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

warpProgressCb :: ProgressCallback -> IO ProgressCb
warpProgressCb hcb =
    let from = toEnum . fromEnum in
    let cbFun e n p t c = do
            cs <- peekCString n
            hcb (from e) cs (from p) (coerce t) (coerce c)
     in mkProgresscb cbFun

setProgressCallback :: ProgressCallback -> AlpmM ()
setProgressCallback cb = do
    h <- ask
    cb' <- liftIO $ warpProgressCb cb
    b <- liftIO $ alpmOptionSetProgresscb h cb'
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)
