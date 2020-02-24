module Foreign.ALPM.Logging where

import           Data.Text (Text)
import           Foreign
import           Foreign.ALPM.PublicAPI
import           Foreign.ALPM.Internal.Types
import           GHC.Stack


logAction
    :: Text  -- ^ caller-specific prefix for the log
    -> Text  -- ^ output format
    -> AlpmM ()
logAction prefix fmt = do
    h <- ask
    b <- liftIO $ alpmLogaction h prefix fmt
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)
