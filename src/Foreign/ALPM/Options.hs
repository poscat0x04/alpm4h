module Foreign.ALPM.Options where

import           Control.Monad.Reader
import           Control.Monad.Except
import           Data.Text (Text)
import           Foreign
import           Foreign.ALPM.PublicAPI
import           Foreign.ALPM.Internal.Types
import           Foreign.ALPM.List
import           Foreign.C.Types
import           Foreign.ALPM.Internal.Marshal
import           GHC.Stack


getRoot :: AlpmM Text
getRoot = do
    h <- ask
    liftIO $ alpmOptionGetRoot h

getDBPath :: AlpmM Text
getDBPath = do
    h <- ask
    liftIO $ alpmOptionGetDbpath h

getLockfile :: AlpmM Text
getLockfile = do
    h <- ask
    liftIO $ alpmOptionGetLockfile h

getOverwriteFiles :: AlpmM (AlpmList AlpmFile)
getOverwriteFiles = do
    h <- ask
    fmap fromHList $ liftIO $ alpmOptionGetOverwriteFiles h

setOverwriteFiles :: AlpmList AlpmFile -> AlpmM ()
setOverwriteFiles hl = do
    h <- ask
    b <- liftIO $ alpmOptionSetOverwriteFiles h (toHList hl)
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

addOverwriteFile :: Text -> AlpmM ()
addOverwriteFile f = do
    h <- ask
    b <- liftIO $ alpmOptionAddOverwriteFile h f
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

getLogfile :: AlpmM Text
getLogfile = do
    h <- ask
    liftIO $ alpmOptionGetLogfile h

setLogfile :: Text -> AlpmM ()
setLogfile f = do
    h <- ask
    b <- liftIO $ alpmOptionSetLogfile h f
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

getGPGdir :: AlpmM Text
getGPGdir = do
    h <- ask
    liftIO $ alpmOptionGetGpgdir h

setGPGdir :: Text -> AlpmM ()
setGPGdir dir = do
    h <- ask
    b <- liftIO $ alpmOptionSetGpgdir h dir
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

getUseSyslog :: AlpmM Bool
getUseSyslog = do
    h <- ask
    liftIO $ alpmOptionGetUsesyslog h

setUseSyslog :: Bool -> AlpmM ()
setUseSyslog b = do
    h <- ask
    b' <- liftIO $ alpmOptionSetUsesyslog h b
    when b' $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

getArch :: AlpmM Text
getArch = do
    h <- ask
    liftIO $ alpmOptionGetArch h

setArch :: Text -> AlpmM ()
setArch arch = do
    h <- ask
    b <- liftIO $ alpmOptionSetArch h arch
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

getCheckspace :: AlpmM Int
getCheckspace = do
    h <- ask
    liftIO $ alpmOptionGetCheckspace h

setCheckspace :: Int -> AlpmM ()
setCheckspace space = do
    h <- ask
    b <- liftIO $ alpmOptionSetCheckspace h space
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

getDBExt :: AlpmM Text
getDBExt = do
    h <- ask
    liftIO $ alpmOptionGetDbext h

setDBExt :: Text -> AlpmM ()
setDBExt ext = do
    h <- ask
    b <- liftIO $ alpmOptionSetDbext h ext
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

getDefaultSigLevel :: AlpmM AlpmSigLevel
getDefaultSigLevel = do
    h <- ask
    liftIO $ alpmOptionGetDefaultSiglevel h

setDefaultSigLevel :: AlpmSigLevel -> AlpmM ()
setDefaultSigLevel siglevel = do
    h <- ask
    b <- liftIO $ alpmOptionSetDefaultSiglevel h siglevel
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

getRemoteFileSigLevel :: AlpmM AlpmSigLevel
getRemoteFileSigLevel = do
    h <- ask
    liftIO $ alpmOptionGetRemoteFileSiglevel h

setRemoteFileSigLevel :: AlpmSigLevel -> AlpmM ()
setRemoteFileSigLevel siglevel = do
    h <- ask
    b <- liftIO $ alpmOptionSetRemoteFileSiglevel h siglevel
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

setDisableDLTimeout :: Bool -> AlpmM ()
setDisableDLTimeout b = do
    h <- ask
    b' <- liftIO $ alpmOptionSetDisableDlTimeout h b
    when b' $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

getCacheDirs :: AlpmM [Text]
getCacheDirs = do
    h <- ask
    l <- fmap fromHList $ liftIO $ alpmOptionGetCachedirs h
    l' <- liftIO $ toList l
    liftIO $ traverse peekCString l'

setCacheDirs
    :: [Text]
    -> AlpmM ()
setCacheDirs tl = do
    h <- ask
    l <- liftIO $ toCStringList tl
    b <- liftIO $ alpmOptionSetCachedirs h (toHList l)
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

addCacheDir
    :: Text
    -> AlpmM ()
addCacheDir dir = do
    h <- ask
    b <- liftIO $ alpmOptionAddCachedir h dir
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

removeCacheDir
    :: Text
    -> AlpmM ()
removeCacheDir dir = do
    h <- ask
    b <- liftIO $ alpmOptionRemoveCachedir h dir
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

getHookDirs :: AlpmM [Text]
getHookDirs = do
    h <- ask
    l <- fmap fromHList $ liftIO $ alpmOptionGetHookdirs h
    l' <- liftIO $ toList l
    liftIO $ traverse peekCString l'

setHookDirs
    :: [Text]
    -> AlpmM ()
setHookDirs dirs = do
    h <- ask
    l <- liftIO $ toCStringList dirs
    b <- liftIO $ alpmOptionSetHookdirs h (toHList l)
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

addHookDir
    :: Text
    -> AlpmM ()
addHookDir dir = do
    h <- ask
    b <- liftIO $ alpmOptionAddHookdir h dir
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

removeHookDir
    :: Text
    -> AlpmM ()
removeHookDir dir = do
    h <- ask
    b <- liftIO $ alpmOptionRemoveHookdir h dir
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

getNoupgrades :: AlpmM (AlpmList AlpmFile)
getNoupgrades = do
    h <- ask
    hl <- liftIO $ alpmOptionGetNoupgrades h
    return (fromHList hl)

setNoupgrades
    :: AlpmList AlpmFile
    -> AlpmM ()
setNoupgrades fl = do
    h <- ask
    b <- liftIO $ alpmOptionSetNoupgrades h (toHList fl)
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

addNoupgrade
    :: Text
    -> AlpmM ()
addNoupgrade f = do
    h <- ask
    b <- liftIO $ alpmOptionAddNoupgrade h f
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

removeNoupgrade
    :: Text
    -> AlpmM ()
removeNoupgrade f = do
    h <- ask
    b <- liftIO $ alpmOptionRemoveNoupgrade h f
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO  currentCallStack
        throwError $ AlpmError stack (Just errno)

matchNoupgrade
    :: Text
    -> AlpmM Bool
matchNoupgrade f = do
    h <- ask
    b <- liftIO $ alpmOptionMatchNoupgrade h f
    return $ not b

getNoextracts :: AlpmM [Text]
getNoextracts = do
    h <- ask
    l <- fmap fromHList $ liftIO $ alpmOptionGetNoextracts h
    l' <- liftIO $ toList l
    liftIO $ traverse peekCString l'

addNoextracts
    :: Text
    -> AlpmM ()
addNoextracts f = do
    h <- ask
    b <- liftIO $ alpmOptionAddNoextract h f
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

setNoextracts
    :: [Text]
    -> AlpmM ()
setNoextracts fl = do
    h <- ask
    l <- liftIO $ toCStringList fl
    b <- liftIO $ alpmOptionSetNoextracts h (toHList l)
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

removeNoextract
    :: Text
    -> AlpmM ()
removeNoextract f = do
    h <- ask
    b <- liftIO $ alpmOptionRemoveNoextract h f
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

matchNoextract
    :: Text
    -> AlpmM Bool
matchNoextract f = do
    h <- ask
    b <- liftIO $ alpmOptionMatchNoextract h f
    return (not b)

getIgnorePkgs :: AlpmM (AlpmList AlpmPackage)
getIgnorePkgs = do
    h <- ask
    fmap fromHList $ liftIO $ alpmOptionGetIgnorepkgs h

addIgnorePkg
    :: Text
    -> AlpmM ()
addIgnorePkg p = do
    h <- ask
    b <- liftIO $ alpmOptionAddIgnorepkg h p
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

setIgnorePkgs
    :: AlpmList AlpmPackage
    -> AlpmM ()
setIgnorePkgs pl = do
    h <- ask
    b <- liftIO $ alpmOptionSetIgnorepkgs h (toHList pl)
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

removeIgnorePkg
    :: Text
    -> AlpmM ()
removeIgnorePkg p = do
    h <- ask
    b <- liftIO $ alpmOptionRemoveIgnorepkg h p
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

getIgnoreGroups :: AlpmM (AlpmList AlpmGroup)
getIgnoreGroups = do
    h <- ask
    fmap fromHList $ liftIO $ alpmOptionGetIgnoregroups h

addIgnoreGroup
    :: Text
    -> AlpmM ()
addIgnoreGroup g = do
    h <- ask
    b <- liftIO $ alpmOptionAddIgnoregroup h g
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

setIgnoreGroups
    :: AlpmList AlpmGroup
    -> AlpmM ()
setIgnoreGroups gs = do
    h <- ask
    b <- liftIO $ alpmOptionSetIgnoregroups h (toHList gs)
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

removeIgnoreGroup
    :: Text
    -> AlpmM ()
removeIgnoreGroup g = do
    h <- ask
    b <- liftIO $ alpmOptionRemoveIgnoregroup h g
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

getAssumeInstalled :: AlpmM (AlpmList AlpmDependency)
getAssumeInstalled = do
    h <- ask
    fmap fromHList $ liftIO $ alpmOptionGetAssumeinstalled h

addAssumeInstalled
    :: AlpmDependencyPtr
    -> AlpmM ()
addAssumeInstalled d = do
    h <- ask
    b <- liftIO $ alpmOptionAddAssumeinstalled h d
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

setAssumeInstalled
    :: AlpmList AlpmDependency
    -> AlpmM ()
setAssumeInstalled dl = do
    h <- ask
    b <- liftIO $ alpmOptionSetAssumeinstalled h (toHList dl)
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

removeAssumeInstalled
    :: AlpmDependencyPtr
    -> AlpmM ()
removeAssumeInstalled d = do
    h <- ask
    b <- liftIO $ alpmOptionRemoveAssumeinstalled h d
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)
