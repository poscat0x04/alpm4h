module Foreign.ALPM.Database where

import           Data.Set (Set)
import           Data.Text (Text)
import           Foreign.ALPM.Internal.Types
import           Foreign.ALPM.Internal.Marshal
import           Foreign.ALPM.PublicAPI
import           Foreign.ALPM.List
import           Foreign.C.Types
import           Foreign
import           GHC.Stack


-----------------------------
-- Acquiring database

getLocalDB :: AlpmM AlpmDatabase
getLocalDB = do
    h <- ask
    liftIO $ alpmGetLocaldb h

getSyncDBs :: AlpmM (AlpmList AlpmDatabase)
getSyncDBs = do
    h <- ask
    hl <- liftIO $ alpmGetSyncdbs h
    return (fromHList hl)

registerSyncDB
    :: Text
    -> AlpmSigLevel
    -> AlpmM (Maybe AlpmDatabase)
registerSyncDB t sig = do
    h <- ask
    liftIO $ alpmRegisterSyncdb h t sig

unregisterAllSyncDBs :: AlpmM ()
unregisterAllSyncDBs = do
    h <- ask
    b <- liftIO $ alpmUnregisterAllSyncdbs h
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

dbUnregister :: AlpmDatabase -> AlpmM()
dbUnregister db = do
   b <- liftIO $ alpmDbUnregister db
   when b $ do
       h <- ask
       errno <- liftIO $ alpmErrno h
       stack <- liftIO currentCallStack
       throwError $ AlpmError stack (Just errno)

------------------------------
-- Package related

dbCheckPgpSignature
    :: AlpmDatabase
    -> AlpmSiglistPtr
    -> AlpmM Bool
dbCheckPgpSignature db siglist = do
    r <- liftIO $ alpmDbCheckPgpSignature db siglist
    case r of
      0 -> return True
      1 -> return False
      _ -> do
          stack <- liftIO currentCallStack
          throwError $ AlpmError stack Nothing

findGroupPkgs
    :: AlpmList AlpmDatabase
    -> Text
    -> AlpmM (AlpmList AlpmPackage)
findGroupPkgs dbs group =
    fmap fromHList $ liftIO $ alpmFindGroupPkgs (toHList dbs) group

syncGetNewVersion
    :: AlpmList AlpmDatabase
    -> AlpmPackage
    -> AlpmM AlpmPackage
syncGetNewVersion dbs pkg =
    liftIO $ alpmSyncGetNewVersion pkg (toHList dbs)


-----------------------------
-- Accessor

dbGetServers
    :: AlpmDatabase
    -> AlpmM [Text]
dbGetServers db = do
    csList <- fmap fromHList $ liftIO $ alpmDbGetServers db
    l <- liftIO $ toList csList
    liftIO $ traverse peekCString l

dbSetServers
    :: AlpmDatabase
    -> [Text]   -- ^ a list of servers. Note: the database will take ownership of the list and it should no longer be freed by the caller
    -> AlpmM ()
dbSetServers db l = do
    h <- ask
    l' <- liftIO $ toCStringList l
    b <- liftIO $ alpmDbSetServers db (toHList l')
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

dbAddServer
    :: AlpmDatabase
    -> Text
    -> AlpmM ()
dbAddServer db server = do
    b <- liftIO $ alpmDbAddServer db server
    when b $ do
        h <- ask
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

dbRemoveServer
    :: AlpmDatabase
    -> Text
    -> AlpmM ()
dbRemoveServer db server = do
    b <- liftIO $ alpmDbRemoveServer db server
    when b $ do
        h <- ask
        stack <- liftIO currentCallStack
        errno <- liftIO $ alpmErrno h
        throwError $ AlpmError stack (Just errno)

------------------------------------
--

dbGetName
    :: AlpmDatabase
    -> AlpmM (Maybe Text)
dbGetName db =
    liftIO $ alpmDbGetName db

dbGetSigLevel
    :: AlpmDatabase
    -> AlpmM AlpmSigLevel
dbGetSigLevel db = liftIO $ alpmDbGetSiglevel db

dbGetValid
    :: AlpmDatabase
    -> AlpmM ()
dbGetValid db = do
    b <- liftIO $ alpmDbGetValid db
    when b $ do
        h <- ask
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

dbUpdate
    :: Bool
    -> AlpmDatabase
    -> AlpmM Bool
dbUpdate force db = do
    r <- liftIO $ alpmDbUpdate force db
    when (r == -1) $ do
        h <- ask
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)
    if r == 0
      then return True
      else return False

dbGetPkg
    :: AlpmDatabase
    -> Text
    -> AlpmM (Maybe AlpmPackage)
dbGetPkg db name =
    liftIO $ alpmDbGetPkg db name

dbGetPkgCache
    :: AlpmDatabase
    -> AlpmM (AlpmList AlpmPackage)
dbGetPkgCache db = do
    fmap fromHList $ liftIO $ alpmDbGetPkgcache db

dbGetGroup
    :: AlpmDatabase
    -> Text
    -> AlpmM (Maybe AlpmGroupPtr)
dbGetGroup db name =
    liftIO $ alpmDbGetGroup db name

dbGetGroupCache
    :: AlpmDatabase
    -> AlpmM (AlpmList AlpmGroup)
dbGetGroupCache db =
    fmap fromHList $ liftIO $ alpmDbGetGroupcache db

dbSearch
    :: AlpmDatabase
    -> [Text]
    -> AlpmM (AlpmList AlpmPackage)
dbSearch db needles = do
    needles' <- liftIO $ toCStringList needles
    fmap fromHList $ liftIO $ alpmDbSearch db (toHList needles')

dbSetUsage
    :: AlpmDatabase
    -> Set AlpmDBUsage
    -> AlpmM ()
dbSetUsage db usage = do
    b <- liftIO $ alpmDbSetUsage db usage
    when b $ do
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack Nothing

dbGetUsage
    :: AlpmDatabase
    -> AlpmM (Set AlpmDBUsage)
dbGetUsage db = do
    (b, usage) <- liftIO $ alpmDbGetUsage db
    when b $ do
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack Nothing
    return usage
