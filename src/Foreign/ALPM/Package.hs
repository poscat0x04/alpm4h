module Foreign.ALPM.Package where

import           Control.Monad.Reader
import           Control.Monad.Except
import           Data.Text (Text, pack)
import           Foreign.ALPM.Internal.Types
import           Foreign.ALPM.PublicAPI
import           Foreign.ALPM.Internal.Marshal
import           Foreign.ALPM.List
import           Foreign.C.String hiding (peekCString)
import           Foreign.C.Types
import           Foreign
import           GHC.Stack


-- | Package related transaction functions

syncSysupgrade
    :: Bool     -- ^ whether allow downgrade
    -> AlpmM ()
syncSysupgrade b = do
    h <- ask
    r <- liftIO $ alpmSyncSysupgrade h b
    when r $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

addPackage
    :: AlpmPackage
    -> AlpmM ()
addPackage p = do
    h <- ask
    b <- liftIO $ alpmAddPkg h p
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)
removePackage
    :: AlpmPackage
    -> AlpmM ()
removePackage p = do
    h <- ask
    b <- liftIO $ alpmRemovePkg h p
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

-- | Packag acquiring and freeing

pkgLoad
    :: Text           -- ^ location of the package tarball
    -> Bool           -- ^ whether to stop the load after metadata is read or continue through the full archive
    -> AlpmSigLevel   -- ^ what level of package signature checking to perform on the package
    -> AlpmM AlpmPackage
pkgLoad name b sig = do
    h <- ask
    (b', p) <- liftIO $ alpmPkgLoad h name b sig
    if b'
      then do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)
      else return p

pkgFind
    :: AlpmList AlpmPackage
    -> Text
    -> AlpmM (Maybe AlpmPackage)
pkgFind (AlpmList heystack) needle = do
    p@(AlpmPackage ptr) <- liftIO $ alpmPkgFind heystack needle
    if ptr == nullPtr
      then return Nothing
      else return $ Just p

pkgFree
    :: AlpmPackage
    -> AlpmM ()
pkgFree p = do
    h <- ask
    b <- liftIO $ alpmPkgFree p
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)

pkgGetFilename :: AlpmPackage -> AlpmM Text
pkgGetFilename p = liftIO $ alpmPkgGetFilename p

pkgGetBase :: AlpmPackage -> AlpmM Text
pkgGetBase p = liftIO $ alpmPkgGetBase p

pkgGetName :: AlpmPackage -> AlpmM Text
pkgGetName p = liftIO $ alpmPkgGetName p

pkgGetVersion :: AlpmPackage -> AlpmM Text
pkgGetVersion p = liftIO $ alpmPkgGetVersion p

pkgGetOrigin :: AlpmPackage -> AlpmM (Maybe AlpmPkgFrom)
pkgGetOrigin p = do
    r <- liftIO $ alpmPkgGetOrigin p
    case r of
      -1 -> return Nothing
      _  -> return $ Just (toEnum r)

pkgGetDesc :: AlpmPackage -> AlpmM Text
pkgGetDesc p = liftIO $ alpmPkgGetDesc p

pkgGetUrl :: AlpmPackage -> AlpmM Text
pkgGetUrl p = liftIO $ alpmPkgGetUrl p

pkgGetBuilddate :: AlpmPackage -> AlpmM Int64
pkgGetBuilddate p = liftIO $ alpmPkgGetBuilddate p

pkgGetInstalldate :: AlpmPackage -> AlpmM Int64
pkgGetInstalldate p = liftIO $ alpmPkgGetInstalldate p

pkgGetPackager :: AlpmPackage -> AlpmM Text
pkgGetPackager p = liftIO $ alpmPkgGetPackager p

pkgGetMD5 :: AlpmPackage -> AlpmM Text
pkgGetMD5 p = liftIO $ alpmPkgGetMd5sum p

pkgGetSHA256 :: AlpmPackage -> AlpmM Text
pkgGetSHA256 p = liftIO $ alpmPkgGetSha256sum p

pkgGetArch :: AlpmPackage -> AlpmM Text
pkgGetArch p = liftIO $ alpmPkgGetArch p

pkgGetSize :: AlpmPackage -> AlpmM Int64
pkgGetSize p = liftIO $ alpmPkgGetSize p

pkgGetISize :: AlpmPackage -> AlpmM Int64
pkgGetISize p = liftIO $ alpmPkgGetIsize p

pkgGetReason :: AlpmPackage -> AlpmM AlpmPkgReason
pkgGetReason p = liftIO $ alpmPkgGetReason p

pkgGetLicenses :: AlpmPackage -> AlpmM [Text]
pkgGetLicenses p = do
    hl <- liftIO $ alpmPkgGetLicenses p
    l <- liftIO $ toList (fromHList hl :: AlpmList CChar)
    forM l $ \ptr -> liftIO $ peekCString ptr

pkgGetGroups :: AlpmPackage -> AlpmM [Text]
pkgGetGroups p = do
    hl <- liftIO $ alpmPkgGetLicenses p
    l <- liftIO $ toList (fromHList hl :: AlpmList CChar)
    forM l $ \ptr -> liftIO $ peekCString ptr

pkgGetDepends :: AlpmPackage -> AlpmM (AlpmList AlpmDependency)
pkgGetDepends p = fmap fromHList $ liftIO $ alpmPkgGetDepends p

pkgGetOptdepends :: AlpmPackage -> AlpmM (AlpmList AlpmDependency)
pkgGetOptdepends p = fmap fromHList $ liftIO $ alpmPkgGetOptdepends p

pkgGetCheckdepends :: AlpmPackage -> AlpmM (AlpmList AlpmDependency)
pkgGetCheckdepends p = fmap fromHList $ liftIO $ alpmPkgGetCheckdepends p

pkgGetMakedepends :: AlpmPackage -> AlpmM (AlpmList AlpmDependency)
pkgGetMakedepends p = fmap fromHList $ liftIO $ alpmPkgGetMakedepends p

pkgGetConflicts :: AlpmPackage -> AlpmM (AlpmList AlpmDependency)
pkgGetConflicts p = fmap fromHList $ liftIO $ alpmPkgGetConflicts p

pkgGetProvides :: AlpmPackage -> AlpmM (AlpmList AlpmDependency)
pkgGetProvides p = fmap fromHList $ liftIO $ alpmPkgGetProvides p

pkgGetReplaces :: AlpmPackage -> AlpmM (AlpmList AlpmDependency)
pkgGetReplaces p = fmap fromHList $ liftIO $ alpmPkgGetReplaces p

pkgGetFiles :: AlpmPackage -> AlpmM AlpmFilelistPtr
pkgGetFiles p = liftIO $ alpmPkgGetFiles p

pkgGetBackup :: AlpmPackage -> AlpmM (AlpmList AlpmBackup)
pkgGetBackup p = fmap fromHList $ liftIO $ alpmPkgGetBackup p

pkgGetDB :: AlpmPackage -> AlpmM AlpmDatabase
pkgGetDB p = liftIO $ alpmPkgGetDb p

pkgGetBase64Sig :: AlpmPackage -> AlpmM Text
pkgGetBase64Sig p = liftIO $ alpmPkgGetBase64Sig p

pkgGetValidation :: AlpmPackage -> AlpmM AlpmPkgValidation
pkgGetValidation p = liftIO $ alpmPkgGetValidation p

pkgHasScriptlet :: AlpmPackage -> AlpmM Bool
pkgHasScriptlet p = liftIO $ alpmPkgHasScriptlet p

pkgDownloadSize :: AlpmPackage -> AlpmM Int64
pkgDownloadSize p = liftIO $ alpmPkgDownloadSize p

pkgSetReason :: AlpmPackage -> AlpmPkgReason -> AlpmM ()
pkgSetReason p reason= do
    b <- liftIO $ alpmPkgSetReason p reason
    when b $ do
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack Nothing

pkgCheckMD5'
    :: AlpmPackage
    -> AlpmM Bool
pkgCheckMD5' p = fmap not $ liftIO $ alpmPkgCheckmd5sum p

pkgVerCmp
    :: Text
    -> Text
    -> AlpmM Ordering
pkgVerCmp t1 t2 = liftIO $ alpmPkgVercmp t1 t2

pkgGetRequiredBy
    :: AlpmPackage
    -> AlpmM (AlpmList AlpmPackage)
pkgGetRequiredBy p = do
    l <- liftIO $ alpmPkgComputeRequiredby p
    return (fromHList l)

pkgGetOptionalFor
    :: AlpmPackage
    -> AlpmM (AlpmList AlpmPackage)
pkgGetOptionalFor p = do
    l <- liftIO $ alpmPkgComputeOptionalfor p
    return (fromHList l)

pkgShouldIgnore
    :: AlpmPackage
    -> AlpmM Bool
pkgShouldIgnore p = do
    h <- ask
    liftIO $ alpmPkgShouldIgnore h p

filelistContains
    :: AlpmFilelistPtr
    -> Text
    -> AlpmM (Maybe AlpmFile)
filelistContains l path = do
    ptr <- liftIO $ alpmFilelistContains l path
    liftIO $ maybePeek peek ptr

pkgCheckPgpSignature
    :: AlpmPackage
    -> AlpmSiglistPtr
    -> AlpmM Bool
pkgCheckPgpSignature p siglist = do
    r <- liftIO $ alpmPkgCheckPgpSignature p siglist
    case r of
      0 -> return True
      1 -> return False
      _ -> do
          stack <- liftIO currentCallStack
          throwError $ AlpmError stack Nothing

decodeSignature
    :: Text  -- ^ the signature encoded in base64
    -> AlpmM (Sig, SigLen)
decodeSignature encoded = do
    (b, sig, len) <- liftIO $ alpmDecodeSignature encoded
    when b $ do
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack Nothing
    return (sig, len)

extractKeyid
    :: Text  -- ^ pgp key identifier
    -> (Sig, SigLen)
    -> AlpmM (AlpmList AlpmPgpkey)
extractKeyid identifier (sig, len) = do
    h <- ask
    (b, hl) <- liftIO $ alpmExtractKeyid h identifier sig len
    when b $ do
        errno <- liftIO $ alpmErrno h
        stack <- liftIO currentCallStack
        throwError $ AlpmError stack (Just errno)
    return (fromHList hl)

-- | combines decodeSignature and extractKeyid
decodeThenExtract
    :: Text
    -> Text
    -> AlpmM (AlpmList AlpmPgpkey)
decodeThenExtract encoded identifier = do
    p@(sig, _) <- decodeSignature encoded
    l <- extractKeyid identifier p
    liftIO $ free sig
    return l
