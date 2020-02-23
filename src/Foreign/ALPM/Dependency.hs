module Foreign.ALPM.Dependency where

import           Data.Text (Text)
import           Foreign
import           Foreign.ALPM.List
import           Foreign.ALPM.Internal.Types
import           Foreign.ALPM.PublicAPI


checkDeps
    :: AlpmList AlpmPackage  -- ^ the list of local packages
    -> AlpmList AlpmPackage  -- ^ a list of packages to be removed
    -> AlpmList AlpmPackage  -- ^ a list of packages to be upgraded (remove-then-upgrade)
    -> Bool                  -- ^ handles the backward dependencies
    -> AlpmM (AlpmList AlpmDepmissing)
checkDeps pkglist remove upgrade reversedeps = do
    h <- ask
    hl <- liftIO $
        alpmCheckdeps h (toHList pkglist) (toHList remove) (toHList upgrade) reversedeps
    return (fromHList hl)

findSatisfier
    :: AlpmList AlpmPackage  -- ^ a list of packages where the satisfier will be searched
    -> Text                  -- ^ package or provision name, versioned or not
    -> AlpmM AlpmPackage
findSatisfier l depstring =
    liftIO $ alpmFindSatisfier (toHList l) depstring

checkConflicts
    :: AlpmList AlpmPackage
    -> AlpmM (AlpmList AlpmConflict)
checkConflicts l = do
    h <- ask
    hl <- liftIO $ alpmCheckconflicts h (toHList l)
    return (fromHList hl)

showDep
    :: AlpmDependencyPtr
    -> AlpmM (Text)
showDep dep = liftIO $ alpmDepComputeString dep

freeDep
    :: AlpmDependencyPtr
    -> AlpmM ()
freeDep dep = liftIO $ alpmDepFree dep

readDep
    :: Text
    -> AlpmM AlpmDependencyPtr
readDep str = liftIO $ alpmDepFromString str
