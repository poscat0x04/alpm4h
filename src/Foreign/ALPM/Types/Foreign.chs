{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Foreign.ALPM.Types.Foreign where

import           Control.Exception
import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.C.Types
import           Foreign

#include <alpm.h>


{#enum alpm_errno_t as AlpmErrno {underscoreToCase} deriving (Show, Eq, Ord) #}

instance Storable AlpmErrno where
    sizeOf _ = {#sizeof alpm_errno_t #}
    alignment _ = {#alignof alpm_errno_t #}
    peek ptr = do
        val <- peek (castPtr ptr)
        return $ toEnum $ fromEnum (val :: CInt)
    poke ptr v = poke (castPtr ptr) (toEnum $ fromEnum v :: CInt)

{#enum alpm_transflag_t as AlpmTransFlag {underscoreToCase} deriving (Show, Eq, Ord) #}

{#enum alpm_loglevel_t as AlpmLogLevel {underscoreToCase} deriving (Show, Eq, Ord) #}

{#enum alpm_pkgreason_t as AlpmPkgReason {underscoreToCase} deriving (Show, Eq, Ord) #}

{#enum alpm_pkgfrom_t as AlpmPkgFrom {underscoreToCase} deriving (Show, Eq, Ord) #}

{#enum alpm_pkgvalidation_t as AlpmPkgValidation {underscoreToCase} deriving (Show, Eq, Ord) #}

{#enum alpm_depmod_t as AlpmDepmod {underscoreToCase} deriving (Show, Eq, Ord) #}

{#enum alpm_fileconflicttype_t as AlpmFileConflictType {underscoreToCase} deriving (Show, Eq, Ord) #}

{#enum alpm_siglevel_t as AlpmSigLevel {underscoreToCase} deriving (Show, Eq, Ord) #}

{#enum alpm_sigstatus_t as AlpmSigStatus {underscoreToCase} deriving (Show, Eq, Ord) #}

{#enum alpm_sigvalidity_t as AlpmSigValidity {underscoreToCase} deriving (Show, Eq, Ord) #}

{#enum alpm_event_type_t as AlpmEventType {underscoreToCase} deriving (Show, Eq, Ord) #}

{#enum alpm_package_operation_t as AlpmPackageOperation {underscoreToCase} deriving (Show, Eq, Ord) #}

{#enum alpm_question_type_t as AlpmQuestionType {underscoreToCase} deriving (Show, Eq, Ord) #}

{#enum alpm_progress_t as AlpmProgress {underscoreToCase} deriving (Show, Eq, Ord) #}

{#enum alpm_db_usage_t as AlpmDBUsage {underscoreToCase} deriving (Show, Eq, Ord) #}

{#enum alpm_caps as AlpmCaps {underscoreToCase} deriving (Show, Eq, Ord) #}

-----------------------------------------------
-- Opaque Pointers

{#pointer *alpm_handle_t as AlpmHandle newtype #}

deriving instance Storable AlpmHandle

{#pointer *alpm_db_t as AlpmDatabase newtype #}

deriving instance Storable AlpmDatabase

{#pointer *alpm_pkg_t as AlpmPackage newtype #}

deriving instance Storable AlpmPackage

{#pointer *alpm_trans_t as AlpmTransaction newtype #}

deriving instance Storable AlpmTransaction

----------------------------------------------
-- Pointers

{#pointer *alpm_group_t as AlpmGroup newtype #}

deriving instance Storable AlpmGroup

{#pointer *alpm_file_t as AlpmFile newtype #}

deriving instance Storable AlpmFile

{#pointer *alpm_filelist_t as AlpmFilelist newtype #}

deriving instance Storable AlpmFilelist

{#pointer *alpm_siglist_t as AlpmSiglist newtype #}

deriving instance Storable AlpmSiglist

{#pointer *alpm_errno_t as AlpmErrnoPtr -> AlpmErrno #}

{#pointer *alpm_conflict_t as AlpmConflict newtype #}

deriving instance Storable AlpmConflict

{#pointer *alpm_fileconflict_t as AlpmFileConflict newtype #}

deriving instance Storable AlpmFileConflict

{#pointer *alpm_depend_t as AlpmDependency newtype #}

deriving instance Storable AlpmDependency

{#pointer *alpm_depmissing_t as AlpmDepmissing newtype #}

deriving instance Storable AlpmDepmissing

{#pointer *alpm_list_t as AlpmListPtr newtype #}

deriving instance Storable AlpmListPtr
