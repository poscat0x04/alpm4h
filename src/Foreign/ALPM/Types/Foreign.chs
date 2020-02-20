module Foreign.ALPM.Types.Foreign where

import           Control.Exception
import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.C.Types

#include <alpm.h>
#include <alpm_list.h>


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




data AlpmList
    = AlpmList
      { prev  :: Ptr ()
      , next  :: Ptr ()
      , value :: Ptr ()
      }

{#pointer *alpm_list_t as AlpmListPtr -> AlpmList #}

instance Storable AlpmList where
    sizeOf _ = {#sizeof __alpm_list_t #}
    alignment _ = {#alignof __alpm_list_t #}
    peek ptr = do
        d <- {#get __alpm_list_t->data #} ptr
        p <- {#get __alpm_list_t->prev #} ptr
        n <- {#get __alpm_list_t->next #} ptr
        return $ AlpmList d p n
    poke ptr (AlpmList d p n) = do
        {#set __alpm_list_t->data #} ptr d
        {#set __alpm_list_t->prev #} ptr p
        {#set __alpm_list_t->next #} ptr n

-----------------------------------------------
-- Opaque Pointers

{#pointer *alpm_handle_t as AlpmHandle newtype #}

{#pointer *alpm_db_t as AlpmDatabase newtype #}

{#pointer *alpm_pkg_t as AlpmPackage newtype #}

{#pointer *alpm_trans_t as AlpmTransaction newtype #}

----------------------------------------------
-- Pointers

{#pointer *alpm_group_t as AlpmGroup newtype #}

{#pointer *alpm_file_t as AlpmFile newtype #}

{#pointer *alpm_filelist_t as AlpmFilelist newtype #}

{#pointer *alpm_siglist_t as AlpmSiglist newtype #}

{#pointer *alpm_errno_t as AlpmErrnoPtr -> AlpmErrno #}

{#pointer *alpm_conflict_t as AlpmConflict newtype #}

{#pointer *alpm_fileconflict_t as AlpmFileConflict newtype #}

{#pointer *alpm_depend_t as AlpmDependency newtype #}

{#pointer *alpm_depmissing_t as AlpmDepmissing newtype #}
