{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Foreign.ALPM.Types.Foreign where

import           Data.Coerce
import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.ALPM.Internal.TH
import           Foreign.C.Types
import           Foreign.C.String
import           Foreign.Storable.Generic
import           GHC.Generics

#include <alpm.h>
#include <alpm_list.h>


{#enum alpm_errno_t as AlpmErrno {underscoreToCase} deriving (Show, Eq, Ord) #}

deriveStorable ''AlpmErrno
deriveGStorable ''AlpmErrno

{#enum alpm_transflag_t as AlpmTransFlag {underscoreToCase} deriving (Show, Eq, Ord) #}

deriveStorable ''AlpmTransFlag
deriveGStorable ''AlpmTransFlag

{#enum alpm_loglevel_t as AlpmLogLevel {underscoreToCase} deriving (Show, Eq, Ord) #}

deriveStorable ''AlpmLogLevel
deriveGStorable ''AlpmLogLevel

{#enum alpm_pkgreason_t as AlpmPkgReason {underscoreToCase} deriving (Show, Eq, Ord) #}

deriveStorable ''AlpmPkgReason
deriveGStorable ''AlpmPkgReason

{#enum alpm_pkgfrom_t as AlpmPkgFrom {underscoreToCase} deriving (Show, Eq, Ord) #}

deriveStorable ''AlpmPkgFrom
deriveGStorable ''AlpmPkgFrom

{#enum alpm_pkgvalidation_t as AlpmPkgValidation {underscoreToCase} deriving (Show, Eq, Ord) #}

deriveStorable ''AlpmPkgValidation
deriveGStorable ''AlpmPkgValidation

{#enum alpm_depmod_t as AlpmDepmod {underscoreToCase} deriving (Show, Eq, Ord) #}

deriveStorable ''AlpmDepmod
deriveGStorable ''AlpmDepmod

{#enum alpm_fileconflicttype_t as AlpmFileConflictType {underscoreToCase} deriving (Show, Eq, Ord) #}

deriveStorable ''AlpmFileConflictType
deriveGStorable ''AlpmFileConflictType

{#enum alpm_siglevel_t as AlpmSigLevel {underscoreToCase} deriving (Show, Eq, Ord) #}

deriveStorable ''AlpmSigLevel
deriveGStorable ''AlpmSigLevel

{#enum alpm_sigstatus_t as AlpmSigStatus {underscoreToCase} deriving (Show, Eq, Ord) #}

deriveStorable ''AlpmSigStatus
deriveGStorable ''AlpmSigStatus

{#enum alpm_sigvalidity_t as AlpmSigValidity {underscoreToCase} deriving (Show, Eq, Ord) #}

deriveStorable ''AlpmSigValidity
deriveGStorable ''AlpmSigValidity

{#enum alpm_event_type_t as AlpmEventType {underscoreToCase} deriving (Show, Eq, Ord) #}

deriveStorable ''AlpmEventType
deriveGStorable ''AlpmEventType

{#enum alpm_package_operation_t as AlpmPackageOperation {underscoreToCase} deriving (Show, Eq, Ord) #}

deriveStorable ''AlpmPackageOperation
deriveGStorable ''AlpmPackageOperation

{#enum alpm_question_type_t as AlpmQuestionType {underscoreToCase} deriving (Show, Eq, Ord) #}

deriveStorable ''AlpmQuestionType
deriveGStorable ''AlpmQuestionType

{#enum alpm_progress_t as AlpmProgress {underscoreToCase} deriving (Show, Eq, Ord) #}

deriveStorable ''AlpmProgress
deriveGStorable ''AlpmProgress

{#enum alpm_db_usage_t as AlpmDBUsage {underscoreToCase} deriving (Show, Eq, Ord) #}

deriveStorable ''AlpmDBUsage
deriveGStorable ''AlpmDBUsage

{#enum alpm_caps as AlpmCaps {underscoreToCase} deriving (Show, Eq, Ord) #}

deriveStorable ''AlpmCaps
deriveGStorable ''AlpmCaps

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

data AlpmGroup
    = AlpmGroup
      { groupName :: CString
      , groupPackages :: AlpmListPtr
      }
    deriving Generic

{#pointer *alpm_group_t as AlpmGroupPtr -> AlpmGroup #}

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

---------------------------------------------------
-- Synonyms

-- | Frees the value pointed by a void pointer
type FreeFunc = Ptr () -> IO ()
