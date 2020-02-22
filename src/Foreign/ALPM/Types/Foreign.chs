{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Foreign.ALPM.Types.Foreign where

import           Data.Coerce
import           Foreign.Ptr
import           Foreign.Storable
import           Foreign.ALPM.Internal.TH
import           Foreign.ALPM.Internal.GStorableInstances
import           Foreign.C.Types
import           Foreign.C.String
import           Foreign.Storable.Generic
import           GHC.Generics

#include <alpm.h>
#include <alpm_list.h>


{#pointer *alpm_errno_t as AlpmErrnoPtr -> AlpmErrno #}

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
deriveGStorable ''AlpmHandle

{#pointer *alpm_db_t as AlpmDatabase newtype #}

deriving instance Storable AlpmDatabase
deriveGStorable ''AlpmDatabase

{#pointer *alpm_pkg_t as AlpmPackage newtype #}

deriving instance Storable AlpmPackage
deriveGStorable ''AlpmPackage

{#pointer *alpm_trans_t as AlpmTransaction newtype #}

deriving instance Storable AlpmTransaction
deriveGStorable ''AlpmTransaction

----------------------------------------------
-- Pointers

data AlpmHList
    = AlpmHList
      { payload :: Ptr ()
      , prev :: AlpmListPtr
      , next :: AlpmListPtr
      }
    deriving Generic

instance GStorable AlpmHList

{#pointer *alpm_list_t as AlpmListPtr -> AlpmHList #}

data AlpmGroup
    = AlpmGroup
      { name :: CString
      , packages :: AlpmListPtr
      }
    deriving Generic

instance GStorable AlpmGroup

{#pointer *alpm_group_t as AlpmGroupPtr -> AlpmGroup #}

data AlpmFile
    = AlpmFile
      { name :: CString
      , size :: CLong
      , mode :: CUInt
      }
    deriving Generic

instance GStorable AlpmFile

{#pointer *alpm_file_t as AlpmFilePtr -> AlpmFile #}

data AlpmFilelist
    = AlpmFilelist
      { count  :: CSize
      , files  :: AlpmFilePtr
      }
    deriving Generic

instance GStorable AlpmFilelist

{#pointer *alpm_filelist_t as AlpmFilelistPtr -> AlpmFilelist #}

data AlpmPgpkey
    = AlpmPgpkey
      { payload :: Ptr ()
      , fingerprint :: CString
      , uid :: CString
      , name :: CString
      , email :: CString
      , created :: CLong
      , expires :: CLong
      , length :: CUInt
      , revoked :: CUInt
      , pubkeyAlgo :: CChar
      }
    deriving Generic

instance GStorable AlpmPgpkey

{#pointer *alpm_pgpkey_t as AlpmPgpkeyPtr -> AlpmPgpkey #}

data AlpmSigResult
    = AlpmSigResult
      { key :: AlpmPgpkey
      , status :: AlpmSigStatus
      , validity :: AlpmSigValidity
      }
    deriving Generic

instance GStorable AlpmSigResult

{#pointer *alpm_sigresult_t as AlpmSigResultPtr -> AlpmSigResult #}

data AlpmSiglist
    = AlpmSiglist
      { count :: CSize
      , results :: AlpmSigResultPtr
      }
    deriving Generic

instance GStorable AlpmSiglist

{#pointer *alpm_siglist_t as AlpmSiglistPtr
 foreign finalizer alpm_siglist_cleanup -> AlpmSiglist #}

data AlpmConflict
    = AlpmConflict
      { package1Hash :: CULong
      , package2Hash :: CULong
      , package1 :: CString
      , package2 :: CString
      , reason :: AlpmDependency
      }
    deriving Generic

instance GStorable AlpmConflict

{#pointer *alpm_conflict_t as AlpmConflictPtr
 foreign finalizer alpm_conflict_free -> AlpmConflict #}

data AlpmFileConflict
    = AlpmFileConflict
      { target :: CString
      , conflictType :: AlpmFileConflictType
      , file :: CString
      , ctarget :: CString
      }
    deriving Generic

instance GStorable AlpmFileConflict

{#pointer *alpm_fileconflict_t as AlpmFileConflictPtr
 foreign finalizer alpm_fileconflict_free -> AlpmFileConflict #}

data AlpmDependency
    = AlpmDependency
      { name :: CString
      , version :: CString
      , desc :: CString
      , nameHash :: CULong
      , depmod :: AlpmDepmod
      }
    deriving Generic

instance GStorable AlpmDependency

{#pointer *alpm_depend_t as AlpmDependencyPtr -> AlpmDependency #}

data AlpmDepmissing
    = AlpmDepmissing
      { target :: CString
      , depend :: AlpmDependencyPtr
      , causingPkg :: CString
      }
    deriving Generic

instance GStorable AlpmDepmissing

{#pointer *alpm_depmissing_t as AlpmDepmissingPtr
 foreign finalizer alpm_depmissing_free -> AlpmDepmissing #}

---------------------------------------------------
-- Synonyms

-- | Frees the value pointed by a void pointer
type FreeFunc = Ptr () -> IO ()
