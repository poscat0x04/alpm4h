{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
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

{#enum alpm_hook_when_t as AlpmHookWhen {underscoreToCase} deriving (Show, Eq, Ord) #}

deriveStorable ''AlpmHookWhen
deriveGStorable ''AlpmHookWhen

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

data AlpmBackup
    = AlpmBackup
      { name :: CString
      , hash :: CString
      }
    deriving Generic

instance GStorable AlpmBackup

{#pointer *alpm_backup_t as AlpmBackupPtr -> AlpmBackup #}

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

data AlpmEvent
    = AlpmPkgOp
      { event :: AlpmEventType
      , op :: AlpmPackageOperation
      , oldpkg :: AlpmPackage
      , newpkg :: AlpmPackage
      }
    | AlpmOptdepRemoval
      { pkg :: AlpmPackage
      , optdep :: AlpmDependencyPtr
      }
    | AlpmScriptlet
      { line :: CString
      }
    | AlpmDBMissing
      { dbname :: CString
      }
    | AlpmPkgDownload
      { event :: AlpmEventType
      , file :: CString
      }
    | AlpmPacnewCreated
      { fromNoUpgrade :: CInt
      , oldPkg :: AlpmPackage
      , newPkg :: AlpmPackage
      , file :: CString
      }
    | AlpmPacsaveCreated
      { oldpkg :: AlpmPackage
      , file :: CString
      }
    | AlpmEventHook
      { event :: AlpmEventType
      , triggertime :: AlpmHookWhen
      }
    | AlpmEventHookRun
      { event :: AlpmEventType
      , name :: CString
      , desc :: CString
      , position :: CULong
      , total :: CULong
      }
    | Other
      { event :: AlpmEventType
      }

instance Storable AlpmEvent where
    sizeOf = const {#sizeof alpm_event_t #}
    alignment = const {#alignof alpm_event_t #}
    peek ptr = do
        tag <- {#get alpm_event_t->type #} ptr
        let eventType = ((toEnum $ fromEnum tag) :: AlpmEventType)
        if | (||) <$> (== AlpmEventHookRunStart) <*> (== AlpmEventHookRunDone) $ eventType
             -> (AlpmEventHookRun eventType) <$>
                 {#get alpm_event_hook_run_t->name#} ptr <*>
                 {#get alpm_event_hook_run_t->desc#} ptr <*>
                 {#get alpm_event_hook_run_t->position#} ptr <*>
                 {#get alpm_event_hook_run_t->total #} ptr
           | (||) <$> (== AlpmEventHookStart) <*> (== AlpmEventHookDone) $ eventType
             ->  do
               i <- {#get alpm_event_hook_t -> when#} ptr
               return $ AlpmEventHook eventType (toEnum $ fromEnum i)
           | eventType == AlpmEventPacsaveCreated
             ->  AlpmPacsaveCreated
             <$> {#get alpm_event_pacsave_created_t -> oldpkg #} ptr
             <*> {#get alpm_event_pacsave_created_t -> file #} ptr
           | eventType == AlpmEventPacnewCreated
             ->  AlpmPacnewCreated
             <$> {#get alpm_event_pacnew_created_t -> from_noupgrade #} ptr
             <*> {#get alpm_event_pacnew_created_t -> oldpkg #} ptr
             <*> {#get alpm_event_pacnew_created_t -> newpkg #} ptr
             <*> {#get alpm_event_pacnew_created_t -> file #} ptr
           | (||) <$> (== AlpmEventPkgdownloadStart) <*> (== AlpmEventPkgdownloadDone) $ eventType
             ->  (AlpmPkgDownload eventType)
             <$> {#get alpm_event_pkgdownload_t -> file #} ptr
           | eventType == AlpmEventDatabaseMissing
             ->  AlpmDBMissing
             <$> {#get alpm_event_database_missing_t -> dbname #} ptr
           | eventType == AlpmEventScriptletInfo
             ->  AlpmScriptlet
             <$> {#get alpm_event_scriptlet_info_t -> line #} ptr
           | eventType == AlpmEventOptdepRemoval
             ->  AlpmOptdepRemoval
             <$> {#get alpm_event_optdep_removal_t -> pkg #} ptr
             <*> {#get alpm_event_optdep_removal_t -> optdep #} ptr
           | (||) <$> (== AlpmEventPackageOperationStart) <*> (== AlpmEventPackageOperationDone) $ eventType
             ->  (AlpmPkgOp eventType)
             <$> (fmap (toEnum . fromEnum) $ {#get alpm_event_package_operation_t -> operation #} ptr)
             <*> {#get alpm_event_package_operation_t -> oldpkg #} ptr
             <*> {#get alpm_event_package_operation_t -> newpkg #} ptr
           | otherwise -> return (Other eventType)
    poke ptr a =
      case a of
        (AlpmPkgOp e op old new ) -> do
          {#set alpm_event_package_operation_t -> type #} ptr (from e)
          {#set alpm_event_package_operation_t -> operation #} ptr (from op)
          {#set alpm_event_package_operation_t -> oldpkg #} ptr old
          {#set alpm_event_package_operation_t -> newpkg #} ptr new
        (AlpmOptdepRemoval pkg optdep) -> do
          {#set alpm_event_optdep_removal_t -> type #} ptr (from AlpmEventOptdepRemoval)
          {#set alpm_event_optdep_removal_t -> pkg #} ptr pkg
          {#set alpm_event_optdep_removal_t -> optdep #} ptr optdep
        (AlpmScriptlet line) -> do
          {#set alpm_event_scriptlet_info_t -> type #} ptr (from AlpmEventScriptletInfo)
          {#set alpm_event_scriptlet_info_t -> line #} ptr line
        (AlpmDBMissing dbname) -> do
          {#set alpm_event_database_missing_t -> type #} ptr (from AlpmEventDatabaseMissing)
          {#set alpm_event_database_missing_t -> dbname #} ptr dbname
        (AlpmPkgDownload e file) -> do
          {#set alpm_event_pkgdownload_t -> type #} ptr (from e)
          {#set alpm_event_pkgdownload_t -> file #} ptr file
        (AlpmPacnewCreated noup oldpkg newpkg file) -> do
          {#set alpm_event_pacnew_created_t -> type #} ptr (from AlpmEventPacnewCreated)
          {#set alpm_event_pacnew_created_t -> from_noupgrade #} ptr noup
          {#set alpm_event_pacnew_created_t -> oldpkg #} ptr oldpkg
          {#set alpm_event_pacnew_created_t -> newpkg #} ptr newpkg
          {#set alpm_event_pacnew_created_t -> file #} ptr file
        (AlpmPacsaveCreated oldpkg file) -> do
          {#set alpm_event_pacsave_created_t -> type #} ptr (from AlpmEventPacsaveCreated)
          {#set alpm_event_pacsave_created_t -> oldpkg #} ptr oldpkg
          {#set alpm_event_pacsave_created_t -> file #} ptr file
        (AlpmEventHook e triggertime) -> do
          {#set alpm_event_hook_t -> type #} ptr (from e)
          {#set alpm_event_hook_t -> when #} ptr (from triggertime)
        (AlpmEventHookRun e name desc pos total) -> do
          {#set alpm_event_hook_run_t -> type #} ptr (from e)
          {#set alpm_event_hook_run_t -> name #} ptr name
          {#set alpm_event_hook_run_t -> desc #} ptr desc
          {#set alpm_event_hook_run_t -> position #} ptr pos
          {#set alpm_event_hook_run_t -> total #} ptr total
        (Other e) -> do
          {#set alpm_event_any_t -> type #} ptr (from e)
      where
        from :: (Enum b, Enum a) => a -> b
        from = toEnum . fromEnum


{#pointer *alpm_event_t as AlpmEventPtr -> AlpmEvent #}

data AlpmQuestion
    = AlpmInstallIgnorepkg
      { install :: CInt
      , pkg :: AlpmPackage
      }
    | AlpmReplace
      { replace :: CInt
      , oldpkg :: AlpmPackage
      , newpkg :: AlpmPackage
      , newdb :: AlpmDatabase
      }
    | AlpmConflictPkg
      { remove :: CInt
      , conflict :: (Ptr AlpmConflict)
      }
    | AlpmCorrupted
      { remove :: CInt
      , filePath :: CString
      , reason :: AlpmErrno
      }
    | AlpmRemovePkgs
      { skip :: CInt
      , packages :: AlpmListPtr
      }
    | AlpmSelectProvider
      { index :: CInt
      , providers :: AlpmListPtr
      , depend :: AlpmDependencyPtr
      }
    | AlpmImportKey
      { imp :: CInt
      , key :: AlpmPgpkeyPtr
      }

instance Storable AlpmQuestion where
    sizeOf = const {#sizeof alpm_question_t #}
    alignment = const {#alignof alpm_question_t #}
    peek ptr = do
        tag <- {#get alpm_question_t -> type #} ptr
        let qtype = toEnum $ fromEnum tag
        let from = toEnum . fromEnum
        if | qtype == AlpmQuestionInstallIgnorepkg
             -> AlpmInstallIgnorepkg
            <$> {#get alpm_question_install_ignorepkg_t -> install #} ptr
            <*> {#get alpm_question_install_ignorepkg_t -> pkg #} ptr
           | qtype == AlpmQuestionReplacePkg
             -> AlpmReplace
            <$> {#get alpm_question_replace_t -> replace #} ptr
            <*> {#get alpm_question_replace_t -> oldpkg #} ptr
            <*> {#get alpm_question_replace_t -> newpkg #} ptr
            <*> {#get alpm_question_replace_t -> newdb #} ptr
           | qtype == AlpmQuestionConflictPkg
             -> AlpmConflictPkg
            <$> {#get alpm_question_conflict_t -> remove #} ptr
            <*> {#get alpm_question_conflict_t -> conflict #} ptr
           | qtype == AlpmQuestionCorruptedPkg
             -> AlpmCorrupted
            <$> {#get alpm_question_corrupted_t -> remove #} ptr
            <*> {#get alpm_question_corrupted_t -> filepath #} ptr
            <*> (fmap from $ {#get alpm_question_corrupted_t -> reason #} ptr)
           | qtype == AlpmQuestionRemovePkgs
             -> AlpmRemovePkgs
            <$> {#get alpm_question_remove_pkgs_t -> skip #} ptr
            <*> {#get alpm_question_remove_pkgs_t -> packages #} ptr
           | qtype == AlpmQuestionSelectProvider
             -> AlpmSelectProvider
            <$> {#get alpm_question_select_provider_t -> use_index #} ptr
            <*> {#get alpm_question_select_provider_t -> providers #} ptr
            <*> {#get alpm_question_select_provider_t -> depend #} ptr
           | qtype == AlpmQuestionImportKey
             -> AlpmImportKey
            <$> {#get alpm_question_import_key_t -> import #} ptr
            <*> {#get alpm_question_import_key_t -> key #} ptr
    poke ptr q = case q of
      AlpmInstallIgnorepkg{..} -> do
        {#set alpm_question_install_ignorepkg_t -> type #} ptr (from AlpmQuestionInstallIgnorepkg)
        {#set alpm_question_install_ignorepkg_t -> install #} ptr install
        {#set alpm_question_install_ignorepkg_t -> pkg #} ptr pkg
      AlpmReplace{..} -> do
        {#set alpm_question_replace_t -> type #} ptr (from AlpmQuestionReplacePkg)
        {#set alpm_question_replace_t -> replace #} ptr replace
        {#set alpm_question_replace_t -> oldpkg #} ptr oldpkg
        {#set alpm_question_replace_t -> newpkg #} ptr newpkg
        {#set alpm_question_replace_t -> newdb #} ptr newdb
      AlpmConflictPkg{..} -> do
        {#set alpm_question_conflict_t -> type #} ptr (from AlpmQuestionConflictPkg)
        {#set alpm_question_conflict_t -> remove #} ptr remove
        {#set alpm_question_conflict_t -> conflict #} ptr conflict
      AlpmCorrupted{..} -> do
        {#set alpm_question_corrupted_t -> type #} ptr (from AlpmQuestionCorruptedPkg)
        {#set alpm_question_corrupted_t -> remove #} ptr remove
        {#set alpm_question_corrupted_t -> filepath #} ptr filePath
        {#set alpm_question_corrupted_t -> reason #} ptr (from reason)
      AlpmRemovePkgs{..} -> do
        {#set alpm_question_remove_pkgs_t -> type #} ptr (from AlpmQuestionRemovePkgs)
        {#set alpm_question_remove_pkgs_t -> skip #} ptr skip
        {#set alpm_question_remove_pkgs_t -> packages #} ptr packages
      AlpmSelectProvider{..} -> do
        {#set alpm_question_select_provider_t -> type #} ptr (from AlpmQuestionSelectProvider)
        {#set alpm_question_select_provider_t -> use_index #} ptr index
        {#set alpm_question_select_provider_t -> providers #} ptr providers
        {#set alpm_question_select_provider_t -> depend #} ptr depend
      AlpmImportKey{..} -> do
        {#set alpm_question_import_key_t -> type #} ptr (from AlpmQuestionImportKey)
        {#set alpm_question_import_key_t -> import #} ptr imp
        {#set alpm_question_import_key_t -> key #} ptr key
      where
        from = toEnum . fromEnum

{#pointer *alpm_question_t as AlpmQuestionPtr -> AlpmQuestion #}

---------------------------------------------------
-- Synonyms

-- | Frees the value pointed by a void pointer
type FreeFunc = Ptr () -> IO ()

type LogCbFun = CInt -> CString -> Ptr () -> IO ()
type LogCb = FunPtr LogCbFun

type DownloadCbFun = CString -> CLong -> CLong -> IO ()
type DownloadCb = FunPtr DownloadCbFun

type FetchCbFun = CString -> CString -> CInt -> IO CInt
type FetchCb = FunPtr FetchCbFun

type TotalDlCbFun = CLong -> IO ()
type TotalDlCb = FunPtr TotalDlCbFun

type EventCbFun = AlpmEventPtr -> IO ()
type EventCb = FunPtr EventCbFun

type QuestionCbFun = AlpmQuestionPtr -> IO ()
type QuestionCb = FunPtr QuestionCbFun

type ProgressCbFun = CInt -> CString -> CInt -> CULong -> CULong -> IO ()
type ProgressCb = FunPtr ProgressCbFun

type Sig = Ptr CUChar
type SigLen = CULong
