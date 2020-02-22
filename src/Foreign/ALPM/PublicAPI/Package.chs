module Foreign.ALPM.PublicAPI.Package
       (
       -- Re-exports
         module Foreign.ALPM.PublicAPI.Package.Transaction
       , module Foreign.ALPM.PublicAPI.Package.Dependency

       -- Functions
       , alpmPkgLoad
       , alpmPkgFind
       , alpmPkgFree
       , alpmPkgCheckmd5sum
       , alpmPkgVercmp
       , alpmPkgComputeRequiredby
       , alpmPkgComputeOptionalfor
       , alpmPkgShouldIgnore
       , alpmFilelistContains
       , alpmPkgCheckPgpSignature
       , alpmDbCheckPgpSignature
       , alpmSiglistCleanup
       , alpmDecodeSignature
       , alpmExtractKeyid
       , alpmFindGroupPkgs
       , alpmSyncGetNewVersion

       -- Accessor Functions
       , alpmPkgGetFilename
       , alpmPkgGetBase
       , alpmPkgGetName
       , alpmPkgGetVersion
       , alpmPkgGetOrigin
       , alpmPkgGetDesc
       , alpmPkgGetUrl
       , alpmPkgGetBuilddate
       , alpmPkgGetInstalldate
       , alpmPkgGetPackager
       , alpmPkgGetMd5sum
       , alpmPkgGetSha256sum
       , alpmPkgGetArch
       , alpmPkgGetSize
       , alpmPkgGetIsize
       , alpmPkgGetReason
       , alpmPkgGetLicenses
       , alpmPkgGetGroups
       , alpmPkgGetDepends
       , alpmPkgGetOptdepends
       , alpmPkgGetCheckdepends
       , alpmPkgGetMakedepends
       , alpmPkgGetConflicts
       , alpmPkgGetProvides
       , alpmPkgGetReplaces
       , alpmPkgGetFiles
       , alpmPkgGetBackup
       , alpmPkgGetDb
       , alpmPkgGetBase64Sig
       , alpmPkgGetValidation
       , alpmPkgHasScriptlet
       , alpmPkgDownloadSize
       , alpmPkgSetReason

       -- Transaction Functions
       , alpmSyncSysupgrade
       , alpmAddPkg
       , alpmRemovePkg
       ) where

{# import Foreign.ALPM.Types.Foreign #}
import           Data.Text (Text)
import           Data.Int
import           Data.Word
import           Foreign.ALPM.PublicAPI.Package.Transaction
import           Foreign.ALPM.PublicAPI.Package.Dependency
import           Foreign.ALPM.Internal.Marshal
import           Foreign
import           Foreign.C.Types

#include <alpm.h>


{#fun alpm_pkg_load as ^ { `AlpmHandle'
                         , withCString* `Text'
                         , `Bool'
                         , `AlpmSigLevel'  -- ^ a package sig level
                         , alloca- `AlpmPackage' peek*
                         } -> `Bool' #}
{#fun alpm_pkg_find as ^ { `AlpmListPtr'
                         , withCString* `Text'
                         } -> `AlpmPackage' #}
{#fun alpm_pkg_free as ^ {`AlpmPackage'} -> `Bool' #}
{#fun alpm_pkg_checkmd5sum as ^ {`AlpmPackage'} -> `Bool' #}
{#fun alpm_pkg_vercmp as ^ { withCString* `Text'
                           , withCString* `Text'
                           } -> `Ordering' enumToOrd #}
{#fun alpm_pkg_compute_requiredby as ^ {`AlpmPackage'} -> `AlpmListPtr' #}
{#fun alpm_pkg_compute_optionalfor as ^ {`AlpmPackage'} -> `AlpmListPtr' #}
{#fun alpm_pkg_should_ignore as ^ {`AlpmHandle', `AlpmPackage'} -> `Bool' #}
{#fun alpm_filelist_contains as ^ {`AlpmFilelist', withCString* `Text'} -> `AlpmFilePtr' #}
{#fun alpm_pkg_check_pgp_signature as ^ {`AlpmPackage', `AlpmSiglist'} -> `Int' #}
{#fun alpm_db_check_pgp_signature as ^ {`AlpmDatabase', `AlpmSiglist'} -> `Int' #}
{#fun alpm_siglist_cleanup as ^ {`AlpmSiglist'} -> `Bool' #}
{#fun alpm_decode_signature as ^ { withCString* `Text'
                                 , alloca- `Ptr CUChar' peek*
                                 , alloca- `CULong' peek*
                                 } -> `Bool' #}
{#fun alpm_extract_keyid as ^ { `AlpmHandle'
                              , withCString* `Text'
                              , id `Ptr CUChar'
                              , `Word64'
                              , alloca- `AlpmListPtr' peek*
                              } -> `Bool' #}
{#fun alpm_find_group_pkgs as ^ {`AlpmListPtr', withCString* `Text'} -> `AlpmListPtr' #}
{#fun alpm_sync_get_new_version as ^ {`AlpmPackage', `AlpmListPtr'} -> `AlpmPackage' #}


-------------------------------------
-- Property Accessor

{#fun alpm_pkg_get_filename as ^ {`AlpmPackage'} -> `Text' peekCString* #}
{#fun alpm_pkg_get_base as ^ {`AlpmPackage'} -> `Text' peekCString* #}
{#fun alpm_pkg_get_name as ^ {`AlpmPackage'} -> `Text' peekCString* #}
{#fun alpm_pkg_get_version as ^ {`AlpmPackage'} -> `Text' peekCString* #}
{#fun alpm_pkg_get_origin as ^ {`AlpmPackage'} -> `Int' #}
{#fun alpm_pkg_get_desc as ^ {`AlpmPackage'} -> `Text' peekCString* #}
{#fun alpm_pkg_get_url as ^ {`AlpmPackage'} -> `Text' peekCString* #}
{#fun alpm_pkg_get_builddate as ^ {`AlpmPackage'} -> `Int' #}
{#fun alpm_pkg_get_installdate as ^ {`AlpmPackage'} -> `Int' #}
{#fun alpm_pkg_get_packager as ^ {`AlpmPackage'} -> `Text' peekCString* #}
{#fun alpm_pkg_get_md5sum as ^ {`AlpmPackage'} -> `Text' peekCString* #}
{#fun alpm_pkg_get_sha256sum as ^ {`AlpmPackage'} -> `Text' peekCString* #}
{#fun alpm_pkg_get_arch as ^ {`AlpmPackage'} -> `Text' peekCString* #}
{#fun alpm_pkg_get_size as ^ {`AlpmPackage'} -> `Int64' #}
{#fun alpm_pkg_get_isize as ^ {`AlpmPackage'} -> `Int64' #}
{#fun alpm_pkg_get_reason as ^ {`AlpmPackage'} -> `AlpmPkgReason' #}
{#fun alpm_pkg_get_licenses as ^ {`AlpmPackage'} -> `AlpmListPtr' #}
{#fun alpm_pkg_get_groups as ^ {`AlpmPackage'} -> `AlpmListPtr' #}
{#fun alpm_pkg_get_depends as ^ {`AlpmPackage'} -> `AlpmListPtr' #}
{#fun alpm_pkg_get_optdepends as ^ {`AlpmPackage'} -> `AlpmListPtr' #}
{#fun alpm_pkg_get_checkdepends as ^ {`AlpmPackage'} -> `AlpmListPtr' #}
{#fun alpm_pkg_get_makedepends as ^ {`AlpmPackage'} -> `AlpmListPtr' #}
{#fun alpm_pkg_get_conflicts as ^ {`AlpmPackage'} -> `AlpmListPtr' #}
{#fun alpm_pkg_get_provides as ^ {`AlpmPackage'} -> `AlpmListPtr' #}
{#fun alpm_pkg_get_replaces as ^ {`AlpmPackage'} -> `AlpmListPtr' #}
{#fun alpm_pkg_get_files as ^ {`AlpmPackage'} -> `AlpmFilelist' #}
{#fun alpm_pkg_get_backup as ^ {`AlpmPackage'} -> `AlpmListPtr' #}
{#fun alpm_pkg_get_db as ^ {`AlpmPackage'} -> `AlpmDatabase' #}
{#fun alpm_pkg_get_base64_sig as ^ {`AlpmPackage'} -> `Text' peekCString* #}
{#fun alpm_pkg_get_validation as ^ {`AlpmPackage'} -> `AlpmPkgValidation' #}
-- changelog and mtree related functions were not imported
{#fun alpm_pkg_has_scriptlet as ^ {`AlpmPackage'} -> `Bool' #}
{#fun alpm_pkg_download_size as ^ {`AlpmPackage'} -> `Int64' #}
{#fun alpm_pkg_set_reason as ^ {`AlpmPackage', `AlpmPkgReason'} -> `Bool' #}


------------------------------------------
-- Common Transactions

{#fun alpm_sync_sysupgrade as ^ {`AlpmHandle', `Bool'} -> `Bool' #}
{#fun alpm_add_pkg as ^ {`AlpmHandle', `AlpmPackage'} -> `Bool' #}
{#fun alpm_remove_pkg as ^ {`AlpmHandle', `AlpmPackage'} -> `Bool' #}
