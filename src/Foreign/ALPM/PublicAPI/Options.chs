module Foreign.ALPM.PublicAPI.Options
       ( alpmOptionGetRoot
       , alpmOptionGetDbpath
       , alpmOptionGetLockfile
       , alpmOptionGetOverwriteFiles
       , alpmOptionSetOverwriteFiles
       , alpmOptionAddOverwriteFile
       , alpmOptionGetLogfile
       , alpmOptionSetLogfile
       , alpmOptionGetGpgdir
       , alpmOptionSetGpgdir
       , alpmOptionGetUsesyslog
       , alpmOptionSetUsesyslog
       , alpmOptionGetArch
       , alpmOptionSetArch
       , alpmOptionGetCheckspace
       , alpmOptionSetCheckspace
       , alpmOptionGetDbext
       , alpmOptionSetDbext
       , alpmOptionGetDefaultSiglevel
       , alpmOptionSetDefaultSiglevel
       , alpmOptionGetRemoteFileSiglevel
       , alpmOptionSetRemoteFileSiglevel
       , alpmOptionSetDisableDlTimeout
       , alpmOptionGetCachedirs
       , alpmOptionSetCachedirs
       , alpmOptionAddCachedir
       , alpmOptionRemoveCachedir
       , alpmOptionGetHookdirs
       , alpmOptionSetHookdirs
       , alpmOptionAddHookdir
       , alpmOptionRemoveHookdir
       , alpmOptionGetNoupgrades
       , alpmOptionSetNoupgrades
       , alpmOptionAddNoupgrade
       , alpmOptionRemoveNoupgrade
       , alpmOptionMatchNoupgrade
       , alpmOptionGetNoextracts
       , alpmOptionSetNoextracts
       , alpmOptionAddNoextract
       , alpmOptionRemoveNoextract
       , alpmOptionMatchNoextract
       , alpmOptionGetIgnorepkgs
       , alpmOptionSetIgnorepkgs
       , alpmOptionAddIgnorepkg
       , alpmOptionRemoveIgnorepkg
       , alpmOptionGetIgnoregroups
       , alpmOptionSetIgnoregroups
       , alpmOptionAddIgnoregroup
       , alpmOptionRemoveIgnoregroup
       , alpmOptionGetAssumeinstalled
       , alpmOptionSetAssumeinstalled
       , alpmOptionAddAssumeinstalled
       , alpmOptionRemoveAssumeinstalled

       , alpmOptionGetLogcb
       , alpmOptionSetLogcb
       , alpmOptionGetDlcb
       , alpmOptionSetDlcb
       , alpmOptionGetFetchcb
       , alpmOptionSetFetchcb
       , alpmOptionGetTotaldlcb
       , alpmOptionSetTotaldlcb
       , alpmOptionGetEventcb
       , alpmOptionSetEventcb
       , alpmOptionGetQuestioncb
       , alpmOptionSetQuestioncb
       , alpmOptionGetProgresscb
       , alpmOptionSetProgresscb
       ) where

{# import Foreign.ALPM.Types.Foreign #}
import           Foreign.ALPM.Internal.Marshal
import           Data.Text (Text)

#include <alpm.h>


{#fun alpm_option_get_logcb as ^ {`AlpmHandle'} -> `LogCb' id #}
{#fun alpm_option_set_logcb as ^ {`AlpmHandle'
                                 , id `LogCb'} -> `Bool' #}
{#fun alpm_option_get_dlcb as ^ {`AlpmHandle'} -> `DownloadCb' id #}
{#fun alpm_option_set_dlcb as ^ { `AlpmHandle'
                                , id `DownloadCb'
                                } -> `Bool' #}
{#fun alpm_option_get_fetchcb as ^ {`AlpmHandle'} -> `FetchCb' id #}
{#fun alpm_option_set_fetchcb as ^ { `AlpmHandle'
                                   , id `FetchCb'
                                   } -> `Bool' #}
{#fun alpm_option_get_totaldlcb as ^ {`AlpmHandle'} -> `TotalDlCb' id #}
{#fun alpm_option_set_totaldlcb as ^ { `AlpmHandle'
                                   , id `TotalDlCb'
                                   } -> `Bool' #}
{#fun alpm_option_get_eventcb as ^ {`AlpmHandle'} -> `EventCb' id #}
{#fun alpm_option_set_eventcb as ^ { `AlpmHandle'
                                   , id `EventCb'
                                   } -> `Bool' #}
{#fun alpm_option_get_questioncb as ^ {`AlpmHandle'} -> `QuestionCb' id #}
{#fun alpm_option_set_questioncb as ^ { `AlpmHandle'
                                      , id `QuestionCb'
                                      } -> `Bool' #}
{#fun alpm_option_get_progresscb as ^ {`AlpmHandle'} -> `ProgressCb' id #}
{#fun alpm_option_set_progresscb as ^ { `AlpmHandle'
                                      , id `ProgressCb'
                                      } -> `Bool' #}
{#fun alpm_option_get_root as ^ {`AlpmHandle'} -> `Text' peekCString* #}
{#fun alpm_option_get_dbpath as ^ {`AlpmHandle'} -> `Text' peekCString* #}
{#fun alpm_option_get_lockfile as ^ {`AlpmHandle'} -> `Text' peekCString* #}
{#fun alpm_option_get_overwrite_files as ^ {`AlpmHandle'} -> `AlpmListPtr' #}
{#fun alpm_option_set_overwrite_files as ^ {`AlpmHandle', `AlpmListPtr'} -> `Bool' #}
{#fun alpm_option_add_overwrite_file as ^ { `AlpmHandle'
                                           , withCString* `Text'} -> `Bool' #}
{#fun alpm_option_get_logfile as ^ {`AlpmHandle'} -> `Text' peekCString* #}
{#fun alpm_option_set_logfile as ^ { `AlpmHandle'
                                   , withCString* `Text'} -> `Bool' #}
{#fun alpm_option_get_gpgdir as ^ {`AlpmHandle'} -> `Text' peekCString* #}
{#fun alpm_option_set_gpgdir as ^ { `AlpmHandle'
                                  , withCString* `Text'} -> `Bool' #}
{#fun alpm_option_get_usesyslog as ^ {`AlpmHandle'} -> `Bool' #}
{#fun alpm_option_set_usesyslog as ^ {`AlpmHandle', `Bool'} -> `Bool' #}
{#fun alpm_option_get_arch as ^ {`AlpmHandle'} -> `Text' peekCString* #}
{#fun alpm_option_set_arch as ^ { `AlpmHandle'
                                , withCString* `Text'} -> `Bool' #}
{#fun alpm_option_get_checkspace as ^ {`AlpmHandle'} -> `Int' #}
{#fun alpm_option_set_checkspace as ^ {`AlpmHandle', `Int'} -> `Bool' #}
{#fun alpm_option_get_dbext as ^ {`AlpmHandle'} -> `Text' peekCString* #}
{#fun alpm_option_set_dbext as ^ { `AlpmHandle'
                                 , withCString* `Text'} -> `Bool' #}
{#fun alpm_option_get_default_siglevel as ^ {`AlpmHandle'} -> `AlpmSigLevel' #}
{#fun alpm_option_set_default_siglevel as ^ {`AlpmHandle', `AlpmSigLevel'} -> `Bool' #}
{#fun alpm_option_get_remote_file_siglevel as ^ {`AlpmHandle'} -> `AlpmSigLevel' #}
{#fun alpm_option_set_remote_file_siglevel as ^ {`AlpmHandle', `AlpmSigLevel'} -> `Bool' #}
{#fun alpm_option_set_disable_dl_timeout as ^ {`AlpmHandle', `Bool'} -> `Bool' #}

{#fun alpm_option_get_cachedirs as ^ {`AlpmHandle'} -> `AlpmListPtr' #}
{#fun alpm_option_set_cachedirs as ^ {`AlpmHandle', `AlpmListPtr'} -> `Bool' #}
{#fun alpm_option_add_cachedir as ^ { `AlpmHandle'
                                    , withCString* `Text'} -> `Bool' #}
{#fun alpm_option_remove_cachedir as ^ { `AlpmHandle'
                                       , withCString* `Text'} -> `Bool' #}

{#fun alpm_option_get_hookdirs as ^ {`AlpmHandle'} -> `AlpmListPtr' #}
{#fun alpm_option_set_hookdirs as ^ {`AlpmHandle', `AlpmListPtr'} -> `Bool' #}
{#fun alpm_option_add_hookdir as ^ { `AlpmHandle'
                                   , withCString* `Text'} -> `Bool' #}
{#fun alpm_option_remove_hookdir as ^ { `AlpmHandle'
                                      , withCString* `Text'} -> `Bool' #}

{#fun alpm_option_get_noupgrades as ^ {`AlpmHandle'} -> `AlpmListPtr' #}
{#fun alpm_option_set_noupgrades as ^ {`AlpmHandle', `AlpmListPtr'} -> `Bool' #}
{#fun alpm_option_add_noupgrade as ^ { `AlpmHandle'
                                     , withCString* `Text'} -> `Bool' #}
{#fun alpm_option_remove_noupgrade as ^ { `AlpmHandle'
                                        , withCString* `Text'} -> `Bool' #}
{#fun alpm_option_match_noupgrade as ^ { `AlpmHandle'
                                       , withCString* `Text'} -> `Bool' #}

{#fun alpm_option_get_noextracts as ^ {`AlpmHandle'} -> `AlpmListPtr' #}
{#fun alpm_option_set_noextracts as ^ {`AlpmHandle', `AlpmListPtr'} -> `Bool' #}
{#fun alpm_option_add_noextract as ^ { `AlpmHandle'
                                     , withCString* `Text'} -> `Bool' #}
{#fun alpm_option_remove_noextract as ^ { `AlpmHandle'
                                        , withCString* `Text'} -> `Bool' #}
{#fun alpm_option_match_noextract as ^ { `AlpmHandle'
                                       , withCString* `Text'} -> `Bool' #}

{#fun alpm_option_get_ignorepkgs as ^ {`AlpmHandle'} -> `AlpmListPtr' #}
{#fun alpm_option_set_ignorepkgs as ^ {`AlpmHandle', `AlpmListPtr'} -> `Bool' #}
{#fun alpm_option_add_ignorepkg as ^ { `AlpmHandle'
                                     , withCString* `Text'} -> `Bool' #}
{#fun alpm_option_remove_ignorepkg as ^ { `AlpmHandle'
                                        , withCString* `Text'} -> `Bool' #}

{#fun alpm_option_get_ignoregroups as ^ {`AlpmHandle'} -> `AlpmListPtr' #}
{#fun alpm_option_set_ignoregroups as ^ {`AlpmHandle', `AlpmListPtr'} -> `Bool' #}
{#fun alpm_option_add_ignoregroup as ^ { `AlpmHandle'
                                       , withCString* `Text'} -> `Bool' #}
{#fun alpm_option_remove_ignoregroup as ^ { `AlpmHandle'
                                          , withCString* `Text'} -> `Bool' #}

{#fun alpm_option_get_assumeinstalled as ^ {`AlpmHandle'} -> `AlpmListPtr' #}
{#fun alpm_option_set_assumeinstalled as ^ {`AlpmHandle', `AlpmListPtr'} -> `Bool' #}
{#fun alpm_option_add_assumeinstalled as ^ { `AlpmHandle'
                                           , `AlpmDependencyPtr'} -> `Bool' #}
{#fun alpm_option_remove_assumeinstalled as ^ { `AlpmHandle'
                                              , `AlpmDependencyPtr'} -> `Bool' #}
