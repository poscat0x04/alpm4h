module Foreign.ALPM.PublicAPI.Options where

{# import Foreign.ALPM.Types.Foreign #}
import           Foreign.ALPM.Internal.Marshal
import           Data.Text (Text)

#include <alpm.h>


--  functions involving callbacks are ignored

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
{#fun alpm_option_get_default_siglevel as ^ {`AlpmHandle'} -> `Int' #}
{#fun alpm_option_set_default_siglevel as ^ {`AlpmHandle', `Int'} -> `Bool' #}
{#fun alpm_option_get_remote_file_siglevel as ^ {`AlpmHandle'} -> `Int' #}
{#fun alpm_option_set_remote_file_siglevel as ^ {`AlpmHandle', `Int'} -> `Bool' #}
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
                                           , `AlpmDependency'} -> `Bool' #}
{#fun alpm_option_remove_assumeinstalled as ^ { `AlpmHandle'
                                              , `AlpmDependency'} -> `Bool' #}
