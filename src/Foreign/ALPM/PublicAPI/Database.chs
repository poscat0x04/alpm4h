module Foreign.ALPM.PublicAPI.Database where

{# import Foreign.ALPM.Types.Foreign #}
import           Data.Text (Text)
import           Data.Set (Set)
import           Foreign.ALPM.Internal.Marshal
import           Foreign
import           Foreign.C.Types

#include <alpm.h>


{#fun alpm_get_localdb as ^ {`AlpmHandle'} -> `AlpmDatabase' #}
{#fun alpm_get_syncdbs as ^ {`AlpmHandle'} -> `AlpmListPtr' #}
{#fun alpm_register_syncdb as ^ { `AlpmHandle'
                                , withCString* `Text'
                                , `AlpmSigLevel'
                                } -> `AlpmDatabase' #}
{#fun alpm_unregister_all_syncdbs as ^ {`AlpmHandle'} -> `Bool' #}
{#fun alpm_db_unregister as ^ {`AlpmDatabase'} -> `Bool' #}
{#fun alpm_db_get_name as ^ {`AlpmDatabase'} -> `Text' peekCString* #}
{#fun alpm_db_get_siglevel as ^ {`AlpmDatabase'} -> `AlpmSigLevel' #}
{#fun alpm_db_get_valid as ^ {`AlpmDatabase'} -> `Bool' #}
{#fun alpm_db_update as ^ {`Bool', `AlpmDatabase'} -> `Int' #}
{#fun alpm_db_get_pkg as ^ { `AlpmDatabase'
                           , withCString* `Text'
                           } -> `AlpmPackage' #}
{#fun alpm_db_get_pkgcache as ^ {`AlpmDatabase'} -> `AlpmListPtr' #}
{#fun alpm_db_get_group as ^ { `AlpmDatabase'
                             , withCString* `Text'
                             } -> `AlpmGroupPtr' #}
{#fun alpm_db_get_groupcache as ^ {`AlpmDatabase'} -> `AlpmListPtr' #}
{#fun alpm_db_search as ^ { `AlpmDatabase'
                          , `AlpmListPtr'
                          } -> `AlpmListPtr' #}
{#fun alpm_db_set_usage as ^ { `AlpmDatabase'
                             , encodeDBUsage `Set AlpmDBUsage'
                             } -> `Bool' #}
{#fun alpm_db_get_usage as ^ {`AlpmDatabase'
                             , alloca- `Set AlpmDBUsage' peekAndDecodeUsage*} -> `Bool' #}

{#fun alpm_db_get_servers as ^ {`AlpmDatabase'} -> `AlpmListPtr' #}
{#fun alpm_db_set_servers as ^ {`AlpmDatabase', `AlpmListPtr'} -> `Bool' #}
{#fun alpm_db_add_server as ^ { `AlpmDatabase'
                              , withCString* `Text'} -> `Bool' #}
{#fun alpm_db_remove_server as ^ { `AlpmDatabase'
                                 , withCString* `Text'} -> `Bool' #}


