module Foreign.ALPM.PublicAPI.Package.Dependency where

{# import Foreign.ALPM.Types.Foreign #}
import           Foreign.ALPM.Internal.Marshal
import           Data.Text (Text)

#include <alpm.h>


{#fun alpm_checkdeps as ^ { `AlpmHandle'
                          , `AlpmListPtr'
                          , `AlpmListPtr'
                          , `AlpmListPtr'
                          , `Bool'
                          } -> `AlpmListPtr' #}
{#fun alpm_find_satisfier as ^ { `AlpmListPtr'
                               , withCString* `Text'
                               } -> `AlpmPackage' #}

{#fun alpm_find_dbs_satisfier as ^ { `AlpmHandle'
                                   , `AlpmListPtr'
                                   , withCString* `Text'
                                   } -> `AlpmPackage' #}
{#fun alpm_checkconflicts as ^ {`AlpmHandle', `AlpmListPtr'} -> `AlpmListPtr' #}
{#fun alpm_dep_compute_string as ^ {`AlpmDependencyPtr'} -> `Text' peekCString* #}
{#fun alpm_dep_from_string as ^ {withCString* `Text'} -> `AlpmDependencyPtr' #}
{#fun alpm_dep_free as ^ {`AlpmDependencyPtr'} -> `()' #}


