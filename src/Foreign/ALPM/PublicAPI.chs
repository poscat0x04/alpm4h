module Foreign.ALPM.PublicAPI
       ( module Foreign.ALPM.Internal.Marshal
       , module Foreign.ALPM.PublicAPI.Package
       , module Foreign.ALPM.PublicAPI.Miscellaneous
       , module Foreign.ALPM.PublicAPI.Logging
       , module Foreign.ALPM.PublicAPI.Interface
       , module Foreign.ALPM.PublicAPI.Error

       , alpmFetchPkgurl
       , alpmUnlock
       , alpmVersion
       , alpmCapabilities
       , alpmConflictFree
       , alpmFileconflictFree
       , alpmDepmissingFree
       ) where

{# import Foreign.ALPM.Types.Foreign #}
import           Foreign.ALPM.Internal.Marshal
import           Foreign.ALPM.PublicAPI.Package
import           Foreign.ALPM.PublicAPI.Miscellaneous
import           Foreign.ALPM.PublicAPI.Logging
import           Foreign.ALPM.PublicAPI.Interface
import           Foreign.ALPM.PublicAPI.Error
import           Data.Text (Text)

#include <alpm.h>


{#fun alpm_fetch_pkgurl as ^ { `AlpmHandle'
                             , withCString* `Text'
                             } -> `Text' peekCString* #}

{#fun alpm_unlock as ^ {`AlpmHandle'} -> `Int' #}
{#fun alpm_version as ^ {} -> `Text' peekCString* #}
{#fun alpm_capabilities as ^ {} -> `Int' #}

{#fun alpm_conflict_free as ^ {`AlpmConflict'} -> `()' #}
{#fun alpm_fileconflict_free as ^ {`AlpmFileConflict'} -> `()' #}
{#fun alpm_depmissing_free as ^ {`AlpmDepmissing'} -> `()' #}

