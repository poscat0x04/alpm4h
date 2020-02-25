module Foreign.ALPM.PublicAPI
       ( module Foreign.ALPM.PublicAPI.Package
       , module Foreign.ALPM.PublicAPI.Miscellaneous
       , module Foreign.ALPM.PublicAPI.Logging
       , module Foreign.ALPM.PublicAPI.Interface
       , module Foreign.ALPM.PublicAPI.Error
       , module Foreign.ALPM.PublicAPI.Options
       , module Foreign.ALPM.PublicAPI.Database

       , alpmFetchPkgurl
       , alpmUnlock
       , alpmVersion
       , alpmCapabilities
       ) where

{# import Foreign.ALPM.Types.Foreign #}
import           Data.Set (Set)
import           Foreign.ALPM.Internal.Marshal
import           Foreign.ALPM.PublicAPI.Package
import           Foreign.ALPM.PublicAPI.Miscellaneous
import           Foreign.ALPM.PublicAPI.Logging
import           Foreign.ALPM.PublicAPI.Interface
import           Foreign.ALPM.PublicAPI.Error
import           Foreign.ALPM.PublicAPI.Options
import           Foreign.ALPM.PublicAPI.Database
import           Data.Text (Text)

#include <alpm.h>


{#fun alpm_fetch_pkgurl as ^ { `AlpmHandle'
                             , withCString* `Text'
                             } -> `Text' peekCString* #}

{#fun alpm_unlock as ^ {`AlpmHandle'} -> `Bool' #}
{#fun alpm_version as ^ {} -> `Text' peekCString* #}
{#fun alpm_capabilities as ^ {} -> `Set AlpmCaps' decodeCaps #}


