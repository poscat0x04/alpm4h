module Foreign.ALPM.PublicAPI where

{# import Foreign.ALPM.Types.Foreign #}
import           Foreign.ALPM.Internal.Marshal
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

