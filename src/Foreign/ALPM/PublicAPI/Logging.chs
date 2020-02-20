module Foreign.ALPM.PublicAPI.Logging where


{# import Foreign.ALPM.Types.Foreign #}
import           Foreign.ALPM.Internal.Marshal
import           Data.Text (Text)

#include <alpm.h>


{#fun alpm_logaction as ^ { `AlpmHandle'
                          , withCString* `Text'
                          , withCString* `Text'
                          } -> `Bool' #}
