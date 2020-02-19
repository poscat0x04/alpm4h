module Foreign.ALPM.PublicAPI.Error where

{# import Foreign.ALPM.Types.Foreign #}
import           Foreign.ALPM.Internal.Marshal
import           Data.Text (Text)

#include <alpm.h>


{#fun alpm_errno as ^ {`AlpmHandle'} -> `AlpmErrno' #}
{#fun alpm_strerror as ^ {`AlpmErrno'} -> `Text' peekCString* #}
