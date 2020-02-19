module Foreign.ALPM.PublicAPI.Interface where

{#import Foreign.ALPM.Types.Foreign #}
import           Foreign.C

#include <alpm.h>
#include <alpm_list.h>

{#fun alpm_initialize as ^ {`String', `String', `AlpmErrnoPtr' } -> `AlpmHandle' #}
{#fun alpm_release as ^ {`AlpmHandle'} -> `Bool' #}
