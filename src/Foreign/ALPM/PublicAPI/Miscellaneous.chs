module Foreign.ALPM.PublicAPI.Miscellaneous where

{# import Foreign.ALPM.Types.Foreign #}
import           Data.Text (Text)
import           Foreign.ALPM.Internal.Marshal

#include <alpm.h>


{#fun alpm_compute_md5sum as ^ {withCString* `Text'} -> `Text' peekCString* #}
{#fun alpm_compute_sha256sum as ^ {withCString* `Text'} -> `Text' peekCString* #}
