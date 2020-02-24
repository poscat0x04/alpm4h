module Foreign.ALPM.PublicAPI.Miscellaneous
       ( alpmComputeMd5sum
       , alpmComputeSha256sum
       ) where

{# import Foreign.ALPM.Types.Foreign #}
import           Data.Text (Text)
import           Foreign.ALPM.Internal.Marshal

#include <alpm.h>


{#fun alpm_compute_md5sum as ^ {withCString* `Text'} -> `Text' peekCString* #}
{#fun alpm_compute_sha256sum as ^ {withCString* `Text'} -> `Text' peekCString* #}
