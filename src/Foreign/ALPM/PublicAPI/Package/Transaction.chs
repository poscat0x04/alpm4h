module Foreign.ALPM.PublicAPI.Package.Transaction where

{# import Foreign.ALPM.Types.Foreign #}
import           Data.Set (Set)
import           Foreign.ALPM.Internal.Marshal
import           Foreign.Ptr

#include <alpm.h>


{#fun alpm_trans_get_flags as ^ {`AlpmHandle'} -> `Set AlpmTransFlag' decodeTransFlag #}
{#fun alpm_trans_get_add as ^ {`AlpmHandle'} -> `AlpmListPtr' #}
{#fun alpm_trans_get_remove as ^ {`AlpmHandle'} -> `AlpmListPtr' #}
{#fun alpm_trans_init as ^ { `AlpmHandle'
                           , encodeTransFlag `Set AlpmTransFlag'
                           } -> `Bool' #}
{#fun alpm_trans_prepare as ^ {`AlpmHandle', id `Ptr AlpmListPtr'} -> `Bool' #}
{#fun alpm_trans_commit as ^ {`AlpmHandle', id `Ptr AlpmListPtr'} -> `Bool' #}
{#fun alpm_trans_interrupt as ^ {`AlpmHandle'} -> `Bool' #}
{#fun alpm_trans_release as ^ {`AlpmHandle'} -> `Bool' #}
