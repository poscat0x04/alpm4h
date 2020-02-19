module Foreign.ALPM.PublicAPI.Package.Transaction where

{# import Foreign.ALPM.Types.Foreign #}
import Foreign.Ptr

#include <alpm.h>


{#fun alpm_trans_get_flags as ^ {`AlpmHandle'} -> `Int' #}
{#fun alpm_trans_get_add as ^ {`AlpmHandle'} -> `AlpmListPtr' #}
{#fun alpm_trans_get_remove as ^ {`AlpmHandle'} -> `AlpmListPtr' #}
{#fun alpm_trans_init as ^ {`AlpmHandle', `Int'} -> `Int' #}
{#fun alpm_trans_prepare as ^ {`AlpmHandle', id `Ptr AlpmListPtr'} -> `Int' #}
{#fun alpm_trans_commit as ^ {`AlpmHandle', id `Ptr AlpmListPtr'} -> `Int' #}
{#fun alpm_trans_interrupt as ^ {`AlpmHandle'} -> `Int' #}
{#fun alpm_trans_release as ^ {`AlpmHandle'} -> `Int' #}
