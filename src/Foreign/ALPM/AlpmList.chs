-- | Bindings to list functions
module Foreign.ALPM.AlpmList where

#include <alpm.h>

{# import Foreign.ALPM.Types.Foreign #}

import           Data.Text (Text)
import           Foreign
import           Foreign.ALPM.Internal.Marshal


{#fun alpm_list_free as ^ {`AlpmListPtr'} -> `()' #}
{#fun alpm_list_free_inner as ^ { `AlpmListPtr'
                                , withFreeFunc* `FreeFunc'
                                } -> `()' #}
{#fun alpm_list_add as ^ {`AlpmListPtr', `Ptr ()'} -> `AlpmListPtr' #}
{#fun alpm_list_append as ^ { id `Ptr AlpmListPtr'
                            , `Ptr ()'
                            } -> `AlpmListPtr' #}
{#fun alpm_list_append_strdup as ^ { id `Ptr AlpmListPtr'
                                   , withCString* `Text'
                                   } -> `AlpmListPtr' #}
-- alpm_list_add_sorted
{#fun alpm_list_join as ^ {`AlpmListPtr', `AlpmListPtr'} -> `AlpmListPtr' #}
-- alpm_list_mmerge
-- alpm_list_msort
{#fun alpm_list_remove_item as ^ {`AlpmListPtr', `AlpmListPtr'} -> `AlpmListPtr' #}
-- alpm_list_remove
{#fun alpm_list_remove_str as ^ { `AlpmListPtr'
                                , withCString* `Text'
                                , alloca- `Text' peekCStringRef*
                                } -> `AlpmListPtr' #}
{#fun alpm_list_remove_dupes as ^ {`AlpmListPtr'} -> `AlpmListPtr' #}
{#fun alpm_list_strdup as ^ {`AlpmListPtr'} -> `AlpmListPtr' #}
{#fun alpm_list_copy as ^ {`AlpmListPtr'} -> `AlpmListPtr' #}
{#fun alpm_list_copy_data as ^ {`AlpmListPtr', `Int'} -> `AlpmListPtr' #}
{#fun alpm_list_reverse as ^ {`AlpmListPtr'} -> `AlpmListPtr' #}
{#fun alpm_list_nth as ^ {`AlpmListPtr', `Int'} -> `AlpmListPtr' #}
{#fun alpm_list_next as ^ {`AlpmListPtr'} -> `AlpmListPtr' #}
{#fun alpm_list_previous as ^ {`AlpmListPtr'} -> `AlpmListPtr' #}
{#fun alpm_list_last as ^ {`AlpmListPtr'} -> `AlpmListPtr' #}
{#fun alpm_list_count as ^ {`AlpmListPtr'} -> `Int' #}
-- alpm_list_find
-- alpm_list_find_ptr
-- alpm_list_find_str
-- alpm_list_diff
-- alpm_list_diff_sorted
{#fun alpm_list_to_array as ^ {`AlpmListPtr', `Int', `Int'} -> `Ptr ()' #}

