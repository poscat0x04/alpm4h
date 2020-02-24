module Foreign.ALPM.PublicAPI.Interface
       ( alpmInitialize
       , alpmRelease
       ) where

{#import Foreign.ALPM.Types.Foreign #}
import           Foreign.C hiding ( withCString
                                  , peekCString
                                  )
import           Data.Text (Text)
import           Foreign.ALPM.Internal.Marshal

#include <alpm.h>
#include <alpm_list.h>


{#fun alpm_initialize as ^ { withCString* `Text'
                           , withCString* `Text'
                           , `AlpmErrnoPtr'
                           } -> `AlpmHandle' #}
{#fun alpm_release as ^ {`AlpmHandle'} -> `Bool' #}
