module Foreign.ALPM.Internal.Marshal
       ( withCString
       , peekCString
       ) where


import           Data.Text (Text)
import           Foreign.C.String (CString)
import           Data.ByteString (useAsCString)
import           Data.ByteString.Unsafe (unsafePackCString)
import           Data.Text.Encoding


withCString :: Text -> (CString -> IO a) -> IO a
withCString t = useAsCString (encodeUtf8 t)

peekCString :: CString -> IO Text
peekCString cs = decodeUtf8 <$> unsafePackCString cs
