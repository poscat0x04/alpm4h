{-# LANGUAGE ForeignFunctionInterface #-}
module Foreign.ALPM.Internal.Marshal
       ( withCString
       , peekCString
       , peekCStringRef
       , enumToOrd
       , decodeTransFlag
       , encodeTransFlag
       , decodeDBUsage
       , encodeDBUsage
       , decodeCaps
       , withFreeFunc
       , peekAndDecodeUsage
       , ifNotNull
       , ifNotNullPkg
       , ifNotNullDB
       , peekMaybe
       , peekMaybeCString
       ) where


import           Control.Monad
import           Data.Bits
import qualified Data.Set as S
import           Data.Text (Text)
import           Foreign.C.String (CString)
import           Foreign.C.Types
import           Foreign.ALPM.Types.Foreign
import           Foreign
import           Data.ByteString (useAsCString)
import           Data.ByteString.Unsafe (unsafePackCString)
import           Data.Text.Encoding


withCString :: Text -> (CString -> IO a) -> IO a
withCString t = useAsCString (encodeUtf8 t)

peekCString :: CString -> IO Text
peekCString cs = decodeUtf8 <$> unsafePackCString cs

enumToOrd :: Enum a => a -> Ordering
enumToOrd e
    | i > 0  = GT
    | i == 0 = EQ
    | i < 0  = LT
    where
      i = fromEnum e

transFlags :: [AlpmTransFlag]
transFlags = [ AlpmTransFlagNodeps
             , undefined
             , AlpmTransFlagNosave
             , AlpmTransFlagNodepversion
             , AlpmTransFlagCascade
             , AlpmTransFlagRecurse
             , AlpmTransFlagDbonly
             , undefined
             , AlpmTransFlagAlldeps
             , AlpmTransFlagDownloadonly
             , AlpmTransFlagNoscriptlet
             , AlpmTransFlagNoconflicts
             , undefined
             , AlpmTransFlagNeeded
             , AlpmTransFlagAllexplicit
             , AlpmTransFlagUnneeded
             , AlpmTransFlagRecurseall
             , AlpmTransFlagNolock
             ]

fuseMaybe :: (a -> b -> Maybe c) -> [a] -> [b] -> [c]
fuseMaybe _ [] _ = []
fuseMaybe _ _ [] = []
fuseMaybe p (x:xs) (y:ys)
    = case (p x y) of
        Nothing -> fuseMaybe p xs ys
        Just c -> c : fuseMaybe p xs ys

fromBits :: Bits a => a -> [Bool]
fromBits b
    | b == zeroBits = []
    | otherwise     = testBit b 0 : fromBits (shiftR b 1)

decodeTransFlag :: Bits a => a -> S.Set AlpmTransFlag
decodeTransFlag b =
    let binary = fromBits b
        zipped = fuseMaybe (\x y -> if x then Just y else Nothing) binary transFlags
     in S.fromList zipped

encodeTransFlag :: Enum a => S.Set AlpmTransFlag -> a
encodeTransFlag = toEnum . sum . map fromEnum . S.toList

dbUsage :: [AlpmDBUsage]
dbUsage = [ AlpmDbUsageSync
          , AlpmDbUsageSearch
          , AlpmDbUsageInstall
          , AlpmDbUsageUpgrade
          ]

decodeDBUsage :: Bits a => a -> S.Set AlpmDBUsage
decodeDBUsage b =
    let binary = fromBits b
        zipped = fuseMaybe (\x y -> if x then Just y else Nothing) binary dbUsage
     in S.fromList zipped

encodeDBUsage :: Enum a => S.Set AlpmDBUsage -> a
encodeDBUsage s =
    let l = S.toList s
     in if AlpmDbUsageAll `elem` l
           then toEnum $ fromEnum AlpmDbUsageAll
           else (toEnum . sum . map fromEnum) l

caps :: [AlpmCaps]
caps = [ AlpmCapabilityNls
      , AlpmCapabilityDownloader
      , AlpmCapabilitySignatures
      ]

decodeCaps :: Bits a => a -> S.Set AlpmCaps
decodeCaps b =
    let binary = fromBits b
        zipped = fuseMaybe (\x y -> if x then Just y else Nothing) binary caps
     in S.fromList zipped

foreign import ccall "wrapper"
  mkFreeFunc :: FreeFunc -> IO (FunPtr FreeFunc)

withFreeFunc :: FreeFunc -> (FunPtr FreeFunc -> IO a) -> IO a
withFreeFunc f io = do
    fptr <- mkFreeFunc f
    io fptr

peekCStringRef :: Ptr CString -> IO Text
peekCStringRef = peek >=> peekCString

peekAndDecodeUsage :: Ptr CInt -> IO (S.Set AlpmDBUsage)
peekAndDecodeUsage ptr = decodeDBUsage <$> peek ptr

ifNotNull :: Ptr a -> Maybe (Ptr a)
ifNotNull p = if p == nullPtr then Nothing else Just p

ifNotNullPkg :: AlpmPackage -> Maybe AlpmPackage
ifNotNullPkg p@(AlpmPackage ptr) = if ptr == nullPtr then Nothing else Just p

ifNotNullDB :: AlpmDatabase -> Maybe AlpmDatabase
ifNotNullDB db@(AlpmDatabase ptr) = if ptr ==nullPtr then Nothing else Just db

peekMaybe :: Storable a => Ptr a -> IO (Maybe a)
peekMaybe = maybePeek peek

peekMaybeCString :: CString -> IO (Maybe Text)
peekMaybeCString = maybePeek peekCString
