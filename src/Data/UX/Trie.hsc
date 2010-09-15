{-# Language ForeignFunctionInterface #-}
{-# Language EmptyDataDecls #-}

module Data.UX.Trie (
  Trie,
  
  build,
  save,
  load,
  prefixSearch,
  commonPrefixSearch,
  predictiveSearch,
  decodeKey,
  size,
  allocSize,
  
  ) where

import Control.Applicative
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Foreign
import Foreign.C

data RawTrie
newtype Trie = Trie { unTrie :: ForeignPtr RawTrie } deriving (Eq, Ord)

cNOTFOUND = 0xFFFFFFFF :: Int

new :: IO Trie
new = do
  p <- newTrie
  Trie <$> newForeignPtr p_deleteTrie p

build :: [B.ByteString] -> IO Trie
build keys = do
  t <- Data.UX.Trie.new
  allocaArray len $ \pkeys ->
    f keys pkeys 0 $ do
      withForeignPtr (unTrie t) $ \pt -> do
        buildTrie pt pkeys (fromIntegral len) 1
        return t
  where
    len = length keys
  
    f [] _ _ m = m
    f (k:ks) pkeys ix m = B.useAsCString k $ \pstr -> do
      pokeElemOff pkeys ix pstr
      f ks pkeys (ix+1) m

save :: Trie -> FilePath -> IO ()
save t filepath = do
  withForeignPtr (unTrie t) $ \p ->
    withCString filepath $ \pstr -> do
    saveTrie p pstr -- TODO: error
    return ()

load :: FilePath -> IO Trie
load filepath = do
  t <- Data.UX.Trie.new
  withForeignPtr (unTrie t) $ \p ->
    withCString filepath $ \pstr -> do
      loadTrie p pstr -- TODO error
      return t

prefixSearch :: Trie -> B.ByteString -> IO (Maybe (Int, Int))
prefixSearch t bs = do
  withForeignPtr (unTrie t) $ \p ->
    B.useAsCStringLen bs $ \(pstr, len) ->
      alloca $ \plen -> do
        docid <- prefixSearchTrie p pstr (fromIntegral len) plen
        if fromIntegral docid == cNOTFOUND
          then
          return Nothing
          else do
          retLen <- peek plen
          return $ Just (fromIntegral docid, fromIntegral retLen)

commonPrefixSearch :: Trie -> B.ByteString -> Int -> IO [Int]
commonPrefixSearch =
  searchCommon commonPrefixSearchTrie

predictiveSearch :: Trie -> B.ByteString -> Int -> IO [Int]
predictiveSearch =
  searchCommon predictiveSearchTrie

searchCommon f t bs lim = do
  withForeignPtr (unTrie t) $ \p ->
    alloca $ \ppids ->
    alloca $ \plen -> 
    B.useAsCStringLen bs $ \(pstr, slen) -> do
      f p pstr (fromIntegral slen) ppids plen (fromIntegral lim)
      pids <- peek ppids
      len <- peek plen
      map fromIntegral <$> peekArray (fromIntegral len) pids

decodeKey :: Trie -> Int -> IO B.ByteString
decodeKey t id = do
  withForeignPtr (unTrie t) $ \p ->
    alloca $ \pret ->
    alloca $ \plen -> do
      decodeKeyTrie p (fromIntegral id) pret plen
      ret <- peek pret
      -- len <- peek plen
      B.unsafePackMallocCString ret

size :: Trie -> IO Int
size t = do
  withForeignPtr (unTrie t) $ \p ->
    fromIntegral <$> sizeTrie p

allocSize :: Trie -> IO Int
allocSize t = do
  withForeignPtr (unTrie t) $ \p ->
    fromIntegral <$> getAllocSizeTrie p

-----

foreign import ccall newTrie :: IO (Ptr RawTrie)
foreign import ccall "&deleteTrie" p_deleteTrie :: FunPtr (Ptr RawTrie -> IO ())
foreign import ccall buildTrie :: Ptr RawTrie -> Ptr CString -> CInt -> CInt -> IO ()
foreign import ccall saveTrie :: Ptr RawTrie -> CString -> IO CInt
foreign import ccall loadTrie :: Ptr RawTrie -> CString -> IO CInt
foreign import ccall prefixSearchTrie :: Ptr RawTrie -> CString -> CSize -> Ptr CSize -> IO CULLong
foreign import ccall commonPrefixSearchTrie
  :: Ptr RawTrie -> CString -> CSize -> Ptr (Ptr CULLong) -> Ptr CSize -> CSize -> IO CSize
foreign import ccall predictiveSearchTrie
  :: Ptr RawTrie -> CString -> CSize -> Ptr (Ptr CULLong) -> Ptr CSize -> CSize -> IO CSize
foreign import ccall decodeKeyTrie :: Ptr RawTrie -> CULLong -> Ptr CString -> Ptr CSize -> IO ()

foreign import ccall sizeTrie :: Ptr RawTrie -> IO Int
foreign import ccall getAllocSizeTrie :: Ptr RawTrie -> IO Int

foreign import ccall "free" c_free :: Ptr a -> IO ()
