-- Memory leak test
--
-- This test passes if you can leave it running for several minutes, and it
-- does not leak memory or exhibit any other undesirable behaviour.

module Main where

import Text.XML.Expat.Tree
import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w, w2c)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal as I
import Control.Concurrent
import Control.Monad
import Control.Parallel.Strategies
import Foreign.ForeignPtr
import Foreign.Ptr

longDoc = "<?xml version=\"1.0\"?><long>"++body 1
    where
        body 10000 = "</long>"
        body i = "\n  <item idx=\"" ++ show i ++ "\"/>"++body (i+1)

toBL :: String -> L.ByteString
toBL = L.fromChunks . chunkify
  where
    chunkify [] = []
    chunkify str =
        let (start, rem) = splitAt 1024 str
        in  (B.pack $ map c2w start):chunkify rem

longBL = toBL longDoc

instance NFData B.ByteString where
    rnf bs = ()

myCopy :: B.ByteString -> IO B.ByteString
myCopy (I.PS x s l) = I.create l $ \p -> withForeignPtr x $ \f ->
    I.memcpy p (f `plusPtr` s) (fromIntegral l)

myLCopy :: L.ByteString -> IO L.ByteString
myLCopy bs = do
    let cs = L.toChunks bs
    cs' <- mapM myCopy cs
    return $ L.fromChunks cs'

{-
allocateStuff :: IO ()
allocateStuff = do
    x <- forM [1..1000] $ \idx -> return  $ show idx
    rnf x `seq` return ()
    -}

gocrazy descr = forever $ do
    putStrLn descr
    c <- myLCopy longBL
    let sax = parseSAX Nothing c :: [SAXEvent B.ByteString B.ByteString]
    rnf (take 20 sax) `seq` return ()

main = do
    forkIO $ gocrazy "one"
    gocrazy "two"

