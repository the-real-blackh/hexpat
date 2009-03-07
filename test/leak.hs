module Main where

import Text.XML.Expat.Tree
import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w, w2c)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal as I
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

-- This must not leak memory

main = do
    forever $ do
        putStrLn "loop"
        c <- myLCopy longBL
        let sax = parseSAX byteStringFlavor Nothing c
        print $ take 10 sax
        --rnf (take 10 sax) `seq` return ()

