module Main where

import Text.XML.Expat.Tree
import qualified Data.ByteString as B
import Data.ByteString.Internal (c2w, w2c)
import qualified Data.ByteString.Lazy as L
import Control.Monad
import Control.Parallel.Strategies

infiniteDoc = "<?xml version=\"1.0\"?><infinite>"++body 1
    where
        body i = "\n  <item idx=\"" ++ show i ++ "\"/>"++body (i+1)

toBL :: String -> L.ByteString
toBL = L.fromChunks . chunkify
  where
    chunkify str =
        let (start, rem) = splitAt 1024 str
        in  (B.pack $ map c2w start):chunkify rem

infiniteBL = toBL infiniteDoc

instance NFData B.ByteString where
    rnf bs = ()

-- This must not leak memory

main = do
    forever $ do
        putStrLn "loop"
        let sax = parseSAX byteStringFlavor Nothing (L.copy infiniteBL)
        --print $ take 10 sax
        rnf (take 10 sax) `seq` return ()


