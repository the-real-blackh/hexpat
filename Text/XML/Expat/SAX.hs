module Text.XML.Expat.SAX where

import Text.XML.Expat.IO
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Internal (c2w,w2c)
import Control.Concurrent.MVar
import Control.Concurrent
import System.IO.Unsafe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE


data SAXEvent tag text =
    StartDocument |
    StartElement tag [(tag, text)] |
    EndElement tag |
    CharacterData text |
    EndDocument |
    FailDocument
    deriving (Eq, Show)

parseSAX :: (B.ByteString -> tag, B.ByteString -> text)  -- ^ make tag, make text
         -> Maybe Encoding
         -> L.ByteString
         -> [SAXEvent tag text]
parseSAX (mkTag,mkText) enc doc = unsafePerformIO $ do
    events <- newEmptyMVar
    forkIO $ runParser events

    let readEvents = do
            event <- takeMVar events
            rem <- case event of
                EndDocument -> return []
                FailDocument -> return []
                otherwise ->   unsafeInterleaveIO readEvents
            return (event:rem)
    readEvents
  where
    runParser events = do
        putMVar events StartDocument
        parser <- newParser enc
        setStartElementHandler  parser $ \n a ->
            putMVar events $ StartElement (mkTag n) (map (\(n,v) -> (mkTag n, mkText v)) a)
        setEndElementHandler    parser $ \n ->
            putMVar events $ EndElement (mkTag n)
        setCharacterDataHandler parser $ \s ->
            putMVar events $ CharacterData (mkText s)
        ok <- parse parser doc
        if ok
          then putMVar events EndDocument
          else putMVar events FailDocument

-- | Parse to a tree of type Node String String
parseSAXString :: Maybe Encoding
               -> L.ByteString
               -> [SAXEvent String String]
parseSAXString = parseSAX (unpack, unpack)
  where
    unpack = map w2c . B.unpack

-- | Parse to a tree of type Node ByteString ByteString
parseSAXByteString :: Maybe Encoding
                   -> L.ByteString
                   -> [SAXEvent B.ByteString B.ByteString]
parseSAXByteString = parseSAX (id, id)

-- | Parse to a tree of type Node Text Text
parseSAXText :: Maybe Encoding
             -> L.ByteString
             -> [SAXEvent T.Text T.Text]
parseSAXText = parseSAX (TE.decodeUtf8, TE.decodeUtf8)

