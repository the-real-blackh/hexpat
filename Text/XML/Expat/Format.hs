{-# LANGUAGE FlexibleContexts #-}

module Text.XML.Expat.Format (
        formatTree,
        putTree,
        formatNode
    ) where

import Text.XML.Expat.IO
import Text.XML.Expat.Tree
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Internal (c2w, w2c)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Binary.Put
import Control.Monad

-- | Format document with <?xml.. header.
formatTree :: TreeFlavour tag text
           -> Maybe Encoding
           -> Node tag text
           -> L.ByteString
formatTree flavour mEnc node = runPut $ putTree flavour mEnc node

putTree :: TreeFlavour tag text
        -> Maybe Encoding
        -> Node tag text
        -> Put
putTree flavour mEnc node = do
    putByteString $ pack "<?xml version=\"1.0\""
    case mEnc of
        Just enc -> do
            putByteString $ pack " encoding=\""
            putByteString $ pack $ encodingToString enc
            putByteString $ pack "\""
        Nothing -> return ()
    putByteString $ pack "?>\n"
    formatNode flavour node

-- | Format XML node with no XML header.
formatNode :: TreeFlavour tag text
           -> Node tag text
           -> Put
formatNode flavour@(TreeFlavour _ _ putTag fmtText) (Element name attrs children) = do
    putWord8 $ c2w '<'
    let putThisTag = putTag name
    putThisTag
    forM_ attrs $ \(aname, avalue) -> do
        putWord8 $ c2w ' '
        putTag aname
        putByteString $ pack "=\""
        putXMLText $ fmtText avalue
        putByteString $ pack "\"" 
    if null children
        then
            putByteString $ pack "/>"
        else do
            putWord8 $ c2w '>'
            forM_ children $ formatNode flavour
            putByteString $ pack "</"
            putThisTag
            putWord8 $ c2w '>'
formatNode (TreeFlavour _ _ putTag fmtText) (Text txt) =
    putXMLText $ fmtText txt

pack :: String -> B.ByteString
pack = B.pack . map c2w

unpack :: L.ByteString -> String
unpack = map w2c . L.unpack

putXMLText :: B.ByteString -> Put
putXMLText str | B.null str = return ()
putXMLText str = do
    case w2c $ B.head str of
        '&'  -> putByteString $ pack "&amp;"
        '<'  -> putByteString $ pack "&lt;"
        '"'  -> putByteString $ pack "&quot;"
        '\'' -> putByteString $ pack "&apos;"
        ch   -> putWord8 (c2w ch)
    putXMLText $ B.tail str

