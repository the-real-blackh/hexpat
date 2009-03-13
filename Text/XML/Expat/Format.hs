-- hexpat, a Haskell wrapper for expat
-- Copyright (C) 2008 Evan Martin <martine@danga.com>
-- Copyright (C) 2009 Stephen Blackheath <http://blacksapphire.com/antispam>

-- | This module provides lazy functions to format a tree
-- structure as UTF-8 encoded XML.

{-# LANGUAGE FlexibleContexts #-}

module Text.XML.Expat.Format (
        formatTree,
        formatTree',
        formatNode,
        formatNode',
        putTree,
        putNode
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

-- | Format document with <?xml.. header - lazy variant that returns lazy ByteString.
formatTree :: TreeFlavor tag text
           -> Node tag text
           -> L.ByteString
formatTree flavour node = runPut $ putTree flavour node

-- | Format document with <?xml.. header - strict variant that returns strict ByteString.
formatTree' :: TreeFlavor tag text
           -> Node tag text
           -> B.ByteString
formatTree' flavour node = B.concat $ L.toChunks $ runPut $ putTree flavour node

-- | Format XML node with no header - lazy variant that returns lazy ByteString.
formatNode :: TreeFlavor tag text
           -> Node tag text
           -> L.ByteString
formatNode flavour node = runPut $ putNode flavour node

-- | Format XML node with no header - strict variant that returns strict ByteString.
formatNode' :: TreeFlavor tag text
           -> Node tag text
           -> B.ByteString
formatNode' flavour node = B.concat $ L.toChunks $ runPut $ putNode flavour node

-- | 'Data.Binary.Put.Put' interface for formatting a tree with <?xml.. header.
putTree :: TreeFlavor tag text
        -> Node tag text
        -> Put
putTree flavour node = do
    putByteString $ pack "<?xml version=\"1.0\""
    putByteString $ pack " encoding=\"UTF-8\""
    putByteString $ pack "?>\n"
    putNode flavour node

-- | 'Data.Binary.Put.Put' interface for formatting a node with no header.
putNode :: TreeFlavor tag text
           -> Node tag text
           -> Put
putNode flavour@(TreeFlavor _ _ putTag fmtText) (Element name attrs children) = do
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
            forM_ children $ putNode flavour
            putByteString $ pack "</"
            putThisTag
            putWord8 $ c2w '>'
putNode (TreeFlavor _ _ putTag fmtText) (Text txt) =
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

