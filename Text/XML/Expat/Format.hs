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
formatTree :: (GenericXMLString tag, GenericXMLString text) =>
              Node tag text
           -> L.ByteString
formatTree node = runPut $ putTree node

-- | Format document with <?xml.. header - strict variant that returns strict ByteString.
formatTree' :: (GenericXMLString tag, GenericXMLString text) =>
               Node tag text
            -> B.ByteString
formatTree' node = B.concat $ L.toChunks $ runPut $ putTree node

-- | Format XML node with no header - lazy variant that returns lazy ByteString.
formatNode :: (GenericXMLString tag, GenericXMLString text) =>
              Node tag text
           -> L.ByteString
formatNode node = runPut $ putNode node

-- | Format XML node with no header - strict variant that returns strict ByteString.
formatNode' :: (GenericXMLString tag, GenericXMLString text) =>
               Node tag text
            -> B.ByteString
formatNode' node = B.concat $ L.toChunks $ runPut $ putNode node

-- | 'Data.Binary.Put.Put' interface for formatting a tree with <?xml.. header.
putTree :: (GenericXMLString tag, GenericXMLString text) =>
           Node tag text
        -> Put
putTree node = do
    putByteString $ pack "<?xml version=\"1.0\""
    putByteString $ pack " encoding=\"UTF-8\""
    putByteString $ pack "?>\n"
    putNode node

-- | 'Data.Binary.Put.Put' interface for formatting a node with no header.
putNode :: (GenericXMLString tag, GenericXMLString text) =>
           Node tag text
        -> Put
putNode (Element name attrs children) = do
    putWord8 $ c2w '<'
    let putThisTag = putByteString $ gxToByteString name
    putThisTag
    forM_ attrs $ \(aname, avalue) -> do
        putWord8 $ c2w ' '
        putByteString $ gxToByteString aname
        putByteString $ pack "=\""
        putXMLText $ gxToByteString avalue
        putByteString $ pack "\"" 
    if null children
        then
            putByteString $ pack "/>"
        else do
            putWord8 $ c2w '>'
            forM_ children putNode
            putByteString $ pack "</"
            putThisTag
            putWord8 $ c2w '>'
putNode (Text txt) =
    putXMLText $ gxToByteString txt

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

