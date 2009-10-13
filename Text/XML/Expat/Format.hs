-- hexpat, a Haskell wrapper for expat
-- Copyright (C) 2008 Evan Martin <martine@danga.com>
-- Copyright (C) 2009 Stephen Blackheath <http://blacksapphire.com/antispam>

-- | This module provides functions to format a tree
-- structure or SAX stream as UTF-8 encoded XML.

{-# LANGUAGE FlexibleContexts #-}

module Text.XML.Expat.Format (
        -- * High level
        formatTree,
        formatTree',
        formatNode,
        formatNode',
        -- * Low level
        xmlHeader,
        treeToSAX,
        formatSAX,
        formatSAX'
    ) where

import Text.XML.Expat.IO
import Text.XML.Expat.Tree
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Internal (c2w, w2c)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word

-- | Format document with <?xml.. header - lazy variant that returns lazy ByteString.
formatTree :: (GenericXMLString tag, GenericXMLString text) =>
              Node tag text
           -> L.ByteString
formatTree node = xmlHeader `L.append` formatNode node

-- | Format document with <?xml.. header - strict variant that returns strict ByteString.
formatTree' :: (GenericXMLString tag, GenericXMLString text) =>
               Node tag text
            -> B.ByteString
formatTree' = B.concat . L.toChunks . formatTree

-- | Format XML node with no header - lazy variant that returns lazy ByteString.
formatNode :: (GenericXMLString tag, GenericXMLString text) =>
              Node tag text
           -> L.ByteString
formatNode = formatSAX . treeToSAX

-- | Format XML node with no header - strict variant that returns strict ByteString.
formatNode' :: (GenericXMLString tag, GenericXMLString text) =>
               Node tag text
            -> B.ByteString
formatNode' = B.concat . L.toChunks . formatNode

xmlHeader :: L.ByteString
xmlHeader = L.pack $ map c2w "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"

-- | Flatten a tree structure into SAX events.
treeToSAX :: Node tag text -> [SAXEvent tag text]
treeToSAX (Element name atts children) =
        StartElement name atts : concatMap treeToSAX children ++ [EndElement name]
treeToSAX (Text txt) = [CharacterData txt]

-- | Format SAX events with no header - lazy variant that returns lazy ByteString.
formatSAX :: (GenericXMLString tag, GenericXMLString text) =>
             [SAXEvent tag text]
          -> L.ByteString
formatSAX = L.fromChunks . putSAX

-- | Format SAX events with no header - strict variant that returns strict ByteString.
formatSAX' :: (GenericXMLString tag, GenericXMLString text) =>
              [SAXEvent tag text]
           -> B.ByteString
formatSAX' = B.concat . L.toChunks . formatSAX

-- Do start tag and attributes but omit closing >
startTagHelper :: (GenericXMLString tag, GenericXMLString text) =>
                  tag
               -> [(tag, text)]
               -> [B.ByteString]
startTagHelper name atts =
    B.singleton (c2w '<'):
    gxToByteString name:
    concatMap (
            \(aname, avalue) ->
                B.singleton (c2w ' '):
                gxToByteString aname:
                pack "=\"":
                escapeText (gxToByteString avalue)++
                [B.singleton (c2w '"')]
        ) atts

putSAX :: (GenericXMLString tag, GenericXMLString text) =>
           [SAXEvent tag text]
        -> [B.ByteString]
putSAX (StartElement name attrs:EndElement name2:elts) =
    B.concat (startTagHelper name attrs ++ [pack "/>"]):putSAX elts
putSAX (StartElement name attrs:elts) =
    B.concat (startTagHelper name attrs ++ [B.singleton (c2w '>')]):putSAX elts
putSAX (EndElement name:elts) =
    B.concat [pack "</", gxToByteString name, B.singleton (c2w '>')]:putSAX elts
putSAX (CharacterData txt:elts) =
    B.concat (escapeText (gxToByteString txt)):putSAX elts
putSAX (FailDocument _:elts) = putSAX elts
putSAX [] = []

pack :: String -> B.ByteString
pack = B.pack . map c2w

escapees :: [Word8]
escapees = map c2w "&<\"'"

escapeText :: B.ByteString -> [B.ByteString]
escapeText str | B.null str = []
escapeText str =
    let (good, bad) = B.span (`notElem` escapees) str
    in  if B.null good
            then case w2c $ B.head str of
                '&'  -> pack "&amp;":escapeText rem
                '<'  -> pack "&lt;":escapeText rem
                '"'  -> pack "&quot;":escapeText rem
                '\'' -> pack "&apos;":escapeText rem
                _        -> error "hexpat: impossible"
            else good:escapeText bad
  where
    rem = B.tail str

