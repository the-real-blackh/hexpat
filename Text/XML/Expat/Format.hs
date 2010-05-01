-- hexpat, a Haskell wrapper for expat
-- Copyright (C) 2008 Evan Martin <martine@danga.com>
-- Copyright (C) 2009 Stephen Blackheath <http://blacksapphire.com/antispam>

-- | This module provides functions to format a tree
-- structure or SAX stream as UTF-8 encoded XML.

{-# LANGUAGE FlexibleContexts #-}

module Text.XML.Expat.Format (
        -- * High level
        format,
        format',
        formatNode,
        formatNode',
        -- * Deprecated names
        formatTree,
        formatTree',
        -- * Low level
        xmlHeader,
        treeToSAX,
        formatSAX,
        formatSAX',
        -- * Indentation
        indent,
        indent_
    ) where

import Text.XML.Expat.NodeClass
import Text.XML.Expat.Tree

import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Internal (c2w, w2c)
import Data.Char (isSpace)
import Data.List
import Data.List.Class
import Data.Monoid
import Data.Word

-- | DEPRECATED: Renamed to 'format'.
formatTree :: (GenericXMLString tag, GenericXMLString text) =>
              Node tag text
           -> L.ByteString
formatTree = format

-- | Format document with <?xml.. header - lazy variant that returns lazy ByteString.
format :: (GenericXMLString tag, GenericXMLString text) =>
              Node tag text
           -> L.ByteString
format node = xmlHeader `L.append` formatNode node

-- | DEPRECATED: Renamed to 'format''.
formatTree' :: (GenericXMLString tag, GenericXMLString text) =>
               Node tag text
            -> B.ByteString
formatTree' = B.concat . L.toChunks . formatTree

-- | Format document with <?xml.. header - strict variant that returns strict ByteString.
format' :: (GenericXMLString tag, GenericXMLString text) =>
           Node tag text
        -> B.ByteString
format' = B.concat . L.toChunks . formatTree

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

-- | Flatten a tree structure into SAX events, monadic version.
treeToSAX :: (GenericXMLString tag, GenericXMLString text, Monoid text, NodeClass n c,
              Functor c) =>
             n c tag text -> c (SAXEvent tag text)
treeToSAX node
    | isElement node =
        let name = getName node
            atts = getAttributes node
            children = getChildren node
        in  cons (StartElement name atts) $
            concatL (fmap treeToSAX children)
            `mplus`
            cons (EndElement name) mzero
    | otherwise =
        cons (CharacterData $ getText node) mzero
  where
    concatL :: List l => l (l a) -> l a
    concatL xs = joinL $ foldlL mplus mzero xs

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
putSAX (StartElement name attrs:EndElement _:elts) =
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
                '&'  -> pack "&amp;":escapeText rema
                '<'  -> pack "&lt;":escapeText rema
                '"'  -> pack "&quot;":escapeText rema
                '\'' -> pack "&apos;":escapeText rema
                _        -> error "hexpat: impossible"
            else good:escapeText bad
  where
    rema = B.tail str

-- | Make the output prettier by adding indentation.
indent :: (GenericXMLString tag, GenericXMLString text) =>
          Int   -- ^ Number of indentation spaces per nesting level
       -> Node tag text
       -> Node tag text
indent = indent_ 0

-- | Make the output prettier by adding indentation, specifying initial indent.
indent_ :: (GenericXMLString tag, GenericXMLString text) =>
           Int   -- ^ Initial indent (spaces)
        -> Int   -- ^ Number of indentation spaces per nesting level
        -> Node tag text
        -> Node tag text
indent_ _ _ t@(Text _) = t
indent_ cur perLevel elt@(Element name attrs chs) =
    if any isElement chs
        then Element name attrs $
                let (_, chs') = mapAccumL (\startOfText ch -> case ch of
                            Element _ _ _ ->
                                let cur' = cur + perLevel
                                in  (
                                        True,
                                        [
                                            Text (gxFromString ('\n':replicate cur' ' ')),
                                            indent_ cur' perLevel ch
                                        ]
                                    )
                            Text t | startOfText ->
                                case strip t of
                                    Nothing -> (True, [])
                                    Just t' -> (False, [Text t'])
                            Text _ -> (False, [ch])
                        ) True chs
                in  concat chs' ++ [Text $ gxFromString ('\n':replicate cur ' ')]
        else elt
  where
    strip t | gxNullString t = Nothing
    strip t | isSpace (gxHead t) = strip (gxTail t)
    strip t = Just t

