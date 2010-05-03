{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
-- hexpat, a Haskell wrapper for expat
-- Copyright (C) 2008 Evan Martin <martine@danga.com>
-- Copyright (C) 2009 Stephen Blackheath <http://blacksapphire.com/antispam>

-- | This module provides functions to format a tree
-- structure or SAX stream as UTF-8 encoded XML.
module Text.XML.Expat.Format (
        -- * High level
        format,
        format',
        formatG,
        formatNode,
        formatNode',
        formatNodeG,
        -- * Deprecated names
        formatTree,
        formatTree',
        -- * Low level
        xmlHeader,
        treeToSAX,
        formatSAX,
        formatSAX',
        formatSAXG,
        -- * Indentation
        indent,
        indent_
    ) where

import Text.XML.Expat.NodeClass
import Text.XML.Expat.SAX

import Control.Monad
import Control.Monad.Writer
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Internal (c2w, w2c)
import Data.Char (isSpace)
import Data.List.Class
import Data.Word

-- | DEPRECATED: Renamed to 'format'.
formatTree :: (NodeClass n [], GenericXMLString tag, GenericXMLString text) =>
              n [] tag text
           -> L.ByteString
formatTree = format

-- | Format document with <?xml.. header - lazy variant that returns lazy ByteString.
format :: (NodeClass n [], GenericXMLString tag, GenericXMLString text) =>
          n [] tag text
       -> L.ByteString
format node = L.fromChunks (xmlHeader : formatNodeG node)

-- | Format document with <?xml.. header - generalized variant that returns a generic
-- list of strict ByteStrings.
formatG :: (NodeClass n c, GenericXMLString tag, GenericXMLString text) =>
          n c tag text
       -> c B.ByteString
formatG node = cons xmlHeader $ formatNodeG node

-- | DEPRECATED: Renamed to 'format''.
formatTree' :: (NodeClass n [], GenericXMLString tag, GenericXMLString text) =>
               n [] tag text
            -> B.ByteString
formatTree' = B.concat . L.toChunks . formatTree

-- | Format document with <?xml.. header - strict variant that returns strict ByteString.
format' :: (NodeClass n [], GenericXMLString tag, GenericXMLString text) =>
           n [] tag text
        -> B.ByteString
format' = B.concat . L.toChunks . format

-- | Format XML node with no header - lazy variant that returns lazy ByteString.
formatNode :: (NodeClass n [], GenericXMLString tag, GenericXMLString text) =>
              n [] tag text
           -> L.ByteString
formatNode = formatSAX . treeToSAX

-- | Format XML node with no header - strict variant that returns strict ByteString.
formatNode' :: (NodeClass n [], GenericXMLString tag, GenericXMLString text) =>
               n [] tag text
            -> B.ByteString
formatNode' = B.concat . L.toChunks . formatNode

-- | Format XML node with no header - generalized variant that returns a generic
-- list of strict ByteStrings.
formatNodeG :: (NodeClass n c, GenericXMLString tag, GenericXMLString text) =>
              n c tag text
           -> c B.ByteString
formatNodeG = formatSAXG . treeToSAX

xmlHeader :: B.ByteString
xmlHeader = B.pack $ map c2w "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"

-- | Flatten a tree structure into SAX events, monadic version.
treeToSAX :: forall tag text n c . (GenericXMLString tag, GenericXMLString text,
                 Monoid text, NodeClass n c) =>
             n c tag text -> c (SAXEvent tag text)
treeToSAX node
    | isElement node =
        let name = getName node
            atts = getAttributes node
            children = getChildren node
            postpend :: c (SAXEvent tag text) -> c (SAXEvent tag text)
            postpend l = joinL $ do
                li <- runList l
                return $ case li of
                    Nil -> singleton (EndElement name)
                    Cons n l' -> cons n (postpend l')
        in  cons (StartElement name atts) $
            postpend (concatL $ fmap treeToSAX children)
    | isText node =
        singleton (CharacterData $ getText node)
    | otherwise = mzero
  where
    singleton = return

concatL :: List l => l (l a) -> l a
concatL l1 = joinL $ do
    li1 <- runList l1
    return $ case li1 of
        Nil -> mzero
        Cons l2 l1' ->
            let concat2L l2 = joinL $ do
                    li2 <- runList l2
                    return $ case li2 of
                        Nil -> concatL l1'
                        Cons elt l2' -> cons elt $ concat2L l2'
            in  concat2L l2

-- | Format SAX events with no header - lazy variant that returns lazy ByteString.
formatSAX :: (GenericXMLString tag, GenericXMLString text) =>
             [SAXEvent tag text]
          -> L.ByteString
formatSAX = L.fromChunks . formatSAXG

-- | Format SAX events with no header - strict variant that returns strict ByteString.
formatSAX' :: (GenericXMLString tag, GenericXMLString text) =>
              [SAXEvent tag text]
           -> B.ByteString
formatSAX' = B.concat . formatSAXG

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

-- | Format SAX events with no header - generalized variant that uses generic
-- list.
formatSAXG :: forall c tag text . (List c, GenericXMLString tag,
              GenericXMLString text) =>
          c (SAXEvent tag text)    -- ^ SAX events
       -> c B.ByteString
formatSAXG l1 = joinL $ do
    it1 <- runList l1
    return $ case it1 of
        Nil -> mzero
        Cons (StartElement name attrs) l2 ->
            fromList (startTagHelper name attrs)
            `mplus` (
                joinL $ do
                    it2 <- runList l2
                    return $ case it2 of
                        Cons (EndElement _) l3 ->
                            cons (pack "/>") $
                            formatSAXG l3
                        _ ->
                            cons (B.singleton (c2w '>')) $
                            formatSAXG l2
            )
        Cons (EndElement name) l2 ->
            cons (pack "</") $
            cons (gxToByteString name) $
            cons (B.singleton (c2w '>')) $
            formatSAXG l2
        Cons (CharacterData txt) l2 ->
            fromList (escapeText (gxToByteString txt))
            `mplus`
            formatSAXG l2
        Cons (FailDocument _) l2 ->
            formatSAXG l2

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
indent :: (NodeClass n c, GenericXMLString tag, GenericXMLString text) =>
          Int   -- ^ Number of indentation spaces per nesting level
       -> n c tag text
       -> n c tag text
indent = indent_ 0

-- | Make the output prettier by adding indentation, specifying initial indent.
indent_ :: forall n c tag text . (NodeClass n c, GenericXMLString tag, GenericXMLString text) =>
           Int   -- ^ Initial indent (spaces)
        -> Int   -- ^ Number of indentation spaces per nesting level
        -> n c tag text
        -> n c tag text
indent_ cur perLevel elt | isElement elt =
    flip modifyChildren elt $ \chs -> joinL $ do
        anyElts <- anyElements chs
        if anyElts
            then addSpace True chs
            else return chs
  where
    addSpace :: Bool -> c (n c tag text) -> ItemM c (c (n c tag text))
    addSpace startOfText l = do
        ch <- runList l
        case ch of
            Nil -> return $ singleton (mkText $ gxFromString ('\n':replicate cur ' '))
            Cons elt l' | isElement elt -> do
                let cur' = cur + perLevel
                return $
                    cons (mkText $ gxFromString ('\n':replicate cur' ' ')) $
                    cons (indent_ cur' perLevel elt) $
                    joinL (addSpace True l')

            Cons tx l' | isText tx && startOfText ->
                case strip (getText tx) of
                    Nothing -> addSpace True l'
                    Just t' -> return $
                        cons (mkText t') $
                        joinL $ addSpace False l'
            Cons n l' ->
                return $
                    cons n $
                    joinL $ addSpace False l'
    anyElements :: c (n c tag text) -> ItemM c Bool
    anyElements l = do
        n <- runList l
        case n of
            Nil                    -> return False
            Cons n _ | isElement n -> return True
            Cons _ l'              -> anyElements l'

    strip t | gxNullString t = Nothing
    strip t | isSpace (gxHead t) = strip (gxTail t)
    strip t = Just t

    singleton = return
indent_ _ _ n = n

