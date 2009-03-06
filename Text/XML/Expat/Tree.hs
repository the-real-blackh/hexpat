-- hexpat, a Haskell wrapper for expat
-- Copyright (C) 2008 Evan Martin <martine@danga.com>
-- Copyright (C) 2009 Stephen Blackheath <http://blacksapphire.com/antispam>

-- |The Expat.Tree module provides a simplified interface to parsing, that
-- returns a tree of the XML structure.  It is written using the lower-level
-- bindings in the "Text.XML.Expat.IO" module.  (Note that this is not a lazy
-- parse of the document: as soon as the root node is accessed, the entire
-- document is parsed.)

module Text.XML.Expat.Tree (
  Text.XML.Expat.Tree.parseTree,
  Text.XML.Expat.Tree.parseTreeString,
  Text.XML.Expat.Tree.parseTreeByteString,
  Text.XML.Expat.Tree.parseTreeText,
  Text.XML.Expat.Tree.parseTreeStringLazy,
  Text.XML.Expat.Tree.parseTreeByteStringLazy,
  Text.XML.Expat.Tree.parseTreeTextLazy,
  Node(..)
) where

import Text.XML.Expat.IO
import Text.XML.Expat.SAX
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Data.ByteString.Internal (c2w, w2c)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- |Tree representation. Everything is strict except for eChildren.
data Node tag text =
    Element {
        eName     :: !tag,
        eAttrs    :: ![(tag,text)],
        eChildren :: [Node tag text]
    } |
    Text !text
    deriving Show

modifyChildren :: ([Node tag text] -> [Node tag text])
               -> Node tag text
               -> Node tag text
modifyChildren f node = node { eChildren = f (eChildren node) }

-- |@parse enc doc@ parses /lazy/ bytestring XML content @doc@ with optional
-- encoding override @enc@ and returns the root 'Node' of the document if there
-- were no parsing errors.
parseTree :: Eq tag =>
             (B.ByteString -> tag, B.ByteString -> text)  -- ^ make tag, make text
          -> Maybe Encoding
          -> L.ByteString
          -> Maybe (Node tag text)
parseTree (mkTag, mkText) enc doc = unsafePerformIO $ runParse where
  runParse = do
    parser <- newParser enc
    -- We maintain the invariant that the stack always has one element,
    -- whose only child at the end of parsing is the root of the actual tree.
    stack <- newIORef [Element (mkTag $ B.pack []) [] []]
    setStartElementHandler  parser $ \n a ->
        modifyIORef stack (start (mkTag n) (map (\(n,v) -> (mkTag n, mkText v)) a))
    setEndElementHandler    parser (\n ->
        modifyIORef stack (end (mkTag n)))
    setCharacterDataHandler parser (\s ->
        modifyIORef stack (text (mkText s)))
    ok <- parse parser doc
    if ok
      then do
        [Element _ _ [root]] <- readIORef stack
        return $ Just root
      else return Nothing
  start name attrs stack = Element name attrs [] : stack
  text str (cur:rest) = modifyChildren (Text str:) cur : rest
  end name (cur:parent:rest) =
    if eName cur /= name then error "name mismatch" else
    let node = modifyChildren reverse cur in
    modifyChildren (node:) parent : rest

-- | Parse to a tree of type Node String String, strictly
parseTreeString :: Maybe Encoding
                -> L.ByteString
                -> Maybe (Node String String)
parseTreeString = parseTree (unpack, unpack)
  where
    unpack = map w2c . B.unpack

-- | Parse to a tree of type Node ByteString ByteString, strictly
parseTreeByteString :: Maybe Encoding
                    -> L.ByteString
                    -> Maybe (Node B.ByteString B.ByteString)
parseTreeByteString = parseTree (id, id)

-- | Parse to a tree of type Node Text Text, strictly
parseTreeText :: Maybe Encoding
              -> L.ByteString
              -> Maybe (Node T.Text T.Text)
parseTreeText = parseTree (TE.decodeUtf8, TE.decodeUtf8)

-- |@parse enc doc@ parses /lazy/ bytestring XML content @doc@ with optional
-- encoding override @enc@ and returns the root 'Node' of the document if there
-- were no parsing errors.
parseTreeLazy :: (B.ByteString -> tag, B.ByteString -> text)  -- ^ make tag, make text
              -> Maybe Encoding
              -> L.ByteString
              -> Node tag text
parseTreeLazy recipe mEnc bs = head . fst . ptl $ parseSAX recipe mEnc bs
  where
    ptl (StartDocument:rem) = ptl rem
    ptl (StartElement name attrs:rem) =
        let (children, rem') = ptl rem
            (out, rem'') = ptl rem'
        in  (Element name attrs children:out, rem'')
    ptl (EndElement name:rem) = ([], rem)
    ptl (CharacterData txt:rem) = let (out, rem') = ptl rem in (Text txt:out, rem')
    ptl otherwise = ([], [])

-- | Parse to a tree of type Node String String, lazily
parseTreeStringLazy :: Maybe Encoding
                    -> L.ByteString
                    -> Node String String
parseTreeStringLazy = parseTreeLazy (unpack, unpack)
  where
    unpack = map w2c . B.unpack

-- | Parse to a tree of type Node ByteString ByteString, lazily
parseTreeByteStringLazy :: Maybe Encoding
                        -> L.ByteString
                        -> Node B.ByteString B.ByteString
parseTreeByteStringLazy = parseTreeLazy (id, id)

-- | Parse to a tree of type Node Text Text, lazily
parseTreeTextLazy :: Maybe Encoding
                  -> L.ByteString
                  -> Node T.Text T.Text
parseTreeTextLazy = parseTreeLazy (TE.decodeUtf8, TE.decodeUtf8)

