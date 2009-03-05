-- hexpat, a Haskell wrapper for expat
-- Copyright (C) 2008 Evan Martin <martine@danga.com>
-- Copyright (C) 2009 Stephen Blackheath <http://blacksapphire.com/antispam>

-- |The Expat.Tree module provides a simplified interface to parsing, that
-- returns a tree of the XML structure.  It is written using the lower-level
-- bindings in the "Text.XML.Expat.IO" module.  (Note that this is not a lazy
-- parse of the document: as soon as the root node is accessed, the entire
-- document is parsed.)

{-# LANGUAGE BangPatterns #-}

module Text.XML.Expat.Tree (
  Text.XML.Expat.Tree.parseDoc,
  Text.XML.Expat.Tree.parseDocString,
  Text.XML.Expat.Tree.parseDocByteString,
  Text.XML.Expat.Tree.parseDocText,
  Node(..)
) where

import qualified Text.XML.Expat.IO as EIO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
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
parseDoc :: Eq tag =>
         (BS.ByteString -> tag)   -- | Function to make tag text
      -> (BS.ByteString -> text)  -- | Function to make text
      -> Maybe EIO.Encoding
      -> BSL.ByteString
      -> Maybe (Node tag text)
parseDoc mkTag mkText enc doc = unsafePerformIO $ runParse where
  runParse = do
    parser <- EIO.newParser enc
    -- We maintain the invariant that the stack always has one element,
    -- whose only child at the end of parsing is the root of the actual tree.
    stack <- newIORef [Element (mkTag $ BS.pack []) [] []]
    EIO.setStartElementHandler  parser $ \n a ->
        modifyIORef stack (start (mkTag n) (map (\(n,v) -> (mkTag n, mkText v)) a))
    EIO.setEndElementHandler    parser (\n ->
        modifyIORef stack (end (mkTag n)))
    EIO.setCharacterDataHandler parser (\s ->
        modifyIORef stack (text (mkText s)))
    ok <- EIO.parse parser doc
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

-- | Parse to a tree of type Node String String
parseDocString :: Maybe EIO.Encoding
               -> BSL.ByteString
               -> Maybe (Node String String)
parseDocString = parseDoc unpack unpack
  where
    unpack = map w2c . BS.unpack

-- | Parse to a tree of type Node ByteString ByteString
parseDocByteString :: Maybe EIO.Encoding
                   -> BSL.ByteString
                   -> Maybe (Node BS.ByteString BS.ByteString)
parseDocByteString = parseDoc id id

-- | Parse to a tree of type Node Text Text
parseDocText :: Maybe EIO.Encoding
             -> BSL.ByteString
             -> Maybe (Node T.Text T.Text)
parseDocText = parseDoc TE.decodeUtf8 TE.decodeUtf8

