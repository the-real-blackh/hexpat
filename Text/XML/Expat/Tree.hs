-- hexpat, a Haskell wrapper for expat
-- Copyright (C) 2008 Evan Martin <martine@danga.com>

-- |The Expat.Tree module provides a simplified interface to parsing, that
-- returns a tree of the XML structure.  It is written using the lower-level
-- bindings in the "Text.XML.Expat.IO" module.  (Note that this is not a lazy
-- parse of the document: as soon as the root node is accessed, the entire
-- document is parsed.)

module Text.XML.Expat.Tree (
  Text.XML.Expat.Tree.parse, Node(..),
  EIO.Encoding(..)
) where

import qualified Text.XML.Expat.IO as EIO
import qualified Data.ByteString.Lazy as B
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

-- |Simplistic XML tree representation.
data Node = Element { eName :: String, eAttrs :: [(String,String)],
                      eChildren :: [Node] }
          | Text String
          deriving Show

modifyChildren :: ([Node] -> [Node]) -> Node -> Node
modifyChildren f node = node { eChildren = f (eChildren node) }

-- |@parse enc doc@ parses /lazy/ bytestring XML content @doc@ with optional
-- encoding override @enc@ and returns the root 'Node' of the document if there
-- were no parsing errors.
parse :: Maybe EIO.Encoding -> B.ByteString -> Maybe Node
parse enc doc = unsafePerformIO $ runParse where
  runParse = do
    parser <- EIO.newParser enc
    -- We maintain the invariant that the stack always has one element,
    -- whose only child at the end of parsing is the root of the actual tree.
    stack <- newIORef [Element "" [] []]
    EIO.setStartElementHandler  parser (\n a -> modifyIORef stack (start n a))
    EIO.setEndElementHandler    parser (\n -> modifyIORef stack (end n))
    EIO.setCharacterDataHandler parser (\s -> modifyIORef stack (text s))
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

