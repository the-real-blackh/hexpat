-- hexpat, a Haskell wrapper for expat
-- Copyright (C) 2008 Evan Martin <martine@danga.com>

-- |The Expat.Tree module provides a simplified interface to parsing, that
-- returns a tree of the XML structure.  (Note that this is not a lazy parse
-- of the document: as soon as the root node is accessed, the entire document
-- is parsed.)

module Text.XML.Expat.Tree (
  Text.XML.Expat.Tree.parse, Node(..)
) where

import Text.XML.Expat.IO as EIO
import qualified Data.Tree
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

-- |Simplistic XML tree representation.
data Node = Element { eName :: String, eAttrs :: [(String,String)],
                      eChildren :: [Node] }
          | Text String
          deriving Show

modifyChildren :: ([Node] -> [Node]) -> Node -> Node
modifyChildren f node = node { eChildren = f (eChildren node) }

-- |@parse enc doc@ parses XML content @doc@ with optional encoding @enc@,
-- and returns the root 'Node' of the document if there were no parsing errors.
parse :: Maybe String -> String -> Maybe Node
parse enc doc = unsafePerformIO $ runParse where
  runParse = do
    parser <- EIO.newParser enc
    -- We maintain the invariant that the stack always has one element,
    -- whose only child at the end of parsing is the root of the actual tree.
    stack <- newIORef [Element "" [] []]
    EIO.setStartElementHandler  parser (\n a -> modifyIORef stack (start n a))
    EIO.setEndElementHandler    parser (\n -> modifyIORef stack (end n))
    EIO.setCharacterDataHandler parser (\s -> modifyIORef stack (text s))
    ok <- EIO.parse parser doc True
    if ok
      then do
        [Element _ _ [root]] <- readIORef stack
        return $ Just $ modifyChildren reverse root
      else return Nothing
  start name attrs stack = Element name attrs [] : stack
  text str (cur:rest) = modifyChildren (Text str:) cur : rest
  end name (cur:parent:rest) =
    if eName cur /= name then error "name mismatch" else
    let node = modifyChildren reverse cur in
    modifyChildren (node:) parent : rest

