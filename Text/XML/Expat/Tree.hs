-- hexpat, a Haskell wrapper for expat
-- Copyright (C) 2008 Evan Martin <martine@danga.com>
-- Copyright (C) 2009 Stephen Blackheath <http://blacksapphire.com/antispam>

-- |The Expat.Tree module provides a simplified interface to parsing, that
-- returns a tree of the XML structure.  It is written using the lower-level
-- bindings in the "Text.XML.Expat.IO" module.  (Note that this is not a lazy
-- parse of the document: as soon as the root node is accessed, the entire
-- document is parsed.)

module Text.XML.Expat.Tree (
  Node(..),
  parseTree,
  parseTreeLazy,
  parseSAX,
  TreeFlavour(..),
  stringFlavour,
  byteStringFlavour,
  textFlavour
) where

import Text.XML.Expat.IO
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Data.ByteString.Internal (c2w, w2c)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Codec.Binary.UTF8.String as U8
import Data.Binary.Put
import Control.Concurrent
import Control.Concurrent.MVar
import System.IO.Unsafe


data TreeFlavour tag text = TreeFlavour
        (B.ByteString -> tag)
        (B.ByteString -> text)
        (tag -> Put)
        (text -> B.ByteString)

stringFlavour :: TreeFlavour String String
stringFlavour = TreeFlavour unpack unpack (mapM_ (putWord8 . c2w)) pack
  where
    unpack = U8.decodeString . map w2c . B.unpack
    pack = B.pack . map c2w . U8.encodeString

byteStringFlavour :: TreeFlavour B.ByteString B.ByteString
byteStringFlavour = TreeFlavour id id putByteString id

textFlavour :: TreeFlavour T.Text T.Text
textFlavour = TreeFlavour TE.decodeUtf8 TE.decodeUtf8 (putByteString . TE.encodeUtf8) TE.encodeUtf8

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
             TreeFlavour tag text
          -> Maybe Encoding
          -> L.ByteString
          -> Maybe (Node tag text)
parseTree (TreeFlavour mkTag mkText _ _) enc doc = unsafePerformIO $ runParse where
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

data SAXEvent tag text =
    StartDocument |
    StartElement tag [(tag, text)] |
    EndElement tag |
    CharacterData text |
    EndDocument |
    FailDocument
    deriving (Eq, Show)

parseSAX :: TreeFlavour tag text
         -> Maybe Encoding
         -> L.ByteString
         -> [SAXEvent tag text]
parseSAX (TreeFlavour mkTag mkText _ _) enc doc = unsafePerformIO $ do
    events <- newEmptyMVar
    forkIO $ runParser events

    let readEvents = do
            event <- takeMVar events
            rem <- case event of
                EndDocument -> return []
                FailDocument -> return []
                otherwise ->   unsafeInterleaveIO readEvents
            return (event:rem)
    readEvents
  where
    runParser events = do
        putMVar events StartDocument
        parser <- newParser enc
        setStartElementHandler  parser $ \n a ->
            putMVar events $ StartElement (mkTag n) (map (\(n,v) -> (mkTag n, mkText v)) a)
        setEndElementHandler    parser $ \n ->
            putMVar events $ EndElement (mkTag n)
        setCharacterDataHandler parser $ \s ->
            putMVar events $ CharacterData (mkText s)
        ok <- parse parser doc
        if ok
          then putMVar events EndDocument
          else putMVar events FailDocument

-- |@parse enc doc@ parses /lazy/ bytestring XML content @doc@ with optional
-- encoding override @enc@ and returns the root 'Node' of the document if there
-- were no parsing errors.
parseTreeLazy :: TreeFlavour tag text
              -> Maybe Encoding
              -> L.ByteString
              -> Node tag text
parseTreeLazy flavour mEnc bs = head . fst . ptl $ parseSAX flavour mEnc bs
  where
    ptl (StartDocument:rem) = ptl rem
    ptl (StartElement name attrs:rem) =
        let (children, rem') = ptl rem
            (out, rem'') = ptl rem'
        in  (Element name attrs children:out, rem'')
    ptl (EndElement name:rem) = ([], rem)
    ptl (CharacterData txt:rem) = let (out, rem') = ptl rem in (Text txt:out, rem')
    ptl otherwise = ([], [])

