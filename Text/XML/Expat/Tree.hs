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
  Encoding(..),
  parseTree,
  parseTree',
  XMLParseError(..),
  SAXEvent(..),
  parseSAX,
  TreeFlavor(..),
  stringFlavor,
  byteStringFlavor,
  textFlavor
) where

import Text.XML.Expat.IO
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Data.ByteString.Internal (c2w, w2c, c_strlen)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Codec.Binary.UTF8.String as U8
import Data.Binary.Put
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Parallel.Strategies
import Control.Monad
import System.IO.Unsafe
import Foreign.C.String


data TreeFlavor tag text = TreeFlavor
        (CString -> IO tag)
        (CStringLen -> IO text)
        (tag -> Put)
        (text -> B.ByteString)

stringFlavor :: TreeFlavor String String
stringFlavor = TreeFlavor unpack unpackLen (mapM_ (putWord8 . c2w)) pack
  where
    unpack    cstr = U8.decodeString <$> peekCString cstr
    unpackLen cstr = U8.decodeString <$> peekCStringLen cstr
    pack = B.pack . map c2w . U8.encodeString

byteStringFlavor :: TreeFlavor B.ByteString B.ByteString
byteStringFlavor = TreeFlavor unpack unpackLen putByteString id
  where
    unpack    cstr = peekByteString cstr
    unpackLen cstr = peekByteStringLen cstr

textFlavor :: TreeFlavor T.Text T.Text
textFlavor = TreeFlavor unpack unpackLen (putByteString . TE.encodeUtf8) TE.encodeUtf8
  where
    unpack    cstr = TE.decodeUtf8 <$> peekByteString cstr
    unpackLen cstr = TE.decodeUtf8 <$> peekByteStringLen cstr

-- |Tree representation. Everything is strict except for eChildren.
data Node tag text =
    Element {
        eName     :: !tag,
        eAttrs    :: ![(tag,text)],
        eChildren :: [Node tag text]
    } |
    Text !text
    deriving (Eq, Show)

instance (NFData tag, NFData text) => NFData (Node tag text) where
    rnf (Element nam att chi) = rnf (nam, att, chi)
    rnf (Text txt) = rnf txt

modifyChildren :: ([Node tag text] -> [Node tag text])
               -> Node tag text
               -> Node tag text
modifyChildren f node = node { eChildren = f (eChildren node) }

-- | Strictly parse XML to tree. Returns error message or valid parsed tree.
parseTree' :: Eq tag =>
              TreeFlavor tag text
           -> Maybe Encoding  -- ^ Optional encoding override
           -> L.ByteString
           -> Either XMLParseError (Node tag text)
parseTree' (TreeFlavor mkTag mkText _ _) enc doc = unsafePerformIO $ runParse where
  runParse = do
    parser <- newParser enc
    -- We maintain the invariant that the stack always has one element,
    -- whose only child at the end of parsing is the root of the actual tree.
    emptyString <- withCString "" mkTag 
    stack <- newIORef [Element emptyString [] []]
    setStartElementHandler  parser $ \cName cAttrs -> do
        name <- mkTag cName
        attrs <- forM cAttrs $ \(cAttrName,cAttrValue) -> do
            attrName <- mkTag cAttrName
            len <- c_strlen cAttrValue
            attrValue <- mkText (cAttrValue, fromIntegral len)
            return (attrName, attrValue)
        modifyIORef stack (start name attrs)
    setEndElementHandler parser $ \cName -> do
        name <- mkTag cName
        modifyIORef stack (end name)
    setCharacterDataHandler parser $ \cText -> do
        txt <- mkText cText
        modifyIORef stack (text txt)
    mError <- parse parser doc
    case mError of
        Just error -> return $ Left error
        Nothing -> do
            [Element _ _ [root]] <- readIORef stack
            return $ Right root
            
  start name attrs stack = Element name attrs [] : stack
  text str (cur:rest) = modifyChildren (Text str:) cur : rest
  end name (cur:parent:rest) =
    if eName cur /= name then error "name mismatch" else
    let node = modifyChildren reverse cur in
    modifyChildren (node:) parent : rest

data SAXEvent tag text =
    StartElement tag [(tag, text)] |
    EndElement tag |
    CharacterData text |
    FailDocument XMLParseError
    deriving (Eq, Show)

-- | Lazily parse XML to SAX events.
parseSAX :: TreeFlavor tag text
         -> Maybe Encoding  -- ^ Optional encoding override
         -> L.ByteString
         -> [SAXEvent tag text]
parseSAX (TreeFlavor mkTag mkText _ _) enc doc = unsafePerformIO $ do
    events <- newEmptyMVar
    forkIO $ runParser events

    let readEvents = do
            mEvent <- takeMVar events
            case mEvent of
                Just event@(FailDocument err) ->
                    return [event]
                Just event -> do
                    rem <- unsafeInterleaveIO readEvents
                    return (event:rem)
                Nothing ->
                    return []
    readEvents
  where
    runParser events = do
        parser <- newParser enc
        setStartElementHandler parser $ \cName cAttrs -> do
            name <- mkTag cName
            attrs <- forM cAttrs $ \(cAttrName,cAttrValue) -> do
                attrName <- mkTag cAttrName
                len <- c_strlen cAttrValue
                attrValue <- mkText (cAttrValue, fromIntegral len)
                return (attrName, attrValue)
            putMVar events $ Just $ StartElement name attrs
        setEndElementHandler parser $ \cName -> do
            name <- mkTag cName
            putMVar events $ Just $ EndElement name
        setCharacterDataHandler parser $ \cText -> do
            txt <- mkText cText
            putMVar events $ Just $ CharacterData txt
        mError <- parse parser doc
        case mError of
            Nothing -> putMVar events Nothing
            Just err -> putMVar events (Just $ FailDocument err)

-- | Lazily parse XML to tree. Note that forcing the XMLParseError return value
-- will force the entire parse.  Therefore, to ensure lazy operation, don't
-- check the error status until you have processed the tree.
parseTree :: TreeFlavor tag text
          -> Maybe Encoding  -- ^ Optional encoding override
          -> L.ByteString
          -> (Node tag text, Maybe XMLParseError)
parseTree flavor@(TreeFlavor mkTag _ _ _) mEnc bs =
    let events = parseSAX flavor mEnc bs
        (nodes, mError, _) = ptl events
    in  (safeHead nodes, mError)
  where
    safeHead (a:_) = a
    safeHead [] = Element (unsafePerformIO $ withCString "" mkTag) [] []
    ptl (StartElement name attrs:rem) =
        let (children, err1, rem') = ptl rem
            elt = Element name attrs children
            (out, err2, rem'') = ptl rem'
        in  (elt:out, err1 `mplus` err2, rem'')
    ptl (EndElement name:rem) = ([], Nothing, rem)
    ptl (CharacterData txt:rem) =
        let (out, err, rem') = ptl rem
        in  (Text txt:out, err, rem')
    ptl (FailDocument err:_) = ([], Just err, [])
    ptl [] = ([], Nothing, [])

