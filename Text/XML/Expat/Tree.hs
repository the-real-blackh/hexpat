-- hexpat, a Haskell wrapper for expat
-- Copyright (C) 2008 Evan Martin <martine@danga.com>
-- Copyright (C) 2009 Stephen Blackheath <http://blacksapphire.com/antispam>

-- | This module provides functions to parse an XML document to a tree structure,
-- either strictly or lazily, as well as a lazy SAX-style interface.
--
-- Extensible \"flavors\" give you the ability to use any string type. Three
-- are provided here: String, ByteString and Text.

module Text.XML.Expat.Tree (
  -- * Tree structure
  Node(..),
  -- * Parse to tree
  parseTree,
  parseTree',
  Encoding(..),
  XMLParseError(..),
  -- * SAX-style parse
  parseSAX,
  SAXEvent(..),
  -- * Flavors
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

-- | Flavor for String data type.
stringFlavor :: TreeFlavor String String
stringFlavor = TreeFlavor unpack unpackLen (mapM_ (putWord8 . c2w)) pack
  where
    unpack    cstr = U8.decodeString <$> peekCString cstr
    unpackLen cstr = U8.decodeString <$> peekCStringLen cstr
    pack = B.pack . map c2w . U8.encodeString

-- | Flavor for ByteString data type, containing UTF-8 encoded Unicode.
byteStringFlavor :: TreeFlavor B.ByteString B.ByteString
byteStringFlavor = TreeFlavor unpack unpackLen putByteString id
  where
    unpack    cstr = peekByteString cstr
    unpackLen cstr = peekByteStringLen cstr

-- | Flavor for Text data type.
textFlavor :: TreeFlavor T.Text T.Text
textFlavor = TreeFlavor unpack unpackLen (putByteString . TE.encodeUtf8) TE.encodeUtf8
  where
    unpack    cstr = TE.decodeUtf8 <$> peekByteString cstr
    unpackLen cstr = TE.decodeUtf8 <$> peekByteStringLen cstr

-- | The tree representation of the XML document.
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
              TreeFlavor tag text -- ^ Flavor, which determines the string type to use in the output
           -> Maybe Encoding      -- ^ Optional encoding override
           -> L.ByteString        -- ^ Input text (a lazy ByteString)
           -> Either XMLParseError (Node tag text)
{-# SPECIALIZE parseTree' :: TreeFlavor String String -> Maybe Encoding
               -> L.ByteString -> Either XMLParseError (Node String String) #-}
{-# SPECIALIZE parseTree' :: TreeFlavor B.ByteString B.ByteString -> Maybe Encoding
               -> L.ByteString -> Either XMLParseError (Node B.ByteString B.ByteString) #-}
{-# SPECIALIZE parseTree' :: TreeFlavor T.Text T.Text -> Maybe Encoding
               -> L.ByteString -> Either XMLParseError (Node T.Text T.Text) #-}
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
    
instance (NFData tag, NFData text) => NFData (SAXEvent tag text) where
    rnf (StartElement tag atts) = rnf (tag, atts)
    rnf (EndElement tag) = rnf tag
    rnf (CharacterData text) = rnf text
    rnf (FailDocument err) = rnf err

-- | Lazily parse XML to SAX events. In the event of an error, FailDocument is
-- the last element of the output list.
parseSAX :: TreeFlavor tag text -- ^ Flavor, which determines the string type to use in the output
         -> Maybe Encoding      -- ^ Optional encoding override
         -> L.ByteString        -- ^ Input text (a lazy ByteString)
         -> [SAXEvent tag text]
{-# SPECIALIZE parseSAX :: TreeFlavor String String -> Maybe Encoding
         -> L.ByteString -> [SAXEvent String String] #-}
{-# SPECIALIZE parseSAX :: TreeFlavor B.ByteString B.ByteString -> Maybe Encoding
         -> L.ByteString -> [SAXEvent B.ByteString B.ByteString] #-}
{-# SPECIALIZE parseSAX :: TreeFlavor T.Text T.Text -> Maybe Encoding
         -> L.ByteString -> [SAXEvent T.Text T.Text] #-}
parseSAX (TreeFlavor mkTag mkText _ _) enc doc = unsafePerformIO $ do
    events <- newEmptyMVar
    parserThread <- forkIO $ runParser events

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
parseTree :: TreeFlavor tag text -- ^ Flavor, which determines the string type to use in the tree
          -> Maybe Encoding      -- ^ Optional encoding override
          -> L.ByteString        -- ^ Input text (a lazy ByteString)
          -> (Node tag text, Maybe XMLParseError)
{-# SPECIALIZE parseTree :: TreeFlavor String String -> Maybe Encoding
          -> L.ByteString -> (Node String String, Maybe XMLParseError) #-}
{-# SPECIALIZE parseTree :: TreeFlavor B.ByteString B.ByteString -> Maybe Encoding
          -> L.ByteString -> (Node B.ByteString B.ByteString, Maybe XMLParseError) #-}
{-# SPECIALIZE parseTree :: TreeFlavor T.Text T.Text -> Maybe Encoding
          -> L.ByteString -> (Node T.Text T.Text, Maybe XMLParseError) #-}
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

