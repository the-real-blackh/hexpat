{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances #-}

-- hexpat, a Haskell wrapper for expat
-- Copyright (C) 2008 Evan Martin <martine@danga.com>
-- Copyright (C) 2009 Stephen Blackheath <http://blacksapphire.com/antispam>

-- | This module provides functions to parse an XML document to a tree structure,
-- either strictly or lazily, as well as a lazy SAX-style interface.
--
-- The GenericXMLString type class allows you to use any string type. Three
-- string types are provided for here: @String@, @ByteString@ and @Text@.
--
-- Here is a complete example to get you started:
--
-- > -- | A "hello world" example of hexpat that lazily parses a document, printing
-- > -- it to standard out.
-- >
-- > import Text.XML.Expat.Tree
-- > import Text.XML.Expat.Format
-- > import System.Environment
-- > import System.Exit
-- > import System.IO
-- > import qualified Data.ByteString.Lazy as L
-- >
-- > main = do
-- >     args <- getArgs
-- >     case args of
-- >         [filename] -> process filename
-- >         otherwise  -> do
-- >             hPutStrLn stderr "Usage: helloworld <file.xml>"
-- >             exitWith $ ExitFailure 1
-- >
-- > process :: String -> IO ()
-- > process filename = do
-- >     inputText <- L.readFile filename
-- >     -- Note: Because we're not using the tree, Haskell can't infer the type of
-- >     -- strings we're using so we need to tell it explicitly with a type signature.
-- >     let (xml, mErr) = parseTree Nothing inputText :: (UNode String, Maybe XMLParseError)
-- >     -- Process document before handling error, so we get lazy processing.
-- >     L.hPutStr stdout $ formatTree xml
-- >     putStrLn ""
-- >     case mErr of
-- >         Nothing -> return ()
-- >         Just err -> do
-- >             hPutStrLn stderr $ "XML parse failed: "++show err
-- >             exitWith $ ExitFailure 2
--
-- Error handling in strict parses is very straight forward - just check the
-- 'Either' return value.  Lazy parses are not so simple.  Here are two working
-- examples that illustrate the ways to handle errors.  Here they are:
--
-- Way no. 1 - Using a Maybe value
--
-- > import Text.XML.Expat.Tree
-- > import qualified Data.ByteString.Lazy as L
-- > import Data.ByteString.Internal (c2w)
-- >
-- > -- This is the recommended way to handle errors in lazy parses
-- > main = do
-- >     let (tree, mError) = parseTree Nothing (L.pack $ map c2w $ "<top><banana></apple></top>")
-- >     print (tree :: UNode String)
-- >     -- Note: We check the error _after_ we have finished our processing on the tree.
-- >     case mError of
-- >         Just err -> putStrLn $ "It failed : "++show err
-- >         Nothing -> putStrLn "Success!"
--
-- Way no. 2 - Using exceptions
--
-- Unless exceptions fit in with the design of your program, this way is less preferred.
--
-- > ...
-- > import Control.Exception.Extensible as E
-- >
-- > -- This is not the recommended way to handle errors.
-- > main = do
-- >     do
-- >         let tree = parseTreeThrowing Nothing (L.pack $ map c2w $ "<top><banana></apple></top>")
-- >         print (tree :: UNode String)
-- >         -- Because of lazy evaluation, you should not process the tree outside the 'do' block,
-- >         -- or exceptions could be thrown that won't get caught.
-- >     `E.catch` (\exc ->
-- >         case E.fromException exc of
-- >             Just (XMLParseException err) -> putStrLn $ "It failed : "++show err
-- >             Nothing -> E.throwIO exc)

module Text.XML.Expat.Tree (
  -- * Tree structure
  Node(..),
  Nodes,
  Attributes,
  UNode,
  UNodes,
  UAttributes,
  textContent,
  extractText,
  isElement,
  isNamed,
  isText,
  getAttribute,
  getChildren,
  modifyChildren,

  -- * Parse to tree
  parseTree,
  parseTree',
  Encoding(..),
  XMLParseError(..),
  XMLParseLocation(..),
  -- * SAX-style parse
  parseSAX,
  SAXEvent(..),
  saxToTree,
  parseSAXLocations,
  -- * Variants that throw exceptions
  XMLParseException(..),
  parseTreeThrowing,
  parseSAXThrowing,
  parseSAXLocationsThrowing,
  -- * Abstraction of string types
  GenericXMLString(..)
) where

import Text.XML.Expat.IO
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal as I
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Data.ByteString.Internal (c2w, w2c, c_strlen)
import qualified Data.Monoid as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Codec.Binary.UTF8.String as U8
import Data.Monoid
import Data.Typeable
import Control.Exception.Extensible as Exc
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Parallel.Strategies
import Control.Monad
import System.IO.Unsafe
import System.Mem.Weak
import Foreign.C.String
import Foreign.Ptr


-- | An abstraction for any string type you want to use as xml text (that is,
-- attribute values or element text content). If you want to use a
-- new string type with /hexpat/, you must make it an instance of
-- 'GenericXMLString'.
class (M.Monoid s, Eq s) => GenericXMLString s where
    gxNullString :: s -> Bool
    gxToString :: s -> String
    gxFromString :: String -> s
    gxFromChar :: Char -> s
    gxHead :: s -> Char
    gxTail :: s -> s
    gxBreakOn :: Char -> s -> (s, s)
    gxFromCStringLen :: CStringLen -> IO s
    gxToByteString :: s -> B.ByteString

instance GenericXMLString String where
    gxNullString = null
    gxToString = id
    gxFromString = id
    gxFromChar c = [c]
    gxHead = head
    gxTail = tail
    gxBreakOn c = break (==c)
    gxFromCStringLen cstr = U8.decodeString <$> peekCStringLen cstr
    gxToByteString = B.pack . map c2w . U8.encodeString

instance GenericXMLString B.ByteString where
    gxNullString = B.null
    gxToString = U8.decodeString . map w2c . B.unpack
    gxFromString = B.pack . map c2w . U8.encodeString
    gxFromChar = B.singleton . c2w
    gxHead = w2c . B.head
    gxTail = B.tail
    gxBreakOn c = B.break (== c2w c)
    gxFromCStringLen = peekByteStringLen
    gxToByteString = id

instance GenericXMLString T.Text where
    gxNullString = T.null
    gxToString = T.unpack
    gxFromString = T.pack
    gxFromChar = T.singleton
    gxHead = T.head
    gxTail = T.tail
    gxBreakOn c = T.breakBy (==c)
    gxFromCStringLen cstr = TE.decodeUtf8 <$> peekByteStringLen cstr
    gxToByteString = TE.encodeUtf8

peekByteStringLen :: CStringLen -> IO B.ByteString
{-# INLINE peekByteStringLen #-}
peekByteStringLen (cstr, len) =
    I.create (fromIntegral len) $ \ptr ->
        I.memcpy ptr (castPtr cstr) (fromIntegral len)


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

-- | Type shortcut for attributes
type Attributes tag text = [(tag, text)]

-- | Type shortcut for nodes
type Nodes tag text = [Node tag text]

-- | Type shortcut for nodes with unqualified tag names where tag and
-- text are the same string type.
type UNodes text = Nodes text text

-- | Type shortcut for a single node with unqualified tag names where tag and
-- text are the same string type.
type UNode text = Node text text

-- | Type shortcut for attributes with unqualified names where tag and
-- text are the same string type.
type UAttributes text = Attributes text text

-- | Extract all text content from inside a tag into a single string, including
-- any text contained in children.
textContent :: Monoid text => Node tag text -> text
textContent (Element _ _ children) = mconcat $ map textContent children
textContent (Text txt) = txt

-- | Deprecated - renamed to textContent.
extractText :: Monoid text => Node tag text -> text
{-# DEPRECATED extractText "renamed to textContent" #-}
extractText = textContent

-- | Is the given node an element?
isElement :: Node tag text -> Bool
isElement (Element _ _ _) = True
isElement _               = False

-- | Is the given node text?
isText :: Node tag text -> Bool
isText (Text _) = True
isText _        = False

-- | Is the given node a tag with the given name?
isNamed :: (Eq tag) => tag -> Node tag text -> Bool
isNamed _  (Text _) = False
isNamed nm (Element nm' _ _) = nm == nm'

-- | Get the value of the attribute having the specified name.
getAttribute :: GenericXMLString tag => Node tag text -> tag -> Maybe text
getAttribute n t = lookup t $ eAttrs n

-- | Get children of a node if it's an element, return empty list otherwise.
getChildren :: Node tag text -> [Node tag text]
getChildren (Text _)         = []
getChildren (Element _ _ ch) = ch

-- | Modify a node's children using the specified function.
modifyChildren :: ([Node tag text] -> [Node tag text])
               -> Node tag text
               -> Node tag text
modifyChildren f node@(Text _) = node
modifyChildren f (Element n a c) = Element n a (f c)

mkText :: GenericXMLString text => CString -> IO text
{-# INLINE mkText #-}
mkText cstr = do
    len <- c_strlen cstr
    gxFromCStringLen (cstr, fromIntegral len)

-- | Strictly parse XML to tree. Returns error message or valid parsed tree.
parseTree' :: (GenericXMLString tag, GenericXMLString text) =>
              Maybe Encoding      -- ^ Optional encoding override
           -> B.ByteString        -- ^ Input text (a strict ByteString)
           -> Either XMLParseError (Node tag text)
parseTree' enc doc = unsafePerformIO $ runParse where
  runParse = do
    parser <- newParser enc
    -- We maintain the invariant that the stack always has one element,
    -- whose only child at the end of parsing is the root of the actual tree.
    let emptyString = gxFromString ""
    stack <- newIORef [Element emptyString [] []]
    setStartElementHandler parser $ \cName cAttrs -> do
        name <- mkText cName
        attrs <- forM cAttrs $ \(cAttrName,cAttrValue) -> do
            attrName <- mkText cAttrName
            attrValue <- mkText cAttrValue
            return (attrName, attrValue)
        modifyIORef stack (start name attrs)
        return True
    setEndElementHandler parser $ \cName -> do
        modifyIORef stack end
        return True
    setCharacterDataHandler parser $ \cText -> do
        txt <- gxFromCStringLen cText
        modifyIORef stack (text txt)
        return True
    mError <- parse' parser doc
    case mError of
        Just error -> return $ Left error
        Nothing -> do
            [Element _ _ [root]] <- readIORef stack
            return $ Right root

  start name attrs stack = Element name attrs [] : stack
  text str (cur:rest) = modifyChildren (Text str:) cur : rest
  text _ [] = impossible
  end (cur:parent:rest) =
    let node = modifyChildren reverse cur in
    modifyChildren (node:) parent : rest
  end _ = impossible
  impossible = error "parseTree' impossible"

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
parseSAX :: (GenericXMLString tag, GenericXMLString text) =>
            Maybe Encoding      -- ^ Optional encoding override
         -> L.ByteString        -- ^ Input text (a lazy ByteString)
         -> [SAXEvent tag text]
parseSAX enc input = unsafePerformIO $ do
    parser <- newParser enc
    queueRef <- newIORef []
    setStartElementHandler parser $ \cName cAttrs -> do
        name <- mkText cName
        attrs <- forM cAttrs $ \(cAttrName,cAttrValue) -> do
            attrName <- mkText cAttrName
            attrValue <- mkText cAttrValue
            return (attrName, attrValue)
        modifyIORef queueRef (StartElement name attrs:)
        return True
    setEndElementHandler parser $ \cName -> do
        name <- mkText cName
        modifyIORef queueRef (EndElement name:)
        return True
    setCharacterDataHandler parser $ \cText -> do
        txt <- gxFromCStringLen cText
        modifyIORef queueRef (CharacterData txt:)
        return True

    let runParser input = unsafeInterleaveIO $ do
            rem <- case input of
                (c:cs) -> do
                    mError <- parseChunk parser c False
                    case mError of
                        Just error -> return [FailDocument error]
                        Nothing -> runParser cs
                [] -> do
                    mError <- parseChunk parser B.empty True
                    case mError of
                        Just error -> return [FailDocument error]
                        Nothing -> return []
            queue <- readIORef queueRef
            writeIORef queueRef []
            return $ reverse queue ++ rem

    runParser $ L.toChunks input

-- | An exception indicating an XML parse error, used by the /..Throwing/ variants.
data XMLParseException = XMLParseException XMLParseError
    deriving (Eq, Show, Typeable)

instance Exception XMLParseException where

-- | A variant of parseSAX that gives a document location with each SAX event.
parseSAXLocations :: (GenericXMLString tag, GenericXMLString text) =>
            Maybe Encoding      -- ^ Optional encoding override
         -> L.ByteString        -- ^ Input text (a lazy ByteString)
         -> [(SAXEvent tag text, XMLParseLocation)]
parseSAXLocations enc input = unsafePerformIO $ do
    -- Done with cut & paste coding for maximum speed.
    parser <- newParser enc
    queueRef <- newIORef []
    setStartElementHandler parser $ \cName cAttrs -> do
        name <- mkText cName
        attrs <- forM cAttrs $ \(cAttrName,cAttrValue) -> do
            attrName <- mkText cAttrName
            attrValue <- mkText cAttrValue
            return (attrName, attrValue)
        loc <- getParseLocation parser
        modifyIORef queueRef ((StartElement name attrs,loc):)
        return True
    setEndElementHandler parser $ \cName -> do
        name <- mkText cName
        loc <- getParseLocation parser
        modifyIORef queueRef ((EndElement name, loc):)
        return True
    setCharacterDataHandler parser $ \cText -> do
        txt <- gxFromCStringLen cText
        loc <- getParseLocation parser
        modifyIORef queueRef ((CharacterData txt, loc):)
        return True

    let runParser [] = return []
        runParser (c:cs) = unsafeInterleaveIO $ do
            mError <- parseChunk parser c (null cs)
            queue <- readIORef queueRef
            writeIORef queueRef []
            rem <- case mError of
                Just error -> do
                    loc <- getParseLocation parser
                    return [(FailDocument error, loc)]
                Nothing -> runParser cs
            return $ reverse queue ++ rem

    runParser $ L.toChunks input

-- | Lazily parse XML to SAX events. In the event of an error, throw 'XMLParseException'.
parseSAXThrowing :: (GenericXMLString tag, GenericXMLString text) =>
                    Maybe Encoding      -- ^ Optional encoding override
                 -> L.ByteString        -- ^ Input text (a lazy ByteString)
                 -> [SAXEvent tag text]
parseSAXThrowing mEnc bs = map freakOut $ parseSAX mEnc bs
  where
    freakOut (FailDocument err) = Exc.throw $ XMLParseException err
    freakOut other = other

-- | A variant of parseSAX that gives a document location with each SAX event.
-- In the event of an error, throw 'XMLParseException'.
parseSAXLocationsThrowing :: (GenericXMLString tag, GenericXMLString text) =>
                             Maybe Encoding      -- ^ Optional encoding override
                          -> L.ByteString        -- ^ Input text (a lazy ByteString)
                          -> [(SAXEvent tag text, XMLParseLocation)]
parseSAXLocationsThrowing mEnc bs = map freakOut $ parseSAXLocations mEnc bs
  where
    freakOut (FailDocument err, _) = Exc.throw $ XMLParseException err
    freakOut other = other

-- | A lower level function that lazily converts a SAX stream into a tree structure.
saxToTree :: GenericXMLString tag =>
             [SAXEvent tag text]
          -> (Node tag text, Maybe XMLParseError)
saxToTree events =
    let (nodes, mError, _) = ptl events
    in  (safeHead nodes, mError)
  where
    safeHead (a:_) = a
    safeHead [] = Element (gxFromString "") [] []
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

-- | Lazily parse XML to tree. Note that forcing the XMLParseError return value
-- will force the entire parse.  Therefore, to ensure lazy operation, don't
-- check the error status until you have processed the tree.
parseTree :: (GenericXMLString tag, GenericXMLString text) =>
             Maybe Encoding      -- ^ Optional encoding override
          -> L.ByteString        -- ^ Input text (a lazy ByteString)
          -> (Node tag text, Maybe XMLParseError)
parseTree mEnc bs = saxToTree $ parseSAX mEnc bs

-- | Lazily parse XML to tree. In the event of an error, throw 'XMLParseException'.
parseTreeThrowing :: (GenericXMLString tag, GenericXMLString text) =>
             Maybe Encoding      -- ^ Optional encoding override
          -> L.ByteString        -- ^ Input text (a lazy ByteString)
          -> Node tag text
parseTreeThrowing mEnc bs = fst $ saxToTree $ parseSAXThrowing mEnc bs
