{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances, FlexibleInstances,
        MultiParamTypeClasses, TypeFamilies #-}

-- hexpat, a Haskell wrapper for expat
-- Copyright (C) 2008 Evan Martin <martine@danga.com>
-- Copyright (C) 2009 Stephen Blackheath <http://blacksapphire.com/antispam>

-- | This module provides functions to parse an XML document to a tree structure,
-- either strictly or lazily.
--
-- The 'GenericXMLString' type class allows you to use any string type. Three
-- string types are provided for here: 'String', 'ByteString' and 'Text'.
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
-- >     let (xml, mErr) = parse defaultParserOptions inputText :: (UNode String, Maybe XMLParseError)
-- >     -- Process document before handling error, so we get lazy processing.
-- >     L.hPutStr stdout $ format xml
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
-- >     let (tree, mError) = parse defaultParserOptions
-- >                    (L.pack $ map c2w $ "<top><banana></apple></top>")
-- >     print (tree :: UNode String)
-- >
-- >     -- Note: We check the error _after_ we have finished our processing
-- >     -- on the tree.
-- >     case mError of
-- >         Just err -> putStrLn $ "It failed : "++show err
-- >         Nothing -> putStrLn "Success!"
--
-- Way no. 2 - Using exceptions
--
-- 'parseThrowing' can throw an exception from pure code, which is generally a bad
-- way to handle errors, because Haskell\'s lazy evaluation means it\'s hard to
-- predict where it will be thrown from.  However, it may be acceptable in
-- situations where it's not expected during normal operation, depending on the
-- design of your program.
--
-- > ...
-- > import Control.Exception.Extensible as E
-- >
-- > -- This is not the recommended way to handle errors.
-- > main = do
-- >     do
-- >         let tree = parseThrowing defaultParserOptions
-- >                        (L.pack $ map c2w $ "<top><banana></apple></top>")
-- >         print (tree :: UNode String)
-- >         -- Because of lazy evaluation, you should not process the tree outside
-- >         -- the 'do' block, or exceptions could be thrown that won't get caught.
-- >     `E.catch` (\exc ->
-- >         case E.fromException exc of
-- >             Just (XMLParseException err) -> putStrLn $ "It failed : "++show err
-- >             Nothing -> E.throwIO exc)

module Text.XML.Expat.Tree (
  -- * Tree structure
  NodeG(..),
  Node,
  Attributes,
  UNode,
  UAttributes,
  textContent,
  isElement,
  isNamed,
  isText,
  getName,
  getAttributes,
  getAttribute,
  getChildren,
  modifyName,
  modifyAttributes,
  setAttribute,
  deleteAttribute,
  alterAttribute,
  modifyChildren,
  mapAllTags,

  -- * Parse to tree
  ParserOptions(..),
  defaultParserOptions,
  Encoding(..),
  parse,
  parse',
  XMLParseError(..),
  XMLParseLocation(..),

  -- * Variant that throws exceptions
  parseThrowing,
  XMLParseException(..),

  -- * SAX-style parse
  SAXEvent(..),
  saxToTree,

  -- * Abstraction of string types
  GenericXMLString(..),

  -- * Deprecated
  eAttrs,
  Nodes,
  UNodes,
  parseTree,
  parseTree',
  parseSAX,
  parseSAXLocations,
  parseTreeThrowing,
  parseSAXThrowing,
  parseSAXLocationsThrowing
) where

------------------------------------------------------------------------------
import Text.XML.Expat.IO hiding (parse,parse')
import qualified Text.XML.Expat.IO as IO
import Text.XML.Expat.SAX ( ParserOptions(..)
                          , XMLParseException(..)
                          , SAXEvent(..)
                          , defaultParserOptions
                          , mkText
                          , parseSAX
                          , parseSAXLocations
                          , parseSAXLocationsThrowing
                          , parseSAXThrowing
                          , GenericXMLString(..) )
import qualified Text.XML.Expat.SAX as SAX
import Text.XML.Expat.NodeClass

------------------------------------------------------------------------------
import Control.Arrow
import Control.Monad (forM, mplus)
import Control.Monad.Identity
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.IORef
import Data.Monoid (Monoid,mconcat)
import Control.Parallel.Strategies
import System.IO.Unsafe
import Foreign.C.String
import Foreign.Ptr


-- | The tree representation of the XML document.
data NodeG c tag text =
    Element {
        eName       :: !tag,
        eAttributes :: ![(tag,text)],
        eChildren   :: c (NodeG c tag text)
    } |
    Text !text

instance (Show tag, Show text) => Show (NodeG [] tag text) where
    show (Element na at ch) = "Element "++show na++" "++show at++" "++show ch
    show (Text t) = "Text "++show t

instance (Eq tag, Eq text) => Eq (NodeG [] tag text) where
    Element na1 at1 ch1 == Element na2 at2 ch2 =
        na1 == na2 &&
        at1 == at2 &&
        ch1 == ch2
    Text t1 == Text t2 = t1 == t2
    _ == _ = False

-- | A pure Node that uses a list as its container type.
type Node = NodeG []

eAttrs :: Node tag text -> [(tag, text)]
{-# DEPRECATED eAttrs "use eAttributes instead" #-}
eAttrs = eAttributes

instance (NFData tag, NFData text) => NFData (NodeG [] tag text) where
    rnf (Element nam att chi) = rnf (nam, att, chi)
    rnf (Text txt) = rnf txt

-- | Type shortcut for attributes
type Attributes tag text = [(tag, text)]

-- | DEPRECATED: Use [Node tag text] instead.
--
-- Type shortcut for nodes.
type Nodes tag text = [Node tag text]
{-# DEPRECATED Nodes "use [Node tag text] instead" #-}

-- | DEPRECATED: Use [UNode text] instead.
--
-- Type shortcut for nodes with unqualified tag names where tag and
-- text are the same string type. Deprecated
type UNodes text = Nodes text text
{-# DEPRECATED UNodes "use [UNode text] instead" #-}

-- | Type shortcut for a single node with unqualified tag names where tag and
-- text are the same string type.
type UNode text = Node text text

-- | Type shortcut for attributes with unqualified names where tag and
-- text are the same string type.
type UAttributes text = Attributes text text

instance NodeClass (NodeG []) where
    type NodeMonad (NodeG []) = Identity
    type NodeContainer (NodeG []) = []

    textContent (Element _ _ children) = mconcat $ map textContent children
    textContent (Text txt) = txt
    
    isElement (Element _ _ _) = True
    isElement _               = False
    
    isText (Text _) = True
    isText _        = False
    
    isNamed _  (Text _) = False
    isNamed nm (Element nm' _ _) = nm == nm'

    getName (Text _)             = gxFromString ""
    getName (Element name _ _)   = name

    getAttributes (Text _)            = []
    getAttributes (Element _ attrs _) = attrs

    getChildren (Text _)         = []
    getChildren (Element _ _ ch) = ch

    modifyName _ node@(Text _) = node
    modifyName f (Element n a c) = Element (f n) a c

    modifyAttributes _ node@(Text _) = node
    modifyAttributes f (Element n a c) = Element n (f a) c

    modifyChildren _ node@(Text _) = node
    modifyChildren f (Element n a c) = Element n a (f c)

    mapAllTags _ (Text t) = Text t
    mapAllTags f (Element n a c) = Element (f n) (map (first f) a) (map (mapAllTags f) c)

    mapElement _ (Text t) = Text t
    mapElement f (Element n a c) =
        let (n', a', c') = f (n, a, c)
        in  Element n' a' c'

setEntityDecoder :: (GenericXMLString tag, GenericXMLString text)
                 => Parser
                 -> IORef [Node tag text]
                 -> (tag -> Maybe text)
                 -> IO ()
setEntityDecoder parser queueRef decoder = do
    setUseForeignDTD parser True
    setExternalEntityRefHandler parser eh
    setSkippedEntityHandler parser skip

  where
    text str (cur:rest) = modifyChildren (Text str:) cur : rest
    text _ [] = undefined

    skip _ 1 = return False
    skip entityName 0 = do
        en <- mkText entityName
        let mbt = decoder en
        maybe (return False)
              (\t -> do
                   modifyIORef queueRef $ text t
                   return True)
              mbt
    skip _ _ = undefined

    eh p ctx _ systemID publicID =
        if systemID == nullPtr && publicID == nullPtr
           then withCStringLen "" $ \c -> do
               parseExternalEntityReference p ctx Nothing c
           else return False


-- | Strictly parse XML to tree. Returns error message or valid parsed tree.
parse' :: (GenericXMLString tag, GenericXMLString text) =>
          ParserOptions tag text  -- ^ Parser options
       -> ByteString              -- ^ Input text (a strict ByteString)
       -> Either XMLParseError (Node tag text)
parse' opts doc = unsafePerformIO $ runParse where
  runParse = do
    let enc = parserEncoding opts
    let mEntityDecoder = entityDecoder opts

    parser <- newParser enc

    -- We maintain the invariant that the stack always has one element,
    -- whose only child at the end of parsing is the root of the actual tree.
    let emptyString = gxFromString ""
    stack <- newIORef [Element emptyString [] []]

    maybe (return ())
          (setEntityDecoder parser stack)
          mEntityDecoder

    setStartElementHandler parser $ \cName cAttrs -> do
        name <- mkText cName
        attrs <- forM cAttrs $ \(cAttrName,cAttrValue) -> do
            attrName <- mkText cAttrName
            attrValue <- mkText cAttrValue
            return (attrName, attrValue)
        modifyIORef stack (start name attrs)
        return True
    setEndElementHandler parser $ \_ -> do
        modifyIORef stack end
        return True
    setCharacterDataHandler parser $ \cText -> do
        txt <- gxFromCStringLen cText
        modifyIORef stack (text txt)
        return True
    mError <- IO.parse' parser doc
    case mError of
        Just err -> return $ Left err
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
  impossible = error "parse' impossible"


-- | DEPRECATED: use 'parse' instead.
--
-- Strictly parse XML to tree. Returns error message or valid parsed tree.
parseTree' :: (GenericXMLString tag, GenericXMLString text) =>
              Maybe Encoding      -- ^ Optional encoding override
           -> ByteString          -- ^ Input text (a strict ByteString)
           -> Either XMLParseError (Node tag text)
{-# DEPRECATED parseTree' "use Text.XML.Expat.parse' instead" #-}
parseTree' enc = parse' (ParserOptions enc Nothing)


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
    ptl (StartElement name attrs:rema) =
        let (children, err1, rema') = ptl rema
            elt = Element name attrs children
            (out, err2, rema'') = ptl rema'
        in  (elt:out, err1 `mplus` err2, rema'')
    ptl (EndElement _:rema) = ([], Nothing, rema)
    ptl (CharacterData txt:rema) =
        let (out, err, rema') = ptl rema
        in  (Text txt:out, err, rema')
    ptl (FailDocument err:_) = ([], Just err, [])
    ptl [] = ([], Nothing, [])


-- | Lazily parse XML to tree. Note that forcing the XMLParseError return value
-- will force the entire parse.  Therefore, to ensure lazy operation, don't
-- check the error status until you have processed the tree.
parse :: (GenericXMLString tag, GenericXMLString text) =>
         ParserOptions tag text   -- ^ Parser options
      -> L.ByteString             -- ^ Input text (a lazy ByteString)
      -> (Node tag text, Maybe XMLParseError)
parse opts bs = saxToTree $ SAX.parse opts bs


-- | DEPREACTED: Use 'parse' instead.
--
-- Lazily parse XML to tree. Note that forcing the XMLParseError return value
-- will force the entire parse.  Therefore, to ensure lazy operation, don't
-- check the error status until you have processed the tree.
parseTree :: (GenericXMLString tag, GenericXMLString text) =>
             Maybe Encoding      -- ^ Optional encoding override
          -> L.ByteString        -- ^ Input text (a lazy ByteString)
          -> (Node tag text, Maybe XMLParseError)
{-# DEPRECATED parseTree "use Text.XML.Expat.Tree.parse instead" #-}
parseTree mEnc = parse (ParserOptions mEnc Nothing)


-- | Lazily parse XML to tree. In the event of an error, throw 'XMLParseException'.
--
-- @parseThrowing@ can throw an exception from pure code, which is generally a bad
-- way to handle errors, because Haskell\'s lazy evaluation means it\'s hard to
-- predict where it will be thrown from.  However, it may be acceptable in
-- situations where it's not expected during normal operation, depending on the
-- design of your program.
parseThrowing :: (GenericXMLString tag, GenericXMLString text) =>
         ParserOptions tag text -- ^ Parser options
      -> L.ByteString           -- ^ Input text (a lazy ByteString)
      -> Node tag text
parseThrowing opts bs = fst $ saxToTree $ SAX.parseThrowing opts bs


-- | DEPRECATED: Use 'parseThrowing' instead.
--
-- Lazily parse XML to tree. In the event of an error, throw 'XMLParseException'.
parseTreeThrowing :: (GenericXMLString tag, GenericXMLString text) =>
             Maybe Encoding      -- ^ Optional encoding override
          -> L.ByteString        -- ^ Input text (a lazy ByteString)
          -> Node tag text
{-# DEPRECATED parseTreeThrowing "use Text.XML.Expat.Tree.parseThrowing instead" #-}
parseTreeThrowing mEnc = parseThrowing (ParserOptions mEnc Nothing)
