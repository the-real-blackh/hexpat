{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies,
        FlexibleContexts #-}
-- | A variant of /Node/ in which Element nodes have an annotation of any type,
-- and some concrete functions that annotate with the XML parse location.
--
-- The names conflict with those in /Tree/ so you must use qualified import
-- if you want to use both modules.
module Text.XML.Expat.Annotated (
  -- * Tree structure
  NodeG(..),
  Node,
  Attributes,  -- re-export from Tree
  UNode,
  UAttributes,
  LNode,
  ULNode,
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

  -- * Annotation-specific
  unannotate,
  modifyAnnotation,
  mapAnnotation,

  -- * Qualified nodes
  QName(..),
  QNode,
  QAttributes,
  QLNode,
  toQualified,
  fromQualified,

  -- * Namespaced nodes
  NName (..),
  NNode,
  NAttributes,
  NLNode,
  mkNName,
  mkAnNName,
  toNamespaced,
  fromNamespaced,
  xmlnsUri,
  xmlns,

  -- * Parse to tree
  Tree.ParserOptions(..),
  Tree.defaultParserOptions,
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
  parseSAX,
  parseSAXThrowing,
  parseSAXLocations,
  parseSAXLocationsThrowing,
  parseTree,
  parseTree',
  parseTreeThrowing
) where

import Control.Arrow
import Text.XML.Expat.Tree ( Attributes, UAttributes )
import qualified Text.XML.Expat.Tree as Tree
import Text.XML.Expat.SAX ( Encoding(..)
                          , GenericXMLString(..)
                          , ParserOptions(..)
                          , SAXEvent(..)
                          , XMLParseError(..)
                          , XMLParseException(..)
                          , XMLParseLocation(..)
                          , parseSAX
                          , parseSAXThrowing
                          , parseSAXLocations
                          , parseSAXLocationsThrowing )
import qualified Text.XML.Expat.SAX as SAX
import Text.XML.Expat.Qualified hiding (QNode, QNodes)
import Text.XML.Expat.Namespaced hiding (NNode, NNodes)
import Text.XML.Expat.NodeClass

import Control.Monad (mplus)
import Control.Monad.Identity
import Control.Parallel.Strategies
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Monoid


-- | Annotated variant of the tree representation of the XML document.
data NodeG a c tag text =
    Element {
        eName       :: !tag,
        eAttributes :: ![(tag,text)],
        eChildren   :: c (NodeG a c tag text),
        eAnn        :: a
    } |
    Text !text

-- | A pure Node that uses a list as its container type.
type Node a = NodeG a []

instance (Show tag, Show text, Show a) => Show (NodeG a [] tag text) where
    show (Element na at ch an) = "Element "++show na++" "++show at++" "++show ch++" "++show an
    show (Text t) = "Text "++show t

instance (Eq tag, Eq text, Eq a) => Eq (NodeG a [] tag text) where
    Element na1 at1 ch1 an1 == Element na2 at2 ch2 an2 =
        na1 == na2 &&
        at1 == at2 &&
        ch1 == ch2 &&
        an1 == an2
    Text t1 == Text t2 = t1 == t2
    _ == _ = False

eAttrs :: Node a tag text -> [(tag, text)]
{-# DEPRECATED eAttrs "use eAttributes instead" #-}
eAttrs = eAttributes

instance (NFData tag, NFData text, NFData a) => NFData (NodeG a [] tag text) where
    rnf (Element nam att chi ann) = rnf (nam, att, chi, ann)
    rnf (Text txt) = rnf txt

instance NodeClass (NodeG a) [] where
    type NodeMonad (NodeG a) [] = Identity

    textContent (Element _ _ children _) = mconcat $ map textContent children
    textContent (Text txt) = txt
    
    isElement (Element _ _ _ _) = True
    isElement _                 = False
    
    isText (Text _) = True
    isText _        = False
    
    isNamed _  (Text _) = False
    isNamed nm (Element nm' _ _ _) = nm == nm'

    getName (Text _)             = gxFromString ""
    getName (Element name _ _ _) = name

    getAttributes (Text _)              = []
    getAttributes (Element _ attrs _ _) = attrs

    getChildren (Text _)           = []
    getChildren (Element _ _ ch _) = ch

    modifyName _ node@(Text _) = node
    modifyName f (Element n a c ann) = Element (f n) a c ann

    modifyAttributes _ node@(Text _) = node
    modifyAttributes f (Element n a c ann) = Element n (f a) c ann

    modifyChildren _ node@(Text _) = node
    modifyChildren f (Element n a c ann) = Element n a (f c) ann

    mapAllTags _ (Text t) = Text t
    mapAllTags f (Element n a c ann) = Element (f n) (map (first f) a) (map (mapAllTags f) c) ann

    mapElement _ (Text t) = Text t
    mapElement f (Element n a c ann) =
        let (n', a', c') = f (n, a, c)
        in  Element n' a' c' ann

    mapNodeContainer f (Element n a ch an) = do
        ch' <- f ch
        return $ Element n a ch' an
    mapNodeContainer _ (Text t) = return $ Text t

-- | Convert an annotated tree (/Annotated/ module) into a non-annotated
-- tree (/Tree/ module).  Needed, for example, when you @format@ your tree to
-- XML, since @format@ takes a non-annotated tree.
unannotate :: Functor c => NodeG a c tag text -> Tree.NodeG c tag text
unannotate (Element na at ch _) = (Tree.Element na at (fmap unannotate ch))
unannotate (Text t) = Tree.Text t

-- | Type shortcut for a single annotated node with unqualified tag names where
-- tag and text are the same string type
type UNode a text = Node a text text

-- | Type shortcut for a single annotated node, annotated with parse location
type LNode tag text = Node XMLParseLocation tag text

-- | Type shortcut for a single node with unqualified tag names where
-- tag and text are the same string type, annotated with parse location
type ULNode text = LNode text text 

-- | Type shortcut for a single annotated node where qualified names are used for tags
type QNode a text = Node a (QName text) text

-- | Type shortcut for a single node where qualified names are used for tags, annotated with parse location
type QLNode text = LNode (QName text) text

-- | Type shortcut for a single annotated node where namespaced names are used for tags
type NNode text a = Node a (NName text) text

-- | Type shortcut for a single node where namespaced names are used for tags, annotated with parse location
type NLNode text = LNode (NName text) text

-- | Modify this node's annotation (non-recursively) if it's an element, otherwise no-op.
modifyAnnotation :: (a -> a) -> Node a tag text -> Node a tag text
f `modifyAnnotation` Element na at ch an = Element na at ch (f an)
_ `modifyAnnotation` Text t = Text t

-- | Modify this node's annotation and all its children recursively if it's an element, otherwise no-op.
mapAnnotation :: (a -> b) -> Node a tag text -> Node b tag text
f `mapAnnotation` Element na at ch an = Element na at (map (f `mapAnnotation`) ch) (f an)
_ `mapAnnotation` Text t = Text t

-- | A lower level function that lazily converts a SAX stream into a tree structure.
-- Variant that takes annotations for start tags.
saxToTree :: GenericXMLString tag =>
             [(SAXEvent tag text, a)]
          -> (Node a tag text, Maybe XMLParseError)
saxToTree events =
    let (nodes, mError, _) = ptl events
    in  (safeHead nodes, mError)
  where
    safeHead (a:_) = a
    safeHead [] = Element (gxFromString "") [] [] (error "saxToTree null annotation")
    ptl ((StartElement name attrs, ann):rema) =
        let (children, err1, rema') = ptl rema
            elt = Element name attrs children ann
            (out, err2, rema'') = ptl rema'
        in  (elt:out, err1 `mplus` err2, rema'')
    ptl ((EndElement _, _):rema) = ([], Nothing, rema)
    ptl ((CharacterData txt, _):rema) =
        let (out, err, rema') = ptl rema
        in  (Text txt:out, err, rema')
    ptl ((FailDocument err, _):_) = ([], Just err, [])
    ptl [] = ([], Nothing, [])

-- | Lazily parse XML to tree. Note that forcing the XMLParseError return value
-- will force the entire parse.  Therefore, to ensure lazy operation, don't
-- check the error status until you have processed the tree.
parse :: (GenericXMLString tag, GenericXMLString text) =>
         ParserOptions tag text   -- ^ Optional encoding override
      -> L.ByteString             -- ^ Input text (a lazy ByteString)
      -> (LNode tag text, Maybe XMLParseError)
parse opts bs = saxToTree $ SAX.parseLocations opts bs

-- | DEPRECATED: Use 'parse' instead.
--
-- Lazily parse XML to tree. Note that forcing the XMLParseError return value
-- will force the entire parse.  Therefore, to ensure lazy operation, don't
-- check the error status until you have processed the tree.
parseTree :: (GenericXMLString tag, GenericXMLString text) =>
             Maybe Encoding      -- ^ Optional encoding override
          -> L.ByteString        -- ^ Input text (a lazy ByteString)
          -> (LNode tag text, Maybe XMLParseError)
{-# DEPRECATED parseTree "use Text.XML.Annotated.parse instead" #-}
parseTree mEnc = parse (ParserOptions mEnc Nothing)

-- | Lazily parse XML to tree. In the event of an error, throw 'XMLParseException'.
--
-- @parseThrowing@ can throw an exception from pure code, which is generally a bad
-- way to handle errors, because Haskell\'s lazy evaluation means it\'s hard to
-- predict where it will be thrown from.  However, it may be acceptable in
-- situations where it's not expected during normal operation, depending on the
-- design of your program.
parseThrowing :: (GenericXMLString tag, GenericXMLString text) =>
                 ParserOptions tag text   -- ^ Optional encoding override
              -> L.ByteString             -- ^ Input text (a lazy ByteString)
              -> LNode tag text
parseThrowing opts bs = fst $ saxToTree $ SAX.parseLocationsThrowing opts bs

-- | DEPRECATED: use 'parseThrowing' instead
--
-- Lazily parse XML to tree. In the event of an error, throw 'XMLParseException'.
parseTreeThrowing :: (GenericXMLString tag, GenericXMLString text) =>
             Maybe Encoding      -- ^ Optional encoding override
          -> L.ByteString        -- ^ Input text (a lazy ByteString)
          -> LNode tag text
{-# DEPRECATED parseTreeThrowing "use Text.XML.Annotated.parseThrowing instead" #-}
parseTreeThrowing mEnc = parseThrowing (ParserOptions mEnc Nothing)

-- | Strictly parse XML to tree. Returns error message or valid parsed tree.
parse' :: (GenericXMLString tag, GenericXMLString text) =>
          ParserOptions tag text  -- ^ Optional encoding override
       -> B.ByteString            -- ^ Input text (a strict ByteString)
       -> Either XMLParseError (LNode tag text)
parse' opts bs = case parse opts (L.fromChunks [bs]) of
    (_, Just err)   -> Left err
    (root, Nothing) -> Right root 

-- | DEPRECATED: use 'parse' instead.
--
-- Strictly parse XML to tree. Returns error message or valid parsed tree.
parseTree' :: (GenericXMLString tag, GenericXMLString text) =>
              Maybe Encoding      -- ^ Optional encoding override
           -> B.ByteString        -- ^ Input text (a strict ByteString)
           -> Either XMLParseError (LNode tag text)
{-# DEPRECATED parseTree' "use Text.XML.Expat.parse' instead" #-}
parseTree' mEnc = parse' (ParserOptions mEnc Nothing)

