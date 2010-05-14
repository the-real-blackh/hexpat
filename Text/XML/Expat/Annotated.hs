{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies,
        FlexibleContexts #-}
-- | A variant of /Node/ in which Element nodes have an annotation of any type,
-- and some concrete functions that annotate with the XML parse location.
--
-- The names conflict with those in /Tree/ so you must use qualified import
-- if you want to use both modules.
module Text.XML.Expat.Annotated (
  -- * Tree structure
  Node,
  NodeG(..),
  UNode,
  LNode,
  ULNode,

  -- * Generic node manipulation
  module Text.XML.Expat.Internal.NodeClass,

  -- * Annotation-specific
  modifyAnnotation,
  mapAnnotation,

  -- * Qualified nodes
  QNode,
  QLNode,
  module Text.XML.Expat.Internal.Qualified,

  -- * Namespaced nodes
  NNode,
  NLNode,
  module Text.XML.Expat.Internal.Namespaced,

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
  parseSAX,
  parseSAXThrowing,
  parseSAXLocations,
  parseSAXLocationsThrowing,
  parseTree,
  parseTree',
  parseTreeThrowing,
  unannotate
  ) where

import Control.Arrow
import qualified Text.XML.Expat.Tree as Tree
import Text.XML.Expat.SAX ( Encoding(..)
                          , GenericXMLString(..)
                          , ParserOptions(..)
                          , defaultParserOptions
                          , SAXEvent(..)
                          , XMLParseError(..)
                          , XMLParseException(..)
                          , XMLParseLocation(..)
                          , parseSAX
                          , parseSAXThrowing
                          , parseSAXLocations
                          , parseSAXLocationsThrowing )
import qualified Text.XML.Expat.SAX as SAX
import Text.XML.Expat.Internal.Namespaced
import Text.XML.Expat.Internal.NodeClass
import Text.XML.Expat.Internal.Qualified

import Control.Monad (mplus, mzero)
import Control.Parallel.Strategies
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.List.Class
import Data.Monoid


-- | Annotated variant of the tree representation of the XML document, meaning
-- that it has an extra piece of information of your choice attached to each
-- Element.
--
-- @c@ is the container type for the element's children, which is [] in the
-- @hexpat@ package, and a monadic list type for @hexpat-iteratee@.
--
-- @tag@ is the tag type, which can either be one of several string types,
-- or a special type from the @Text.XML.Expat.Namespaced@ or
-- @Text.XML.Expat.Qualified@ modules.
--
-- @text@ is the string type for text content.
--
-- @a@ is the type of the annotation.  One of the things this can be used for
-- is to store the XML parse location, which is useful for error handling.
--
-- Note that some functions in the @Text.XML.Expat.Cursor@ module need to create
-- new nodes through the 'MkElementClass' type class. Normally this can only be done
-- if @a@ is a Maybe type (so it can provide the Nothing value for the annotation
-- on newly created nodes).  Or, you can write your own 'MkElementClass' instance.
-- Apart from that, there is no requirement for @a@ to be a Maybe type.
data NodeG a c tag text =
    Element {
        eName       :: !tag,
        eAttributes :: ![(tag,text)],
        eChildren   :: c (NodeG a c tag text),
        eAnn        :: a
    } |
    Text !text

type instance ListOf (NodeG a c tag text) = c (NodeG a c tag text)

-- | A pure tree representation that uses a list as its container type,
-- annotated variant.
--
-- In the @hexpat@ package, a list of nodes has the type @[Node tag text]@, but note
-- that you can also use the more general type function 'ListOf' to give a list of
-- any node type, using that node's associated list type, e.g.
-- @ListOf (UNode Text)@.
type Node a tag text = NodeG a [] tag text

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

instance (Functor c, List c) => NodeClass (NodeG a) c where
    textContentM (Element _ _ children _) = foldlL mappend mempty $ joinM $ fmap textContentM children
    textContentM (Text txt) = return txt
    
    isElement (Element _ _ _ _) = True
    isElement _                 = False
    
    isText (Text _) = True
    isText _        = False
    
    isNamed _  (Text _) = False
    isNamed nm (Element nm' _ _ _) = nm == nm'

    getName (Text _)             = mempty
    getName (Element name _ _ _) = name

    getAttributes (Text _)              = []
    getAttributes (Element _ attrs _ _) = attrs

    getChildren (Text _)           = mzero
    getChildren (Element _ _ ch _) = ch

    getText (Text txt) = txt
    getText (Element _ _ _ _) = mempty

    modifyName _ node@(Text _) = node
    modifyName f (Element n a c ann) = Element (f n) a c ann

    modifyAttributes _ node@(Text _) = node
    modifyAttributes f (Element n a c ann) = Element n (f a) c ann

    modifyChildren _ node@(Text _) = node
    modifyChildren f (Element n a c ann) = Element n a (f c) ann

    mapAllTags _ (Text t) = Text t
    mapAllTags f (Element n a c ann) = Element (f n) (map (first f) a) (fmap (mapAllTags f) c) ann

    mapElement _ (Text t) = Text t
    mapElement f (Element n a c ann) =
        let (n', a', c') = f (n, a, c)
        in  Element n' a' c' ann

    mapNodeContainer f (Element n a ch an) = do
        ch' <- f ch
        return $ Element n a ch' an
    mapNodeContainer _ (Text t) = return $ Text t

    mkText = Text

instance (Functor c, List c) => MkElementClass (NodeG (Maybe a)) c where
    mkElement name attrs children = Element name attrs children Nothing

-- | Convert an annotated tree (/Annotated/ module) into a non-annotated
-- tree (/Tree/ module).  DEPRECATED in favour of 'fromElement'.
unannotate :: Functor c => NodeG a c tag text -> Tree.NodeG c tag text
{-# DEPRECATED unannotate "use fromElement instead" #-}
unannotate (Element na at ch _) = (Tree.Element na at (fmap unannotate ch))
unannotate (Text t) = Tree.Text t

-- | Type alias for a single annotated node with unqualified tag names where
-- tag and text are the same string type
type UNode a text = Node a text text

-- | Type alias for a single annotated node, annotated with parse location
type LNode tag text = Node XMLParseLocation tag text

-- | Type alias for a single node with unqualified tag names where
-- tag and text are the same string type, annotated with parse location
type ULNode text = LNode text text 

-- | Type alias for a single annotated node where qualified names are used for tags
type QNode a text = Node a (QName text) text

-- | Type alias for a single node where qualified names are used for tags, annotated with parse location
type QLNode text = LNode (QName text) text

-- | Type alias for a single annotated node where namespaced names are used for tags
type NNode a text = Node a (NName text) text

-- | Type alias for a single node where namespaced names are used for tags, annotated with parse location
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

