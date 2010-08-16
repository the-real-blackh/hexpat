{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies,
        FlexibleContexts #-}
-- | An extended variant of /Node/ intended to implement the entire XML
-- specification.  DTDs are not yet supported, however.
--
-- The names conflict with those in /Tree/ so you must use qualified import
-- if you want to use both modules.
module Text.XML.Expat.Extended (
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
  ParseOptions(..),
  defaultParseOptions,
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
  GenericXMLString(..)
  ) where

import Control.Arrow
import Text.XML.Expat.SAX ( Encoding(..)
                          , GenericXMLString(..)
                          , ParseOptions(..)
                          , defaultParseOptions
                          , SAXEvent(..)
                          , XMLParseError(..)
                          , XMLParseException(..)
                          , XMLParseLocation(..) )
import qualified Text.XML.Expat.SAX as SAX
import Text.XML.Expat.Internal.Namespaced
import Text.XML.Expat.Internal.NodeClass
import Text.XML.Expat.Internal.Qualified

import Control.Monad (mplus, mzero)
import Control.DeepSeq
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.List.Class
import Data.Monoid


-- | Extended variant of the tree representation of the XML document, intended
-- to support the entire XML specification.
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
    Text !text |
    CData !text |
    Comment !text |
    ProcessingInstruction !text !text

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
    showsPrec d (Element na at ch an) = showParen (d > 10) $
        ("Element "++) . showsPrec 11 na . (" "++) .
                         showsPrec 11 at . (" "++) .
                         showsPrec 11 ch . (" "++) .
                         showsPrec 11 an
    showsPrec d (Text t) = showParen (d > 10) $ ("Text "++) . showsPrec 11 t
    showsPrec d (CData t) = showParen (d > 10) $ ("CData "++) . showsPrec 11 t
    showsPrec d (ProcessingInstruction t txt) = showParen (d > 10) $
        ("ProcessingInstruction "++) . showsPrec 11 t . (" "++) . showsPrec 11 txt
    showsPrec d (Comment t) = showParen (d > 10) $ ("Comment "++) . showsPrec 11 t

instance (Eq tag, Eq text, Eq a) => Eq (NodeG a [] tag text) where
    Element na1 at1 ch1 an1 == Element na2 at2 ch2 an2 =
        na1 == na2 &&
        at1 == at2 &&
        ch1 == ch2 &&
        an1 == an2
    Text t1 == Text t2 = t1 == t2
    CData t1 == CData t2 = t1 == t2
    ProcessingInstruction t1 d1 == ProcessingInstruction t2 d2 = 
        t1 == t2 &&
        d1 == d2
    Comment t1 == Comment t2 = t1 == t2
    _ == _ = False

instance (NFData tag, NFData text, NFData a) => NFData (NodeG a [] tag text) where
    rnf (Element nam att chi ann) = rnf (nam, att, chi, ann)
    rnf (Text txt) = rnf txt
    rnf (CData txt) = rnf txt
    rnf (ProcessingInstruction target txt) = rnf (target, txt)
    rnf (Comment txt) = rnf txt

instance (Functor c, List c) => NodeClass (NodeG a) c where
    textContentM (Element _ _ children _) = foldlL mappend mempty $ joinM $ fmap textContentM children
    textContentM (Text txt) = return txt
    textContentM (CData txt) = return txt
    textContentM (ProcessingInstruction _ txt) = return txt
    textContentM (Comment txt) = return txt
    
    isElement (Element _ _ _ _) = True
    isElement _                 = False
    
    isText (Text _) = True
    isText (CData _) = True
    isText _        = False
    
    isCData (CData _) = True
    isCData _        = False
    
    isProcessingInstruction (ProcessingInstruction _ _) = True
    isProcessingInstruction _        = False
    
    isComment (Comment _) = True
    isComment _        = False
    
    isNamed nm (Element nm' _ _ _) = nm == nm'
    isNamed _  _ = False
    
    getName (Element name _ _ _) = name
    getName _             = mempty
    
    hasTarget t (ProcessingInstruction t' _ ) = t == t'
    hasTarget _  _ = False
    
    getTarget (ProcessingInstruction target _) = target
    getTarget _             = mempty

    getAttributes (Element _ attrs _ _) = attrs
    getAttributes _              = []

    getChildren (Element _ _ ch _) = ch
    getChildren _           = mzero

    getText (Text txt) = txt
    getText (CData txt) = txt
    getText (ProcessingInstruction _ txt) = txt
    getText (Comment txt) = txt
    getText (Element _ _ _ _) = mempty

    modifyName f (Element n a c ann) = Element (f n) a c ann
    modifyName _ node = node

    modifyAttributes f (Element n a c ann) = Element n (f a) c ann
    modifyAttributes _ node = node

    modifyChildren f (Element n a c ann) = Element n a (f c) ann
    modifyChildren _ node = node

    mapAllTags f (Element n a c ann) = Element (f n) (map (first f) a) (fmap (mapAllTags f) c) ann
    mapAllTags _ (Text txt) = Text txt
    mapAllTags _ (CData txt) = CData txt
    mapAllTags _ (ProcessingInstruction n txt) = ProcessingInstruction n txt
    mapAllTags _ (Comment txt) = Comment txt

    modifyElement f (Element n a c ann) =
        let (n', a', c') = f (n, a, c)
        in  Element n' a' c' ann
    modifyElement _ (Text txt) = Text txt
    modifyElement _ (CData txt) = CData txt
    modifyElement _ (ProcessingInstruction n txt) = ProcessingInstruction n txt
    modifyElement _ (Comment txt) = Comment txt

    mapNodeContainer f (Element n a ch an) = do
        ch' <- mapNodeListContainer f ch
        return $ Element n a ch' an
    mapNodeContainer _ (Text txt) = return $ (Text txt)
    mapNodeContainer _ (CData txt) = return $ (CData txt)
    mapNodeContainer _ (ProcessingInstruction n txt) = return $ (ProcessingInstruction n txt)
    mapNodeContainer _ (Comment txt) = return $ (Comment txt)

    mkText = Text

instance (Functor c, List c) => MkElementClass (NodeG (Maybe a)) c where
    mkElement name attrs children = Element name attrs children Nothing

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
_ `modifyAnnotation` CData t = CData t
_ `modifyAnnotation` ProcessingInstruction n t = ProcessingInstruction n t
_ `modifyAnnotation` Comment t = Comment t

-- | Modify this node's annotation and all its children recursively if it's an element, otherwise no-op.
mapAnnotation :: (a -> b) -> Node a tag text -> Node b tag text
f `mapAnnotation` Element na at ch an = Element na at (map (f `mapAnnotation`) ch) (f an)
_ `mapAnnotation` Text t = Text t
_ `mapAnnotation` CData t = CData t
_ `mapAnnotation` ProcessingInstruction n t = ProcessingInstruction n t
_ `mapAnnotation` Comment t = Comment t

-- | A lower level function that lazily converts a SAX stream into a tree structure.
-- Variant that takes annotations for start tags.
saxToTree :: (GenericXMLString tag, Monoid text) =>
             [(SAXEvent tag text, a)]
          -> (Node a tag text, Maybe XMLParseError)
saxToTree events =
    let (nodes, mError, _) = ptl events False []
    in  (safeHead nodes, mError)
  where
    safeHead (a:_) = a
    safeHead [] = Element (gxFromString "") [] [] (error "saxToTree null annotation")
    ptl ((StartElement name attrs,ann):rema) isCD cd =
        let (children, err1, rema') = ptl rema isCD cd
            elt = Element name attrs children ann
            (out, err2, rema'') = ptl rema' isCD cd
        in  (elt:out, err1 `mplus` err2, rema'')
    ptl ((EndElement _, _):rema) _ _ = ([], Nothing, rema)
    ptl ((CharacterData txt, _):rema) isCD cd =
        if isCD then
            ptl rema isCD (cd ++ [txt])
        else
            let (out, err, rema') = ptl rema isCD cd
            in  (Text txt:out, err, rema')
    ptl ((StartCData,_) :rema) _ _ =
        ptl rema True mzero
    ptl ((EndCData, _) :rema) _ cd =
        let (out, err, rema') = ptl rema False mzero
        in  (CData (mconcat cd):out, err, rema')
    ptl ((EndComment txt, _):rema) isCD cd =
        let (out, err, rema') = ptl rema isCD cd
        in  (Comment txt:out, err, rema')
    ptl ((EndProcessingInstruction target txt, _):rema) isCD cd =
        let (out, err, rema') = ptl rema isCD cd
        in  (ProcessingInstruction target txt:out, err, rema')
    ptl ((FailDocument err, _):_) _ _ = ([], Just err, [])
    ptl [] _ _ = ([], Nothing, [])

-- | Lazily parse XML to tree. Note that forcing the XMLParseError return value
-- will force the entire parse.  Therefore, to ensure lazy operation, don't
-- check the error status until you have processed the tree.
parse :: (GenericXMLString tag, GenericXMLString text) =>
         ParseOptions tag text   -- ^ Optional encoding override
      -> L.ByteString             -- ^ Input text (a lazy ByteString)
      -> (LNode tag text, Maybe XMLParseError)
parse opts bs = saxToTree $ SAX.parseLocations opts bs

-- | Lazily parse XML to tree. In the event of an error, throw 'XMLParseException'.
--
-- @parseThrowing@ can throw an exception from pure code, which is generally a bad
-- way to handle errors, because Haskell\'s lazy evaluation means it\'s hard to
-- predict where it will be thrown from.  However, it may be acceptable in
-- situations where it's not expected during normal operation, depending on the
-- design of your program.
parseThrowing :: (GenericXMLString tag, GenericXMLString text) =>
                 ParseOptions tag text   -- ^ Optional encoding override
              -> L.ByteString             -- ^ Input text (a lazy ByteString)
              -> LNode tag text
parseThrowing opts bs = fst $ saxToTree $ SAX.parseLocationsThrowing opts bs

-- | Strictly parse XML to tree. Returns error message or valid parsed tree.
parse' :: (GenericXMLString tag, GenericXMLString text) =>
          ParseOptions tag text  -- ^ Optional encoding override
       -> B.ByteString            -- ^ Input text (a strict ByteString)
       -> Either XMLParseError (LNode tag text)
parse' opts bs = case parse opts (L.fromChunks [bs]) of
    (_, Just err)   -> Left err
    (root, Nothing) -> Right root 

