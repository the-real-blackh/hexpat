-- | A variant of /Node/ in which Element nodes have an annotation of any type,
-- and some concrete functions that annotate with the XML parse location.
-- It is assumed you will usually want /Tree/ or /Annotated/, not both, so many
-- of the names conflict.
--
-- Support for qualified and namespaced trees annotated with location information
-- is not complete.
module Text.XML.Expat.Annotated (
        -- * Tree structure
        Node(..),
        Attributes,  -- re-export from Tree
        Nodes,
        UNode,
        UNodes,
        UAttributes,
        LNode,
        LNodes,
        ULNode,
        ULNodes,
        textContent,
        unannotate,
        -- * Qualified nodes
        QName(..),
        QNode,
        QNodes,
        QAttributes,
        QLNode,
        QLNodes,
        -- * Namespaced nodes
        NName (..),
        NNode,
        NNodes,
        NAttributes,
        NLNode,
        NLNodes,
        mkNName,
        mkAnNName,
        xmlnsUri,
        xmlns,
        -- * Deprecated parse functions
        parseSAX,
        parseSAXThrowing,
        parseSAXLocations,
        parseSAXLocationsThrowing,
        parseTree,
        parseTree',
        parseTreeThrowing,

        -- * Parse to tree
        Encoding(..),
        XMLParseError(..),
        XMLParseLocation(..),
        parse,
        parse',
        parseThrowing,

        -- * SAX-style parse
        SAXEvent(..),
        saxToTree,

        -- * Variants that throw exceptions
        XMLParseException(..),
        -- * Abstraction of string types
        GenericXMLString(..)
    ) where

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

import Control.Monad (mplus)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Monoid


-- | Annotated variant of the tree representation of the XML document.
data Node tag text a =
    Element {
        eName     :: !tag,
        eAttrs    :: ![(tag,text)],
        eChildren :: [Node tag text a],
        eAnn      :: a
    } |
    Text !text
    deriving (Eq, Show)

unannotate :: Node tag text a -> Tree.Node tag text
unannotate (Element na at ch _) = (Tree.Element na at (map unannotate ch))
unannotate (Text t) = Tree.Text t

-- | Extract all text content from inside a tag into a single string, including
-- any text contained in children.
textContent :: Monoid text => Node tag text a -> text
textContent (Element _ _ children _) = mconcat $ map textContent children
textContent (Text txt) = txt

-- | Type shortcut for annotated nodes
type Nodes tag text a = [Node tag text a]

-- | Type shortcut for annotated nodes with unqualified tag names where tag and
-- text are the same string type
type UNodes text a = Nodes text text a

-- | Type shortcut for a single annotated node with unqualified tag names where
-- tag and text are the same string type
type UNode text a = Node text text a

-- | Type shortcut for a single annotated node, annotated with parse location
type LNode tag text = Node tag text XMLParseLocation

-- | Type shortcut for annotated nodes with location information.
type LNodes tag text = [Node tag text XMLParseLocation]

-- | Type shortcut for a single node with unqualified tag names where
-- tag and text are the same string type, annotated with parse location
type ULNode text = LNode text text 

-- | Type shortcut for nodes with unqualified tag names where
-- tag and text are the same string type, annotated with parse location
type ULNodes text = LNodes text text

-- | Type shortcut for annotated nodes where qualified names are used for tags
type QNodes text a = Nodes (QName text) text a

-- | Type shortcut for nodes where qualified names are used for tags, annotated with parse location
type QLNodes text = LNodes (QName text) text

-- | Type shortcut for a single annotated node where qualified names are used for tags
type QNode text a = Node (QName text) text a

-- | Type shortcut for a single node where qualified names are used for tags, annotated with parse location
type QLNode text = LNode (QName text) text

-- | Type shortcut for annotated nodes where namespaced names are used for tags
type NNodes text a = Nodes (NName text) text a

-- | Type shortcut for nodes where namespaced names are used for tags, annotated with parse location
type NLNodes text = LNodes (NName text) text

-- | Type shortcut for a single annotated node where namespaced names are used for tags
type NNode text a = Node (NName text) text a

-- | Type shortcut for a single node where namespaced names are used for tags, annotated with parse location
type NLNode text = LNode (NName text) text

instance Functor (Node tag text) where
    f `fmap` Element na at ch an = Element na at (map (f `fmap`) ch) (f an)
    f `fmap` Text t = Text t

-- | A lower level function that lazily converts a SAX stream into a tree structure.
-- Variant that takes annotations for start tags.
saxToTree :: GenericXMLString tag =>
             [(SAXEvent tag text, a)]
          -> (Node tag text a, Maybe XMLParseError)
saxToTree events =
    let (nodes, mError, _) = ptl events
    in  (safeHead nodes, mError)
  where
    safeHead (a:_) = a
    safeHead [] = Element (gxFromString "") [] [] (error "saxToTree null annotation")
    ptl ((StartElement name attrs, ann):rem) =
        let (children, err1, rem') = ptl rem
            elt = Element name attrs children ann
            (out, err2, rem'') = ptl rem'
        in  (elt:out, err1 `mplus` err2, rem'')
    ptl ((EndElement name, _):rem) = ([], Nothing, rem)
    ptl ((CharacterData txt, _):rem) =
        let (out, err, rem') = ptl rem
        in  (Text txt:out, err, rem')
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

-- | Lazily parse XML to tree. Note that forcing the XMLParseError return value
-- will force the entire parse.  Therefore, to ensure lazy operation, don't
-- check the error status until you have processed the tree.
parseTree :: (GenericXMLString tag, GenericXMLString text) =>
             Maybe Encoding      -- ^ Optional encoding override
          -> L.ByteString        -- ^ Input text (a lazy ByteString)
          -> (LNode tag text, Maybe XMLParseError)
{-# DEPRECATED parseTree "use Text.XML.Expat.parse instead" #-}
parseTree mEnc = parse (ParserOptions mEnc Nothing)

-- | Lazily parse XML to tree. In the event of an error, throw 'XMLParseException'.
parseThrowing :: (GenericXMLString tag, GenericXMLString text) =>
                 ParserOptions tag text   -- ^ Optional encoding override
              -> L.ByteString             -- ^ Input text (a lazy ByteString)
              -> LNode tag text
parseThrowing opts bs = fst $ saxToTree $ SAX.parseLocationsThrowing opts bs

-- | Lazily parse XML to tree. In the event of an error, throw 'XMLParseException'.
parseTreeThrowing :: (GenericXMLString tag, GenericXMLString text) =>
             Maybe Encoding      -- ^ Optional encoding override
          -> L.ByteString        -- ^ Input text (a lazy ByteString)
          -> LNode tag text
{-# DEPRECATED parseTreeThrowing "use Text.XML.Expat.parseThrowing instead" #-}
parseTreeThrowing mEnc = parseThrowing (ParserOptions mEnc Nothing)

-- | Strictly parse XML to tree. Returns error message or valid parsed tree.
parse' :: (GenericXMLString tag, GenericXMLString text) =>
          ParserOptions tag text  -- ^ Optional encoding override
       -> B.ByteString            -- ^ Input text (a strict ByteString)
       -> Either XMLParseError (LNode tag text)
parse' opts bs = case parse opts (L.fromChunks [bs]) of
    (_, Just err)   -> Left err
    (root, Nothing) -> Right root 

-- | Strictly parse XML to tree. Returns error message or valid parsed tree.
parseTree' :: (GenericXMLString tag, GenericXMLString text) =>
              Maybe Encoding      -- ^ Optional encoding override
           -> B.ByteString        -- ^ Input text (a strict ByteString)
           -> Either XMLParseError (LNode tag text)
{-# DEPRECATED parseTree' "use Text.XML.Expat.parse' instead" #-}
parseTree' mEnc = parse' (ParserOptions mEnc Nothing)

