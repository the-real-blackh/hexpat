{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
-- | Type classes to allow for XML handling functions to be generalized to
-- work with different document types.
module Text.XML.Expat.Internal.DocumentClass where

import Text.XML.Expat.Internal.NodeClass (NodeClass)
import Control.DeepSeq
import Data.List.Class


-- | XML declaration, consisting of version, encoding and standalone.
data XMLDeclaration text = XMLDeclaration text (Maybe text) (Maybe Bool) deriving (Eq, Show)

-- | Stub for future expansion.
data DocumentTypeDeclaration c tag text = DocumentTypeDeclaration deriving (Eq, Show)

data Misc text =
    Comment !text |
    ProcessingInstruction !text !text

instance Show text => Show (Misc text) where
    showsPrec d (ProcessingInstruction t txt) = showParen (d > 10) $
        ("ProcessingInstruction "++) . showsPrec 11 t . (" "++) . showsPrec 11 txt
    showsPrec d (Comment t) = showParen (d > 10) $ ("Comment "++) . showsPrec 11 t

instance Eq text => Eq (Misc text) where
    ProcessingInstruction t1 d1 == ProcessingInstruction t2 d2 = 
        t1 == t2 &&
        d1 == d2
    Comment t1 == Comment t2 = t1 == t2
    _ == _ = False

instance NFData text => NFData (Misc text) where
    rnf (ProcessingInstruction target txt) = rnf (target, txt)
    rnf (Comment txt) = rnf txt

type family NodeType d :: (* -> *) -> * -> * -> *

class (Functor c, List c, NodeClass (NodeType d) c) => DocumentClass d c where
    -- | Get the XML declaration for this document.
    getXMLDeclaration :: d c tag text -> Maybe (XMLDeclaration text)

    -- | Get the Document Type Declaration (DTD) for this document.
    getDocumentTypeDeclaration :: d c tag text -> Maybe (DocumentTypeDeclaration c tag text)

    -- | Get the top-level 'Misc' nodes for this document.
    getTopLevelMiscs :: d c tag text -> c (Misc text)

    -- | Get the root element for this document.
    getRoot :: d c tag text -> NodeType d c tag text

