module Text.XML.Expat.Namespaced
      ( NName (..)
      , NNode
      , NNodes
      , NAttributes
      , mkNName
      , mkAnNName
      ) where

import Text.XML.Expat.Tree
import Control.Parallel.Strategies

-- | A namespace-qualified tag.
--
-- NName has two components, a local part and an optional namespace. The local part is the
-- name of the tag. The namespace is the URI identifying collections of declared tags.
-- Tags with the same local part but from different namespaces are distinct. Unqualified tags
-- are those with no namespace. They are in the default namespace, and all uses of an
-- unqualified tag are equivalent.
data NName text =
    NName {
        nnNamespace :: Maybe text,
        nnLocalPart :: !text
    }
    deriving (Eq,Show)

instance NFData text => NFData (NName text) where
    rnf (NName ns loc) = rnf (ns, loc)

-- | Type shortcut for nodes where namespaced names are used for tags
type NNodes text = Nodes (NName text) text

-- | Type shortcut for a single node where namespaced names are used for tags
type NNode text = Node (NName text) text

-- | Type shortcut for attributes where namespaced names are used for tags
type NAttributes text = Attributes (NName text) text

-- | Make a new NName from a prefix and localPart.
mkNName :: text -> text -> NName text
mkNName prefix localPart = NName (Just prefix) localPart

-- | Make a new NName with no prefix.
mkAnNName :: text -> NName text
mkAnNName localPart = NName Nothing localPart
