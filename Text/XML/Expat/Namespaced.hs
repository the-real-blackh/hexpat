module Text.XML.Expat.Namespaced
      ( NName (..)
      , NNode
      , NNodes
      , NAttributes
      , mkNName
      , mkAnNName
      , withNamespaces
      ) where

import Text.XML.Expat.Tree
import Text.XML.Expat.Qualified
import Control.Parallel.Strategies
import qualified Data.Map as M
import qualified Data.List as L

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

type NsMap text = M.Map (Maybe text) text

xmlnsUri :: (GenericXMLString text) => text
xmlnsUri = gxFromString "http://www.w3.org/2000/xmlns/"
xmlns :: (GenericXMLString text) => text
xmlns = gxFromString "xmlns"

withNamespaces :: (GenericXMLString text, Ord text) => QNode text -> NNode text
withNamespaces = nodeWithNamespaces $ M.fromList [(Just xmlns, xmlnsUri)]

nodeWithNamespaces :: (GenericXMLString text, Ord text) => NsMap text -> QNode text -> NNode text
nodeWithNamespaces _ (Text t) = Text t
nodeWithNamespaces bindings (Element qname qattrs qchildren) = Element nname nattrs nchildren
  where
    for = flip map
    (nsAtts, otherAtts) = L.partition ((== Just xmlns) . qnPrefix . fst) qattrs
    (dfAtt, normalAtts) = L.partition ((== xmlns) . qnLocalPart . fst) qattrs
    nsMap  = M.fromList $ for nsAtts $ \((QName _ lp), uri) -> (Just lp, uri)
    dfMap  = M.fromList $ for dfAtt $ \q -> (Nothing, snd q)
    chldBs = M.unions [dfMap, nsMap, bindings]
    trans (QName pref qual) = NName (pref `M.lookup` chldBs)
                                    qual
    transAt (qn, v) = (trans qn, v) 

    nname       = trans qname

    nNsAtts     = map transAt nsAtts
    nDfAtt      = map transAt dfAtt
    nNormalAtts = map transAt normalAtts
    nattrs      = concat [nNsAtts, nDfAtt, nNormalAtts]

    nchildren   = for qchildren $ nodeWithNamespaces chldBs
